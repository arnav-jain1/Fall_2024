/** @file libscheduler.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libscheduler.h"
#include "../libpriqueue/libpriqueue.h"

#define MAX_JOBS 50

/**
  Stores information making up a job to be scheduled including any statistics.

  You may need to define some global variables or a struct to store your job queue elements. 
*/
typedef struct _job_t
{
    int id;
    int arrived_time;
    int priority;
    int total_duration;
    int time_remaining;
    int started_time;

    int first_scheduled;
    int first_arrived;

} job_t;

typedef struct _dispatcher_t 
{
    int cores;
    job_t** jobs;
    scheme_t algorithm;
    priqueue_t* queue;
     


    int wait_time;
    int turnaround_time;
    int response_time;

    int finished_jobs;

} dispatcher_t;

dispatcher_t dispatcher;


int compare_fcfs(const void* a, const void* b) {
    job_t* j1 = (job_t*) a;
    job_t* j2 = (job_t*) b;

    if (j1->arrived_time > j2->arrived_time) {
        return 1;

    } else if (j1->arrived_time < j2->arrived_time) {
        return -1;
    }
    return 0;
}

int compare_sjf(const void* a, const void* b) {
    job_t* j1 = (job_t*) a;
    job_t* j2 = (job_t*) b;

    if (j1->time_remaining > j2->time_remaining) {
        return 1;
    } else if (j1->time_remaining < j2->time_remaining) {
        return -1;
    } else {
        return compare_fcfs(a, b);
    }
}

int compare_priority(const void* a, const void* b) {
    job_t* j1 = (job_t*) a;
    job_t* j2 = (job_t*) b;

    if (j1->priority > j2->priority) {
        return 1;
    } else if (j1->priority < j2->priority) {
        return -1;
    } else {
        return compare_fcfs(a, b);
    }
}
/**
  Initalizes the scheduler.
 
  Assumptions:
    - You may assume this will be the first scheduler function called.
    - You may assume this function will be called once once.
    - You may assume that cores is a positive, non-zero number.
    - You may assume that scheme is a valid scheduling scheme.

  @param cores the number of cores that is available by the scheduler. These cores will be known as core(id=0), core(id=1), ..., core(id=cores-1).
  @param scheme  the scheduling scheme that should be used. This value will be one of the six enum values of scheme_t
*/
void scheduler_start_up(int cores, scheme_t scheme)
{
    dispatcher.jobs = (job_t** ) malloc(sizeof(job_t*) * cores);
    dispatcher.cores = cores;
    dispatcher.algorithm = scheme;
    dispatcher.finished_jobs = 0;
    dispatcher.wait_time = 0;
    dispatcher.response_time = 0;
    dispatcher.turnaround_time = 0;

    for (int i = 0; i < cores; i++) {
        dispatcher.jobs[i] = NULL;
    }

    int (*comparer) (const void *, const void *);

    dispatcher.queue = (priqueue_t *) malloc(sizeof(priqueue_t));

    switch (scheme) {
        case FCFS:
            comparer = compare_fcfs;
            break;
        case RR:
            comparer = compare_fcfs;
            break;
        case SJF:
            comparer = compare_sjf;
            break;
        case PSJF:
            comparer = compare_sjf;
            break;
        case PRI:
            comparer = compare_priority;
            break;
        case PPRI:
            comparer = compare_priority;
            break;
    }

    priqueue_init(dispatcher.queue, comparer);

}


/**
  Called when a new job arrives.
 
  If multiple cores are idle, the job should be assigned to the core with the
  lowest id.
  If the job arriving should be scheduled to run during the next
  time cycle, return the zero-based index of the core the job should be
  scheduled on. If another job is already running on the core specified,
  this will preempt the currently running job.
  Assumption:
    - You may assume that every job wil have a unique arrival time.

  @param job_number a globally unique identification number of the job arriving.
  @param time the current time of the simulator.
  @param running_time the total number of time units this job will run before it will be finished.
  @param priority the priority of the job. (The lower the value, the higher the priority.)
  @return index of core job should be scheduled on
  @return -1 if no scheduling changes should be made. 
 
 */
int scheduler_new_job(int job_number, int time, int running_time, int priority)
{
    job_t* job = malloc(sizeof(job_t));
    job->id = job_number;
    job->arrived_time = time; 
    job->first_arrived = time; 
    job->time_remaining = running_time;
    job->total_duration = running_time;
    job->priority = priority;
    job->started_time = -1;
    job->first_scheduled = -1;


    job_t* preempted_job = NULL;

    if (dispatcher.algorithm == PSJF) {
        for (int i = 0; i < dispatcher.cores; i++) {
            if (dispatcher.jobs[i] == NULL) {
                break;
            }
            preempted_job = dispatcher.jobs[i];
            priqueue_remove(dispatcher.queue, preempted_job) ;
            preempted_job->time_remaining -= (time - preempted_job->started_time); 
            priqueue_offer(dispatcher.queue, preempted_job);
            preempted_job = NULL;
        }

    }


    int position = priqueue_offer(dispatcher.queue, job);

    for (int i = 0; i < dispatcher.cores; i++) {
        if (dispatcher.jobs[i] == NULL) {
            dispatcher.jobs[i] = job;
            job->started_time = time;
            job->first_scheduled = time;
            return i;
        }
    }


    // If it is a preemptive and it needs to preempted, then preempt
    if (position < dispatcher.cores && (dispatcher.algorithm == PPRI)) {
        /*printf("Position: %d\n", position);*/
        int lowest = 0;
        for (int i = 0; i < dispatcher.cores; i++) {
            if (dispatcher.jobs[lowest]->priority < dispatcher.jobs[i]->priority) {
                lowest = i;
            }
        }
        /*printf("Lowest: %d\n", lowest);*/
        preempted_job = dispatcher.jobs[lowest];
        dispatcher.jobs[lowest] = job;

        //// this is mainly for psjf if the remaining time == the new job then continue the current job
        /*int position_of_preempt;*/
        /*priqueue_remove(dispatcher.queue, preempted_job);*/
        /*preempted_job->time_remaining -= (time - preempted_job->started_time);*/
        /*position_of_preempt = priqueue_offer(dispatcher.queue, preempted_job);*/
        /*if (position_of_preempt <= position) {*/
            /*dispatcher.jobs[position] = preempted_job;*/
            /*return -1;*/
        /*}*/
        
        // if it doesnt stay, then the preempted job start time is set to -1 and the new job first_scheduled time is set to current time
        preempted_job->started_time = -1;
        job->first_scheduled = time;
        /*printf("Job %d preempted Job %d\n", preempted_job->id, job->id);*/

        return lowest;

    } else if (position < dispatcher.cores && dispatcher.algorithm == PSJF) {
        int lowest = 0;
        for (int i = 0; i < dispatcher.cores; i++) {
            if (dispatcher.jobs[lowest]->priority < dispatcher.jobs[i]->priority) {
                lowest = i;
            }
        }
        /*printf("Lowest: %d\n", lowest);*/
        preempted_job = dispatcher.jobs[lowest];
        dispatcher.jobs[lowest] = job;
        preempted_job->started_time = -1;
        job->first_scheduled = time;

        return lowest;

    }


	return -1;
}


/**
  Called when a job has completed execution.
 
  The core_id, job_number and time parameters are provided for convenience. You may be able to calculate the values with your own data structure.
  If any job should be scheduled to run on the core free'd up by the
  finished job, return the job_number of the job that should be scheduled to
  run on core core_id.
 
  @param core_id the zero-based index of the core where the job was located.
  @param job_number a globally unique identification number of the job.
  @param time the current time of the simulator.
  @return job_number of the job that should be scheduled to run on core core_id
  @return -1 if core should remain idle.
 */
int scheduler_job_finished(int core_id, int job_number, int time)
{
    job_t* next_job;
    job_t* finished_job = dispatcher.jobs[core_id];
    dispatcher.jobs[core_id] = NULL;
    /*printf("Finished: %d \n", finished_job->id);*/
    priqueue_remove(dispatcher.queue, finished_job);


    dispatcher.finished_jobs++;
    dispatcher.turnaround_time += time - finished_job->first_arrived;
    dispatcher.wait_time += time - finished_job->first_arrived - finished_job->total_duration;
    dispatcher.response_time += finished_job->first_scheduled - finished_job->first_arrived;


    free(finished_job);
    finished_job = NULL;


    if (priqueue_size(dispatcher.queue) < dispatcher.cores) {
        next_job = NULL;
    } else {
        for (int i = 0; i < priqueue_size(dispatcher.queue); i++) {
            job_t* potential_job = priqueue_at(dispatcher.queue, i);
            int running = 0;

            for (int j = 0; j < dispatcher.cores; j++) {
                if (dispatcher.jobs[j] == potential_job) {
                    running = 1;
                    break;
                }
            }

            if (running == 0) {
                next_job = potential_job;
                break;
            }
        }
        /*next_job = (job_t *) priqueue_at(dispatcher.queue, 0);*/
    }

    // If there is no new job then return -1
    if (next_job == NULL) {
        return -1;
    }
    for (int i = 0; i < dispatcher.cores; i++) {
        if (dispatcher.jobs[i] == NULL) {
            dispatcher.jobs[i] = next_job;
            next_job->started_time = time;
            if (next_job->first_scheduled == -1) {
                next_job->first_scheduled = time;
            }
            return next_job->id;
        }
    }

	return -1;
}


/**
  When the scheme is set to RR, called when the quantum timer has expired
  on a core.
 
  If any job should be scheduled to run on the core free'd up by
  the quantum expiration, return the job_number of the job that should be
  scheduled to run on core core_id.

  @param core_id the zero-based index of the core where the quantum has expired.
  @param time the current time of the simulator. 
  @return job_number of the job that should be scheduled on core cord_id
  @return -1 if core should remain idle
 */
int scheduler_quantum_expired(int core_id, int time)
{
    /*printf("Queue: %d\n", priqueue_size(dispatcher.queue));*/
    job_t* prev = dispatcher.jobs[core_id];
    if (priqueue_size(dispatcher.queue) <= dispatcher.cores) {
        return prev->id;
    }

    dispatcher.jobs[core_id] = NULL;
    priqueue_remove(dispatcher.queue, prev);

    prev->started_time = -1;
    prev->arrived_time = time;
    priqueue_offer(dispatcher.queue, prev);


    job_t* next_job = priqueue_at(dispatcher.queue, dispatcher.cores-1);
    next_job->started_time = time;
    if (next_job->first_scheduled == -1) {
        next_job->first_scheduled = time;
    }
    dispatcher.jobs[core_id] = next_job;


	return next_job->id;
}


/**
  Returns the average waiting time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all jobs that have arrived will have finished and no new jobs will arrive).
  @return the average waiting time of all jobs scheduled.
 */
float scheduler_average_waiting_time()
{
	return ((float) dispatcher.wait_time) / (float) dispatcher.finished_jobs;
}


/**
  Returns the average turnaround time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all jobs that have arrived will have finished and no new jobs will arrive).
  @return the average turnaround time of all jobs scheduled.
 */
float scheduler_average_turnaround_time()
{
	return ((float) dispatcher.turnaround_time) / (float) dispatcher.finished_jobs;
}


/**
  Returns the average response time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all jobs that have arrived will have finished and no new jobs will arrive).
  @return the average response time of all jobs scheduled.
 */
float scheduler_average_response_time()
{
	return ((float) dispatcher.response_time) / (float) dispatcher.finished_jobs;
}


/**
  Free any memory associated with your scheduler.
 
  Assumption:
    - This function will be the last function called in your library.
*/
void scheduler_clean_up()
{

    /* If the queue is not empty clear it */
    if (priqueue_size(dispatcher.queue) > 0) {
        job_t* job = priqueue_poll(dispatcher.queue);
        while (job != NULL) {
            free(job);
            job = NULL;
        }
    }

    /* Destroy the queue */
    priqueue_destroy(dispatcher.queue);
    free(dispatcher.queue);

    /* Free the malloced array of cores */
    free(dispatcher.jobs);
}


/**
  This function may print out any debugging information you choose. This
  function will be called by the simulator after every call the simulator
  makes to your scheduler.
  In our provided output, we have implemented this function to list the jobs in the order they are to be scheduled. Furthermore, we have also listed the current state of the job (either running on a given core or idle). For example, if we have a non-preemptive algorithm and job(id=4) has began running, job(id=2) arrives with a higher priority, and job(id=1) arrives with a lower priority, the output in our sample output will be:

    2(-1) 4(0) 1(-1)  
  
  This function is not required and will not be graded. You may leave it
  blank if you do not find it useful.
 */
void scheduler_show_queue()
{
    int size = priqueue_size(dispatcher.queue);
    for (int i = 0; i < size; i++) {
        job_t* job = priqueue_at(dispatcher.queue, i);
        printf("%d ", job->id);
    }


}

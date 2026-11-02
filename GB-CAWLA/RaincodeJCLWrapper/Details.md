RaincodeJCLWrapper - Functional \& Technical Design

Implementation preparation (2026-10-09)

An initial .NET 10 REST wrapper and ETR application configuration have been prepared.
The wrapper was retargeted from .NET 8 to match the server's installed .NET 10
SDK and ASP.NET Core runtime. Rebuild and rerun tests after updating the server copy.
See [README.md](./README.md) for setup, API contracts, validation status and outstanding details.

Confirmed decisions: submit waits for completion; RETURNCODE.TXT contains maximum job CC;
reject duplicate active application/job submissions; allow four different jobs in parallel.
The initial API returns a null returnCode until completion, rather than the illustrative
submission returnCode of 0 below.

Confirmed follow-up: ETR maximum CC <= 4 is successful; Raincode is installed in
C:\Program Files\Raincode\Crossbow\net10.0\bin with system environment variables set.
The configured submit command uses that inherited PATH. Target server: AZWAPDFININFD01.
The submit process exit code ($LASTEXITCODE) is confirmed to equal maximum job CC.
Success requires maximum CC <= 4 and a matching process exit code.
Step restart passes -RESTART=STEP1.MYPROC to Raincode and creates a new linked
execution for a failed or completed source job. The wrapper does not infer restart
steps or manage Raincode checkpoints. HTTPS deployment and real-runtime validation
are still pending.

Overview

The objective of the RaincodeJCLWrapper is to provide a REST-based integration layer between CA Workload Automation (CAWLA / ESP dSeries) and Raincode Runtime.



Instead of CAWLA directly invoking Raincode jobs, CAWLA will call a REST endpoint exposed by RaincodeJCLWrapper. The wrapper will receive the JCL name, validate the request, submit the JCL using the Raincode submit utility, monitor execution, manage logging, and provide restart capabilities.



Current Requirements

Job Execution

Jobs are currently executed manually using:



submit -file="F:\\Raincode\\DefaultVolume\\ETR\\P\\MAIN\\JCLLIB\\PTRTMNTM.jcl"

Key Observation

Raincode already provides JCL execution capabilities.



Therefore:



No JCL parser is required.

No JCL interpretation logic is required.

Wrapper acts only as a scheduler integration layer.

High-Level Architecture

CAWLA

&#x20; |

&#x20; | HTTP POST

&#x20; |

&#x20; v

+---------------------+

| RaincodeJCLWrapper  |

+---------------------+

&#x20;        |

&#x20;        |

&#x20;        v

submit -file=<JCL>

&#x20;        |

&#x20;        v

+---------------------+

| Raincode Runtime    |

+---------------------+

&#x20;        |

&#x20;        v

&#x20;Application Programs

Responsibilities of RaincodeJCLWrapper

The wrapper will:



Job Submission

Accept REST requests from CAWLA

Validate requests

Resolve application configuration

Locate target JCL

Execution

Submit JCL using Raincode submit utility

Capture execution status

Capture return codes

Monitoring

Track submitted jobs

Expose job status APIs

Logging

Store execution logs

Store audit logs

Retain logs for 365 days

Restart

Maintain checkpoint data

Allow restart from a failed step

Supported Environment

Current Scope:



Environment = DEV

Root Folder:



F:\\Raincode\\DefaultVolume\\ETR\\P\\MAIN\\

Additional applications can be added later through configuration.



Dataset Mapping

Dataset mappings already exist.



Example:



Mainframe Dataset:



EPF.INPUT.CUST.FILE

Mapped Physical File:



F:\\Raincode\\DefaultVolume\\EPF\\P\\MAIN\\INPUT\\CUST.seq

Metadata File:



F:\\Raincode\\DefaultVolume\\EPF\\P\\MAIN\\INPUT\\CUST.meta

Dataset resolution is already handled through:



Catalog Manager

Existing DSN mappings

Wrapper will not perform dataset translation.



Utility Support

All Raincode-supported utilities are assumed to be available and automatically resolved.



Examples:



SORT

IDCAMS

IEBGENER

FTP

BPXBATCH

IKJEFT01

Custom Utilities

No utility-specific coding is required within the wrapper.



Configuration Strategy

Option A

One configuration file per application.



Example:



Config

|

+-- EPF.json

+-- ETR.json

+-- EGS.json

Sample ETR Configuration

{

&#x20; "Application": "ETR",

&#x20; "RootPath": "F:\\\\Raincode\\\\DefaultVolume\\\\ETR\\\\P\\\\MAIN",

&#x20; "JclPath": "F:\\\\Raincode\\\\DefaultVolume\\\\ETR\\\\P\\\\MAIN\\\\JCLLIB",

&#x20; "LogPath": "F:\\\\Raincode\\\\Logs\\\\ETR",

&#x20; "RetentionDays": 365

}

Sample EPF Configuration

{

&#x20; "Application": "EPF",

&#x20; "RootPath": "F:\\\\Raincode\\\\DefaultVolume\\\\EPF\\\\P\\\\MAIN",

&#x20; "JclPath": "F:\\\\Raincode\\\\DefaultVolume\\\\EPF\\\\P\\\\MAIN\\\\JCLLIB",

&#x20; "LogPath": "F:\\\\Raincode\\\\Logs\\\\EPF",

&#x20; "RetentionDays": 365

}

API Design

Submit Job

Endpoint

POST /api/jobs/submit

Request

{

&#x20; "application": "ETR",

&#x20; "jobName": "PTRTMNTM",

&#x20; "restartStep": null

}

Response

{

&#x20; "jobId": "9f55f9a7",

&#x20; "status": "Submitted",

&#x20; "returnCode": 0

}

Get Job Status

Endpoint

GET /api/jobs/{jobId}

Response

{

&#x20; "jobId": "9f55f9a7",

&#x20; "jobName": "PTRTMNTM",

&#x20; "status": "Running",

&#x20; "startTime": "2026-10-09T10:00:00"

}

Restart Job

Endpoint

POST /api/jobs/{jobId}/restart

Request

{

&#x20; "restartStep": "STEP040"

}

Retrieve Logs

Endpoint

GET /api/jobs/{jobId}/logs

CAWLA Integration

Current Design

CAWLA will invoke the REST API.



Example:



POST /api/jobs/submit

Payload:



{

&#x20; "application": "ETR",

&#x20; "jobName": "PTRTMNTM"

}

CAWLA does not need direct access to:



submit.exe

Raincode Runtime

JCL Paths

All runtime interactions will be handled by the wrapper.



Job Submission Process Flow

CAWLA

&#x20;  |

&#x20;  v

Submit Job Request

&#x20;  |

&#x20;  v

Validate Request

&#x20;  |

&#x20;  v

Load Application Configuration

&#x20;  |

&#x20;  v

Locate JCL File

&#x20;  |

&#x20;  v

Execute



submit -file=<JCL>



&#x20;  |

&#x20;  v

Capture RC

&#x20;  |

&#x20;  v

Store Logs

&#x20;  |

&#x20;  v

Return Job Status

Logging Requirements

Retention:



365 Days

Recommended Structure:



F:\\Raincode\\Logs

|

+-- ETR

|    |

|    +-- PTRTMNTM

|

+-- EPF

|

+-- EGS

Job Log Files

Execution.log

SYSOUT.log

SYSPRINT.log

API.log

Restart Capability

Requirement:



Restart from failed step.

Proposed Checkpoint Storage

Folder:



Checkpoints

Example:



{

&#x20; "JobName": "PTRTMNTM",

&#x20; "CompletedSteps": \[

&#x20;   "STEP010",

&#x20;   "STEP020"

&#x20; ]

}

Restart Request

{

&#x20; "restartStep": "STEP030"

}

Execution resumes from:



STEP030

Recommended Technology Stack

Runtime

.NET 10

ASP.NET Core Web API

Logging

Serilog

Configuration

Microsoft.Extensions.Configuration

Dependency Injection

Microsoft.Extensions.DependencyInjection

Background Processing

Hosted Services

Proposed Solution Structure

RaincodeJCLWrapper

|

+-- Controllers

|    |

|    +-- JobController.cs

|

+-- Services

|    |

|    +-- RaincodeSubmitService.cs

|    +-- RestartService.cs

|    +-- LogService.cs

|    +-- ConfigService.cs

|

+-- Models

|    |

|    +-- SubmitRequest.cs

|    +-- JobStatus.cs

|    +-- AppConfiguration.cs

|

+-- Config

|    |

|    +-- ETR.json

|    +-- EPF.json

|    +-- EGS.json

|

+-- Logs

|

+-- Checkpoints

|

+-- Program.cs

Sample Submission Service

public class RaincodeSubmitService

{

&#x20;   public async Task<int> SubmitJob(string jclFile)

&#x20;   {

&#x20;       var psi = new ProcessStartInfo

&#x20;       {

&#x20;           FileName = "submit",

&#x20;           Arguments = $"-file=\\"{jclFile}\\"",

&#x20;           RedirectStandardOutput = true,

&#x20;           RedirectStandardError = true,

&#x20;           UseShellExecute = false

&#x20;       };



&#x20;       var process = Process.Start(psi);



&#x20;       await process.WaitForExitAsync();



&#x20;       return process.ExitCode;

&#x20;   }

}

Open Questions

The following information is still required before detailed development starts:



1\. Submit Command Output

Need actual output of:



submit -file=<JCL>

For:



Successful execution

Failed execution

2\. Job Monitoring

Need confirmation on:



How can a running Raincode job be monitored?

Examples:



Command line

API

Database

Log files

3\. Failed Step Identification

Need confirmation on:



How do we identify the failed step?

Examples:



SYSOUT

Execution log

Database table

Runtime metadata

4\. Duplicate Submission Handling

Need business decision:



Can the same job be submitted simultaneously?

Options:



Allow

Multiple concurrent executions.

Prevent

Reject duplicate job if already running.

Final Recommendation

Build RaincodeJCLWrapper as a lightweight REST-based orchestration service and keep all JCL execution responsibility within Raincode.



This provides:



Clean CAWLA integration

No JCL parsing complexity

Easier maintenance

Centralized logging

Restart support

Future scalability across EPF, ETR, EGS and other applications

Configuration-driven onboarding of new applications

*          DATA SET DDMODLIB   AT LEVEL 001 AS OF 11/18/20                      
*PHASE MODLIBA                                                                  
         TITLE 'MODLIB - TABLE OF MODULE V. LOADMOD (FOR MODLINK)'              
***********************************************************************         
* LOADED BY MODLINK SUB   ROUTINE LINKED INTO CALLERS PROGRAM         *         
* CONTAINS TWO TABLES:                                                *         
* 1. TABLE OF ENTRYPOINT ID V. LOAD MODULE INDEX                      *         
* 2. TABLE OF LOAD MODULES ADDRESSED BY TABLE OF ENTRYPOINTS          *         
*    NOTE LOAD MODULE NAMES HAVE A SUFFIXES FOR TESTING               *         
***********************************************************************         
         PRINT NOGEN                                                            
MODLIB   CSECT                                                                  
***********************************************************************         
* ENTRY POINT LIST.                                                             
*        A(LOAD MODULE ENTRY),AL2(SPARE),AL2(ENTRY POINT ID)                    
*                                                                               
* NB: MUST BE IN ENTRY POINT ID SEQ WITH NO GAPS SO ENTRY POINT ID              
*     CAN BE USED AS INDEX INTO TABLE (1 BASED SO SUBTRACT 1).                  
*                                                                               
***********************************************************************         
EPTAB    DS    0XL8                                                             
         DC    A(DDDM24),AL2(0,DATAMGRQ)     ROUTINE NO. 01                     
         DC    A(DDDM24),AL2(0,DADDSQ)       ROUTINE NO. 02                     
         DC    A(DDDM24),AL2(0,ISDDSQ)       ROUTINE NO. 03                     
         DC    A(DDDM24),AL2(0,DMDANDXQ)     ROUTINE NO. 04                     
         DC    A(DDDM24),AL2(0,DYNALLOCQ)    ROUTINE NO. 05                     
         DC    A(DDDM24),AL2(0,WORKERQ)      ROUTINE NO. 06                     
         DC    A(DDDM24),AL2(0,DMDABUFFQ)    ROUTINE NO. 07                     
         DC    A(DDDM24),AL2(0,DMDALINKQ)    ROUTINE NO. 08                     
         DC    A(DDDM24),AL2(0,DMDAPTRSQ)    ROUTINE NO. 09                     
         DC    A(DDDM24),AL2(0,PQOPENQ)      ROUTINE NO. 10                     
         DC    A(DDDM24),AL2(0,DMENQDEQQ)    ROUTINE NO. 11                     
         DC    A(DDDM24),AL2(0,DMOD000Q)     ROUTINE NO. 12                     
         DC    A(DDDM24),AL2(0,DMACCEMUQ)    ROUTINE NO. 13                     
         DC    A(DDDM24),AL2(0,LOCKSPCQ)     ROUTINE NO. 14                     
         DC    A(DDDM24),AL2(0,DMDDNAMEQ)    ROUTINE NO. 15                     
         DC    A(DDDM24),AL2(0,LOCKUPQ)      ROUTINE NO. 16                     
         DC    A(DDDM24),AL2(0,MQRPTQ)       ROUTINE NO. 17                     
         DC    A(DDDM24),AL2(0,DMRCVUSSQ)    ROUTINE NO. 18                     
         DC    A(DDDM24),AL2(0,DMISGENQQ)    ROUTINE NO. 19                     
         DC    A(DDDM24),AL2(0,DMSHMUSSQ)    ROUTINE NO. 20                     
         DC    A(DDDM24),AL2(0,DMSYSFILQ)    ROUTINE NO. 21                     
         DC    A(DDDM24),AL2(0,GETRETQ)      ROUTINE NO. 22                     
         DC    A(DDDM24),AL2(0,DMDYNDDQ)     ROUTINE NO. 23                     
*&&UK*&& DC    A(DDDM24),AL2(0,PROMOTEQ)     ROUTINE NO. 24 UK ONLY             
*&&US*&& DC    A(0),AL2(0,0)                 ROUTINE NO. 24 US UNUSED           
         DC    A(DDDATE),AL2(0,ADDAYQ)       ROUTINE NO. 25                     
         DC    A(DDDATE),AL2(0,GETDAYQ)      ROUTINE NO. 26                     
         DC    A(DDDATE),AL2(0,DATCONQ)      ROUTINE NO. 27                     
         DC    A(DDDATE),AL2(0,DATCONXQ)     ROUTINE NO. 28                     
         DC    A(DDDATE),AL2(0,DATVALQ)      ROUTINE NO. 29                     
         DC    A(DDDATE),AL2(0,PERVALQ)      ROUTINE NO. 30                     
         DC    A(DDDATE),AL2(0,PERVERTQ)     ROUTINE NO. 31                     
         DC    A(X'FFFFFFFF'),AL2(X'FFFF',X'FFFF')                              
                                                                                
***********************************************************************         
* LOAD MODULE LIST. CL8'MODULE NAME',A(MODULE),AL1(FLAGS),XL3'SPARE'            
*                                                                               
* NB: MUST BE IN ENTRY POINT ID SEQ WITH NO GAPS SO ENTRY POINT ID              
*     CAN BE USED AS INDEX INTO TABLE (1 BASED SO SUBTRACT 1).                  
*                                                                               
***********************************************************************         
LMTAB    DS    0XL16                                                            
DDDM24   DC    C'DDDM24  ',A(0),AL1(DMGRMODQ),XL3'0',F'0' DMGR 24 BIT           
DDDM31   DC    C'DDDM31  ',A(0),AL1(DMGRMODQ),XL3'0',F'0' DMGR 31 BIT           
DDDATE   DC    C'DDDATE  ',A(0),AL1(0),XL3'0',F'0'        DATE 24 BIT           
*                                                                               
MODLINKD DSECT                                                                  
       ++INCLUDE DDMODLINKD                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDMODLIB  11/18/20'                                      
         END                                                                    

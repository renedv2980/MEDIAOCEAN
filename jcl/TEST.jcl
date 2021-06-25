*          DATA SET TEST       AT LEVEL 003 AS OF 12/17/93                      
//FT0741 JOB 1,PACK,MSGCLASS=T,MSGLEVEL=(1,1),PRTY=6,                           
//    USER=FACIL,                                                               
//    PASSWORD=FACIL,                                                           
//         NOTIFY=BGRI                                                          
//*NET      ID=PS1MK2,HC=1,RL=WSTXS1                                            
//*MAIN  PROC=04                                                                
//*MAIN  CLASS=S1MK                                                             
//STEP1  EXEC   FAC,PROG=SPTBAUPT,ACCT=FAF1Y074                                 
//*FORMAT  PR,DDNAME=SYSPRINT,DEST=ANYLOCAL,PRTY=24                             
//*FORMAT  PR,DDNAME=SYSPRINT,DEST=KEEP                                         
//*FORMAT  PR,DDNAME=SYSPRINT,DEST=OPER                                         
//*FORMAT  PR,DDNAME=SYSPRINT,DEST=FICHE,FORMS=FICHE                            
//SYSPRINT DD   SYSOUT=(T,,1PP),COPIES=1                                        
//RECVIN    DD UNIT=SYSDA,DISP=(OLD,KEEP,KEEP),                                 
//             DSN=SPTDISK.SP1311(0)                                            
//          DD UNIT=SYSDA,DISP=(OLD,KEEP,KEEP),                                 
//             DSN=SPTDISK.STX1311(0)                                           
//*ECVIN   DD   DSN=SPTTAPE.SP131102(-1),UNIT=TAPE,DISP=(OLD,KEEP)              
//*        DD   DSN=SPTTAPE.SP131102(0),UNIT=TAPE,DISP=(OLD,KEEP)               
//SORTCNTL DD   *                                                               
   DEBUG NOESTAE                                                                
   OPTION MAINSIZE=800K                                                         
//SYSIN    DD   *                                                               
SPOT1                                                                           
//                                                                              

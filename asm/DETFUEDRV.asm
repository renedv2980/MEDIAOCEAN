*          DATA SET DETFUEDRV  AT LEVEL 004 AS OF 07/06/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETFDRVA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'NCC AIUE DRIVER/PREPROCESSOR'                                   
***********************************************************************         
*                                                                               
* THIS IS THE NCC AIUE/CARRIAGE-UE PRE-PROCESSOR DRIVER MODULE.                 
*                                                                               
***********************************************************************         
DETFAIUE CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,DETFAIUE,=V(REGSAVE),R9                                        
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(,RF),=C'DRVTRACE'   DDNAME=DRVTRACE              
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
         BE    *+6                 ALL CARDS ARE VALID?                         
         DC    H'0'                NO: INVALID PARAMETER CARD                   
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'A',=CL8'FILESIN')                           
         CLI   DMCB+4,0            IS FILESIN ALLOCATED VIA DD STMT?            
         BE    *+12                NO                                           
         MVI   HAVEDATA,C'Y'       YES: WE HAVE SOMETHING TO CONVERT            
         B     OPENFILS                                                         
*                                                                               
* IF WE GET HERE, THEN THERE IS NO FILESIN DD STATEMENT IN THE JCL.             
* THAT MEANS THAT WE WANT TO DYNAMICALLY ALLOCATE AND CONCATENATE THE           
* ENTIRE CONTENTS OF AN NFS-MOUNTED FOLDER. THE "DDNFSNAMES" PROGRAM            
* MUST BE RUN IN A JOB STEP BEFORE THIS ONE, PRODUCING A TEMPORARY              
* DATASET WITH DDNAME NFSFILES. THAT DATASET WILL CONTAIN THE NAMES OF          
* ALL OF THE FILES IN THE MOUNTED FOLDER.                                       
*                                                                               
         LA    RF,NFSFILES                                                      
         MVC   DCBLRECL-IHADCB(,RF),=AL2(RECLENQ) SET LRECL IN DCB              
         MVI   HAVEDATA,C'N'       ASSUME MOUNTED FOLDER IS EMPTY               
         OPEN  NFSFILES            CONTAINS FILENAMES IN MOUNTED FOLDER         
*                                                                               
         GET   NFSFILES,RDW        GET THE FIRST FILENAME                       
         SR    R2,R2                                                            
         ICM   R2,3,RDW            RECLEN (FROM RDW)                            
         SHI   R2,4                L'RDW                                        
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',=CL8'FILESIN'),((R2),RECORD),   +        
               (X'54',DUMMYDCB)                                                 
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
         MVI   HAVEDATA,C'Y'       WE HAVE AT LEAST ONE FILE TO CONVERT         
*                                                                               
* ALLOCATE A DUMMY OUTPUT DATASET, AND CALL IDCAMS TO COPY THE INPUT            
* FILE TO IT. THIS IS A DEFENSIVE MEASURE, TO CATCH READ ERRORS THAT            
* MIGHT OCCUR BEFORE CALLING ICETOOL.                                           
* IDCAMS PRODUCES A NICE SYSPRINT REPORT SHOWING THE LAST VALID RECORD          
* NUMBER PROCESSED. BY EXAMINING THAT SYSPRINT OUTPUT, WE CAN IDENTIFY          
* THE INVALID RECORD NUMBER, AND REPORT THIS BACK TO NCC.                       
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'N',=CL8'SYSUT2'),(X'80',0),DUMMYDCB         
         OC    DMCB+4(4),DMCB+4                                                 
         JNZ   *+2                 UNSUCCESSFUL DUMMY ALLOCATION ?!?            
*                                                                               
* DYAMICALLY ALLOCATE:   //IDCAMSIN DD UNIT=SYSDA,SPACE=(TRK,1)                 
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'IDCAMSIN'),               +        
               (X'80',=X'000001000000')                                         
*                                                                               
* DYAMICALLY ALLOCATE:   //IDCAMS   DD SYSOUT=*                                 
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'IDCAMS'),(X'80',=CL21' ')          
*                                                                               
         OPEN  (IDCAMSIN,OUTPUT)                                                
         PUT   IDCAMSIN,REPROCRD   FIRST REPRO CONTROL CARD                     
         CLOSE IDCAMSIN                                                         
*                                                                               
         MVC   P(20),=C'CALL IDCAMS TO READ '                                   
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),RECORD      PRINT PATHNAME                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LINK  EP=IDCAMS,PARAM=(IDCAMS0,IDCAMSDD),VL=1                          
         ST    RF,RETCODE          SAVE IDCAMS RETURN CODE                      
         LTR   RF,RF                                                            
         BZ    FILEOK1                                                          
*                                                                               
         MVC   P(69),=C'RETURN FROM IDCAMS *** WITH ERRORS *** SEE IDCA+        
               MS OUTPUT FOR DETAILS.'                                          
         GOTO1 =V(PRINTER)                                                      
         B     DONE                                                             
*                                                                               
FILEOK1  DS    0H                                                               
         MVC   P(29),=C'SUCCESSFUL RETURN FROM IDCAMS'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXTFILE DS    0H                                                               
         GET   NFSFILES,RDW        GET THE NEXT FILENAME                        
         SR    R2,R2                                                            
         ICM   R2,3,RDW            RECLEN (FROM RDW)                            
         SHI   R2,4                L'RDW                                        
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',DUB),((R2),RECORD),             +        
               (X'5C',DUMMYDCB)                                                 
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
*                                                                               
         MVC   DUB1,SPACES                                                      
         ZIC   R1,DMCB+8                                                        
         N     R1,=X'00000007'     L'RETURNED DDNAME (MINUS ONE !!!)            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB1(0),DUB         RETURNED DDNAME FROM DYNALLOC                
*                                                                               
* CALL IDCAMS AGAIN, THIS TIME FOR THE FILE ABOUT TO BE CONCATENATED.           
* SEE COMMENT ABOVE FOR THE REASON WHY WE CALL IDCAMS.                          
*                                                                               
         OPEN  (IDCAMSIN,OUTPUT)                                                
         MVC   REPROINP,DUB1       USE THIS DDNAME FOR REPRO INPUT FILE         
         MVI   REPROINP+8,C')'     PARAMETER DELIMITER                          
         PUT   IDCAMSIN,REPROCRD   REPRO CONTROL CARD                           
         CLOSE IDCAMSIN                                                         
*                                                                               
         MVC   P(20),=C'CALL IDCAMS TO READ '                                   
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),RECORD      PRINT PATHNAME                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LINK  EP=IDCAMS,PARAM=(IDCAMS0,IDCAMSDD),VL=1                          
         ST    RF,RETCODE          SAVE IDCAMS RETURN CODE                      
         LTR   RF,RF                                                            
         BZ    FILEOK2                                                          
*                                                                               
         MVC   P(69),=C'RETURN FROM IDCAMS *** WITH ERRORS *** SEE IDCA+        
               MS OUTPUT FOR DETAILS.'                                          
         GOTO1 =V(PRINTER)                                                      
         B     DONE                                                             
*                                                                               
FILEOK2  DS    0H                                                               
         MVC   P(29),=C'SUCCESSFUL RETURN FROM IDCAMS'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'C',=CL8'FILESIN'),DUB1  CONCATENATE         
         B     NEXTFILE            ALLOCATE & CONCATENATE THE NEXT FILE         
*                                                                               
CLOSNAMS DS    0H                                                               
         CLOSE NFSFILES                                                         
*                                                                               
OPENFILS DS    0H                                                               
         CLI   HAVEDATA,C'Y'       IS THERE ANY DATA TO CONVERT?                
         BNE   DONE                NO                                           
*                                                                               
         MVC   P(42),=C'ABOUT TO CALL ICETOOL VIA TOOLIN INTERFACE'             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         ST    RF,RETCODE          HIGHEST ICETOOL OPERATOR RETURN CODE         
         LTR   RF,RF                                                            
         BZ    GOODICE             PERFECT                                      
         CHI   RF,8                DID A COUNT OPERATOR GET TRAPPED?            
         BE    BADICE              YES: THAT'S OKAY                             
         DC    H'0'                UNEXPECTED ICETOOL RETURN CODE               
*                                                                               
GOODICE  DS    0H                                                               
         MVC   P(30),=C'SUCCESSFUL RETURN FROM ICETOOL'                         
         GOTO1 =V(PRINTER)                                                      
         B     DONE                                                             
*                                                                               
BADICE   DS    0H                                                               
         MVC   P(39),=C'RETURN FROM ICETOOL *** WITH ERRORS ***'                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DONE     DS    0H                                                               
         MVC   P(20),=C'PROCESSING COMPLETED'                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
*                                                                               
READCRDS NTR1                                                                   
*                                                                               
READCARD DS    0H                                                               
         MVC   TITLE(28),=C'NCC AIUE PREPROCESSOR DRIVER'                       
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXTCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    RDCARD90            ALL PARAMETER CARDS VALID                    
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    NEXTCARD            YES                                          
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   *+18                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      DDSIO= OVERRIDE                              
         B     NEXTCARD                                                         
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     NEXTCARD                                                         
*                                                                               
RDCARD90 DS    0H                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     YES                 ALL CARDS ARE VALID                          
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,RECFM=VB,             +        
               EODAD=CLOSNAMS                                                   
*                                                                               
IDCAMSIN DCB   DDNAME=IDCAMSIN,DSORG=PS,MACRF=(GM,PM),RECFM=FB,LRECL=80         
*                                                                               
* THE DUMMYDCB IS USED AS TO HOLD DCB PARAMETERS FOR DDDYNALLOC.                
* THIS DCB ITSELF IS NEVER OPENED.                                              
DUMMYDCB DCB   DSORG=PS,RECFM=VB,MACRF=GM,LRECL=204,BLKSIZE=208                 
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
RETCODE  DC    F'0'                RETURN CODE FROM THIS PROGRAM                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
*                                                                               
IDCAMS0  DC    H'0'                FOR IDCAMS CALL                              
IDCAMSDD DS    0H                                                               
         DC    AL2(IDCAMSLQ)       IDCAMS: DDNAME OVERRIDE LIST                 
         DC    4XL8'00'                                                         
         DC    CL8'IDCAMSIN'       'SYSIN' -->    'IDCAMSIN'                    
         DC    CL8'IDCAMS'         'SYSPRINT' --> 'IDCAMS'                      
IDCAMSLQ EQU   *-IDCAMSDD                                                       
*                                                                               
REPROCRD DS    0CL80                                                            
         DC    C' REPRO OUTFILE(SYSUT2) INFILE('                                
REPROINP DC    C'FILESIN)'                                                      
         DC    CL(80-(*-REPROCRD))' '                                           
*                                                                               
HAVEDATA DS    C                   Y/N                                          
*                                                                               
RECLENQ  EQU   256                                                              
RDW      DS    F                                                                
RECORD   DS    CL(RECLENQ-L'RDW)                                                
         EJECT                                                                  
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL16'*DETFUEDRV SSB**'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 2                                                                
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DETFUEDRV 07/06/17'                                      
         END                                                                    

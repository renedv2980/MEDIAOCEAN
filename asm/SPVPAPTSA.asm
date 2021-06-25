*          DATA SET SPVPAPTSA  AT LEVEL 004 AS OF 09/23/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE PADDUSRA                                                                 
*INCLUDE DYNALLOC                                                               
         TITLE 'ADD AUTHORIZATION PARMS TO JOB STATEMENT IN JCL DECK'           
PADDUSER CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PADDUSER,=A(SAVEAREA)                                          
*                                                                               
* COPY THE SYSUT1 DATASET TO SYSUT2, INSERTING THIS CARD AFTER THE JOB          
*  OR NJB CARD:                                                                 
*  "//      USER=PANAPT,PASSWORD=<SEE BELOW>,"                                  
*                                                                               
* IF NO PARM= PARAMETER IS PASSED TO THIS PROGRAM, THEN THE NEW CARD            
*  GETS INSERTED IMMEDIATELY AFTER THE FIRST CARD, WHICH IS ASSUMED TO          
*  BE THE JOB CARD.                                                             
* IF A PARM= IS PASSED, THEN IT IS AN INTEGER REPRESENTING THE CARD             
*  NUMBER OF AN NJB CARD, IN WHICH CASE THE NEW CARD IS INSERTED                
*  IMMEDIATELY AFTER THAT. (THIS METHOD IS USED BY FRED ROE'S                   
*  EMERGENCY PANEL.)                                                            
*                                                                               
         LR    R1,RC                                                            
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         SR    RF,RF               ASSUME NO PARM                               
         ICM   RF,3,0(R1)          L'PARM                                       
         BNZ   *+12                                                             
         LHI   R8,1                NO PARM: NEW CARD GOES AFTER FIRST           
         B     GO                  NONE: FIRST CARD IS JOB CARD                 
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,2(0,R1)                                                      
         CVB   R8,DUB              R8 = NJB CARD NUMBER                         
         B     GO                                                               
*                                                                               
* PASSWORD IN SIXPACK FORMAT (SO IT DOESN'T APPEAR AS CLEAR TEXT IN             
*  THIS LOAD MODULE)                                                            
*                                                                               
PASSWORD DC    X'096082A225A7'     BOBBYSOX                                     
*                                                                               
GO       DS    0H                                                               
         OPEN  SYSUT1                                                           
         OPEN  (SYSUT2,OUTPUT)                                                  
*                                                                               
*&&US                                                                           
         MVI   SMTPINFO,C' '                                                    
         MVC   SMTPINFO+1(L'SMTPINFO-1),SMTPINFO                                
         MVI   SMTPCLAS,C'T'                                                    
         L     R1,X'10'(,0)        COMMUNICATION VECTOR TABLE                   
         L     R1,CVTSMCA-CVT(,R1) SYSTEM MANAGEMENT CONTROL AREA               
         LA    R1,SMCASID-SMCABASE(,R1)    CPU ID (SMF)                         
         MVC   SMTPWRTR,=C'SMTP01  ' ASSUME WE'RE ON SY1                        
         CLC   =C'SYC ',0(R1)      SYC = SY1                                    
         BE    *+10                                                             
         MVC   SMTPWRTR,=C'SMTP07  ' WE'RE NOT: ASSUME WE'RE ON SY7             
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=C'SMTPOUT '),SMTPINFO                  
*                                                                               
         MVI   EMAILFLG,C'Y'       WE'LL SEND AN E-MAIL                         
*                                                                               
         OPEN  (SMTPOUT,OUTPUT)                                                 
*                                                                               
         MVC   RECORD,=CL80'HELO DDNMVS'                                        
         PUT   SMTPOUT,RECORD                                                   
         MVC   RECORD,=CL80'MAIL FROM:<PANAPT@DDNMVS>'                          
         PUT   SMTPOUT,RECORD                                                   
*&&US*&& MVC   RECORD,=CL80'RCPT TO: <DEISDDNY@MEDIAOCEAN.COM>'                 
*&&UK*&& MVC   RECORD,=CL80'RCPT TO: <TCLEDDLO@MEDIAOCEAN.COM>'                 
         PUT   SMTPOUT,RECORD                                                   
         MVC   RECORD,=CL80'DATA'                                               
         PUT   SMTPOUT,RECORD                                                   
         MVC   RECORD,=CL80'FROM: PanAPT'                                       
         PUT   SMTPOUT,RECORD                                                   
*&&US*&& MVC   RECORD,=CL80'TO: DEISDDNY@MEDIAOCEAN.COM'                        
*&&UK*&& MVC   RECORD,=CL80'TO: TCLEDDLO@MEDIAOCEAN.COM'                        
         PUT   SMTPOUT,RECORD                                                   
*                                                                               
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31                     SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         MVC   SUBJOBNO,JSABJBID-JSAB(R1)  JOBID (E.G., JOB12345)               
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
         LA    R2,FULL                                                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   SUBJOBNM,0(R2)      JOBNAME                                      
         PUT   SMTPOUT,SUBJECT                                                  
*                                                                               
         MVC   RECORD,=CL80' '                                                  
         PUT   SMTPOUT,RECORD      NEED A BLANK LINE AS BODY LINE 1             
*                                                                               
ELOOP    GET   SYSUT1,RECORD       PUT ALL CARDS PRIOR TO NEW CARD              
         PUT   SYSUT2,RECORD                                                    
         PUT   SMTPOUT,RECORD                                                   
         BCT   R8,ELOOP                                                         
         B     PUTPASS                                                          
*&&                                                                             
*                                                                               
LOOP     GET   SYSUT1,RECORD       PUT ALL CARDS PRIOR TO NEW CARD              
         PUT   SYSUT2,RECORD                                                    
         BCT   R8,LOOP                                                          
*                                                                               
PUTPASS  DS    0H                                                               
*                                                                               
* DO A SIXUNPK ON THE PASSWORD FIELD AND PLACE IT IN THE OUTPUT CARD            
*                                                                               
         LA    R2,PASSWORD         A(COMPRESSED DATA)                           
         LA    R3,SECPSWD          A(OUTPUT AREA)                               
         LHI   R4,6                L'INPUT                                      
*                                                                               
         SR    R7,R7               CLEAR COUNTER                                
         SR    R6,R6               CLEAR INNER LOOP COUNTER                     
         LR    R5,R3               R5=OUTPUT FIELD                              
*                                                                               
OUTLP    SR    RE,RE               RE=HOLDS 3 BYTES OF INPUT                    
         ICM   RE,7,0(R2)          GET 3 LOW BYTES->4 CHARS                     
*                                                                               
LP       SRDL  RE,6                SHIFT LOWER 6 BITS INTO RF                   
         SRL   RF,26               RT JUSTIFY BITS                              
         O     RF,=X'000000C0'     TURN ON 2 HOBS (TOSSED BY SIXPACK)           
*                                                                               
         STC   RF,3(R5)            STORE EBCDIC CHAR IN OUTPUT                  
         LA    R6,1(R6)            BUMP LOOP COUNTER                            
         BCTR  R5,0                BACK UP 1 LOCATION                           
         CHI   R6,4                HAVE WE OUTPUT 4 CHARS?                      
         BL    LP                  NOT YET                                      
         LA    R7,3(R7)            BUMP OUTER LOOP COUNTER                      
         SR    R6,R6               RESET INNER LOOP COUNTER                     
         LA    R5,8(R5)            BUMP OUTPUT POINTER                          
         CR    R4,R7               HAVE WE PROCESSED ALL CHARS?                 
         BNH   *+12                YES                                          
         LA    R2,3(R2)            BUMP INPUT POINTER                           
         B     OUTLP               LOOP BACK TO TOP                             
*                                                                               
         PUT   SYSUT2,SECINFO      PUT THE NEW CARD                             
         MVC   SECPSWD,=C'XXXXXXXX'                                             
         CLI   EMAILFLG,C'Y'                                                    
         BE    YESEMAIL                                                         
*                                                                               
NEXT     GET   SYSUT1,RECORD       PUT REMAINING RECORDS TO END                 
         PUT   SYSUT2,RECORD                                                    
         B     NEXT                                                             
*                                                                               
CLOSE    CLOSE SYSUT1                                                           
         CLOSE SYSUT2                                                           
         B     XBASE                                                            
*                                                                               
YESEMAIL DS    0H                                                               
         PUT   SMTPOUT,SECINFO                                                  
*                                                                               
NEXT10   GET   SYSUT1,RECORD       PUT REMAINING RECORDS TO END                 
         PUT   SYSUT2,RECORD                                                    
         PUT   SMTPOUT,RECORD                                                   
         B     NEXT10                                                           
*                                                                               
         MVC   RECORD,=CL80'.'                                                  
         PUT   SMTPOUT,RECORD                                                   
         MVC   RECORD,=CL80'QUIT'                                               
         PUT   SMTPOUT,RECORD                                                   
*                                                                               
         CLOSE SMTPOUT                                                          
         CLOSE SYSUT1                                                           
         CLOSE SYSUT2                                                           
*                                                                               
XBASE    DS    0H                                                               
         XBASE                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
SYSUT1   DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
SYSUT2   DCB   DDNAME=SYSUT2,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB                
SMTPOUT  DCB   DDNAME=SMTPOUT,LRECL=80,RECFM=FB,DSORG=PS,MACRF=PM               
*                                                                               
SECINFO  DS    0CL80                                                            
         DC    C'//          USER=PANAPT,PASSWORD='                             
SECPSWD  DC    C'XXXXXXXX'   OVERWRITTEN BY THE PASSWORD                        
         DC    C','          THIS CARD IS ALWAYS CONTINUED                      
         DC    CL(L'SECINFO-(*-SECINFO))' '                                     
*                                                                               
RECORD   DS    CL80                                                             
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
*                                                                               
EMAILFLG DC    C'N'                ASSUME NO NOTIFICATION E-MAIL                
*                                                                               
SUBJECT  DC    CL80' '                                                          
         ORG   SUBJECT                                                          
         DC    C'SUBJECT: '                                                     
SUBJOBNM DS    CL8                                                              
         DC    C'('                                                             
SUBJOBNO DS    CL8                                                              
         DC    C') PADDUSER EXECUTION NOTIFICATION'                             
         ORG                                                                    
*                                                                               
SMTPINFO DS    0CL21               SYSOUT INFO FOR SMTP E-MAILS                 
SMTPCLAS DS    C                   CLASS                                        
SMTPNODE DS    CL8                 NODEID                                       
SMTPWRTR DS    CL8                 WRITER NAME                                  
SMTPFCDE DS    CL4                 FORM CODE                                    
*                                                                               
         DS    0D                                                               
SAVEAREA DC    5000X'00'                                                        
         SPACE 3                                                                
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         CVT   DSECT=YES                                                        
         IEESMCA                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPVPAPTSA 09/23/13'                                      
         END                                                                    

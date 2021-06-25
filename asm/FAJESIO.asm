*          DATA SET FAJESIO    AT LEVEL 002 AS OF 06/30/87                      
*PHASE FAJESIO,*                                                                
         TITLE 'JESSUB - INITIALISE AND SUBMIT JOB'                             
JESSUB   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY AWORK                                                            
         NBASE JESWRKX-JESWRKD,**JSUB**,WORK=JESWRK                             
         LR    RA,RC                                                            
         USING JESWRKD,RA          RA=A(W/S)                                    
         ST    RA,AWORK                                                         
         L     R9,0(R1)                                                         
         USING ATCD,R9             R9=A(ATC ENTRY)                              
         ST    R9,AATC                                                          
         L     R8,ATCSYSFC                                                      
         USING SYSFACD,R8          R8=A(SYSFACS)                                
         L     R6,VSSB                                                          
         USING SSBD,R6             R6=A(SSB)                                    
         LA    R7,JESORPL                                                       
         USING IFGRPL,R7           R7=A(RPL FOR INTERNAL READER)                
         ST    R7,ATCAORPL                                                      
         MVI   ATCTYPE,ATCTJESS                                                 
         MVC   ATCNAME,ANAME                                                    
         MVI   ATCSTAT1,ATCSATCH                                                
         MVI   ATCSTAT2,0                                                       
         MVI   ATCSTATI,0                                                       
         LA    RF,JESSUB                                                        
         ST    RF,ATCASYNC                                                      
         L     RF,=V(JESPUT)                                                    
         ST    RF,ATCSYNCH                                                      
         XC    ATCPCNT(16),ATCPCNT                                              
         L     RF,=A(JESCARDS)                                                  
         ST    RF,ATCACARD                                                      
         LA    R2,JESIRDR                                                       
         ST    R2,ATCAIRDR         SAVE A(ACB FOR INTERNAL READER)              
         OPEN  ((2))               OPEN INTERNAL READER                         
*                                                                               
SUB2     TBIN  MILI                                                             
         ST    R1,ATCWTIME         SET WAIT TIME                                
         L     R1,ATCWCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ATCWCNT          SET WAIT COUNT                               
         XC    ATCECB,ATCECB       CLEAR ECB FOR WAIT EVENT                     
         LA    R1,ATCECB                                                        
*                                  WAIT FOR EVENT COMPLETION                    
         TM    SSBSTAT1,SSBSEOJ                                                 
         BNZ   SUBX                                                             
         NI    ATCSTAT1,255-ATCSAACT                                            
         WAIT  1,ECB=(1),LONG=YES                                               
         OI    ATCSTAT1,ATCSAACT                                                
         TM    SSBSTAT1,SSBSEOJ                                                 
         BNZ   SUBX                                                             
*                                  SUBMIT JOB TO JES                            
         SR    R4,R4                                                            
         ICM   R4,1,ATCCARDS       R4=NUMBER OF CARDS                           
         BZ    SUB6                                                             
         L     R5,ATCACARD         R5=A(CARD DECK)                              
SUB4     MVC   JESIO,0(R5)                                                      
         PUT   RPL=(7)                                                          
         LA    R5,L'JESIO(R5)                                                   
         BCT   R4,SUB4                                                          
         ENDREQ RPL=(7)                                                         
         MVC   JESID,RPLRBAR       SAVE JES JOB ID IN DUB                       
*                                                                               
         MVC   JESNO,=X'FFFF'      SET BAD JOB NUMBER                           
         CLC   JESID(3),=C'JOB'    TEST VALID JES JOB ID                        
         BNE   SUB5                                                             
         MVC   DUB(5),=C'00000'    TEST VALID JES JOB NUMBER                    
         MVZ   DUB(5),JESID+3                                                   
         CLC   DUB(5),=C'00000'                                                 
         BNE   SUB5                                                             
         PACK  DUB,JESID+3(5)                                                   
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(2),DUB                                                       
         BNZ   SUB5                                                             
         MVC   JESNO,DUB+2                                                      
SUB5     MVC   SSBJESNO,JESNO      SET JES JOB NUMBER IN SSB                    
         OI    SSBJFLAG,SSBJFJS2   SET JOB NUMBER AWAITING POSTING              
*                                                                               
SUB6     MVI   ATCCARDS,0          RESET ATC CONTROL FIELDS                     
         MVI   ATCTSKID,0                                                       
         XC    ATCTRMNO,ATCTRMNO                                                
         XC    ATCATCB,ATCATCB                                                  
         B     SUB2                                                             
*                                                                               
SUBX     NI    ATCSTAT1,ATCSATCH   RESET TASK STATUS                            
         L     R2,ATCAIRDR                                                      
         CLOSE ((2))               CLOSE INTERNAL READER                        
         XBASE ,                   RETURN TO OPERATING SYSTEM                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
AWORK    DC    A(0)                ADDRESS OF WORK AREA                         
ANAME    DC    C'FAJESIO '         TASK NAME                                    
JESJOB   DC    CL8' '                                                           
         SPACE 1                                                                
JESIRDR  ACB   AM=VSAM,MACRF=(ADR,SEQ,OUT),DDNAME=IRDR                          
JESORPL  RPL   ACB=JESIRDR,OPTCD=(ADR,SEQ,SYN,NUP),RECLEN=80,          *        
               AREA=JESIO,AREALEN=80                                            
JESIO    DS    CL80                                                             
         SPACE 1                                                                
JESWRK   DS    250D                WORK AREA                                    
JESCARDM EQU   100                 MAXIMUM NUMBER OF CARDS                      
JESCARDS DS    (JESCARDM)CL80      CARD DECK TO BE SUBMITTED                    
         DROP  RB,R7                                                            
         TITLE 'JESPUT - BUILD CARD DECK AND POST EVENT FOR JESSUB'             
JESPUT   CSECT                                                                  
         NMOD1 0,**JPUT**                                                       
         L     RA,=V(AWORK)                                                     
         L     RA,0(RA)                                                         
         USING JESWRKD,RA          RA=A(W/S)                                    
         L     R9,AATC                                                          
         USING ATCD,R9             R9=A(ATC ENTRY)                              
         L     R8,ATCSYSFC                                                      
         USING SYSFACD,R8          R8=A(SYSFACS)                                
         L     R6,VSSB                                                          
         USING SSBD,R6             R6=A(SSB)                                    
         LR    R2,R1               R2=A(PARAMETER LIST)                         
         LM    R3,R4,0(R2)         R3=A(ACTION),R4=A(CARD)                      
         MVI   0(R2),0             RESET ERROR FLAG                             
*                                                                               
         TM    ATCSTAT1,ATCSAACT   TEST SUBMIT ACTIVE                           
         BNZ   PUTERR1             YES - RETURN BUSY ERROR                      
*                                                                               
         L     RF,SSBTKADR                                                      
         USING TCBD,RF             RF=A(TCB)                                    
         L     RE,TCBUTL                                                        
         USING UTLD,RE             RE=A(UTL)                                    
         TM    ATCSTAT1,ATCSSACT   TEST PUT ACTIVE                              
         BNZ   PUT4                                                             
         OC    ATCATCB,ATCATCB     TEST ATC OWNED BY TASK                       
         BNZ   PUTERR1                                                          
*                                  SET CALLER OWNING CARD DECK                  
         MVC   ATCTSKID,TCBTASK                                                 
         MVC   ATCTRMNO,TNUM                                                    
         ST    RF,ATCATCB          SET A(OWNER TCB ENTRY)                       
         MVI   ATCCARDS,0          RESET NUMBER OF CARDS IN DECK                
         NI    ATCSTAT1,ATCSATCH                                                
         OI    ATCSTAT1,ATCSSACT   SET PUT IS ACTIVE                            
         MVI   ACTION,ACTINI+ACTPUT                                             
         B     PUT6                                                             
*                                  ENSURE CALLER OWNS THE CARD DECK             
PUT4     CLC   ATCTSKID,TCBTASK                                                 
         BNE   PUTERR1                                                          
         CLC   ATCTRMNO,TNUM                                                    
         BNE   PUTERR1                                                          
         MVI   ACTION,ACTPUT+ACTEND+ACTCLR                                      
         DROP  RE,RF                                                            
*                                                                               
PUT6     LA    RE,ACTTAB                                                        
PUT8     CLI   0(RE),0             TEST END OF ACTION TABLE                     
         BE    PUTERR2                                                          
         CLC   1(3,RE),0(R3)       MATCH PARAMETER TO TABLE                     
         BE    *+12                                                             
         LA    RE,L'ACTTAB(RE)                                                  
         B     PUT8                                                             
         NC    ACTION,0(RE)        TEST ACTION SEQUENCE IS VALID                
         BZ    PUTERR3                                                          
*                                                                               
         CLI   ACTION,ACTINI       CALL TO INITIALISE                           
         BE    PUTX                                                             
         CLI   ACTION,ACTCLR       CALL TO CLEAR (ABEND)                        
         BE    PUTFREE                                                          
         CLI   ACTION,ACTPUT       CALL TO PUT A CARD                           
         BE    PUT10                                                            
         CLI   ACTION,ACTEND       CALL TO END (POST EVENT)                     
         BE    PUT12                                                            
*                                                                               
PUT10    ZIC   RF,ATCCARDS                                                      
         LA    RE,1(RF)            BUMP NUMBER OF CARDS IN DECK                 
         LA    R0,JESCARDM-1                                                    
         CR    RE,R0                                                            
         BNL   PUTERR4                                                          
         STC   RE,ATCCARDS                                                      
         LA    RE,L'JESCARDS                                                    
         MR    RE,RE                                                            
         A     RF,ATCACARD                                                      
         MVC   0(L'JESCARDS,RF),0(R4) MOVE CARD TO DECK                         
         B     PUTX                                                             
*                                                                               
PUT12    SR    RF,RF                                                            
         ICM   RF,1,ATCCARDS                                                    
         BZ    PUTERR5                                                          
         BCTR  RF,0                RF=N'CARDS IN DECK-1                         
         LA    R1,L'JESCARDS                                                    
         MR    R0,RF                                                            
         A     R1,ATCACARD         RE=A(LAST CARD IN DECK)                      
         CLC   JESEND,0(R1)        TEST THIS IS AN END CARD                     
         BE    PUT14                                                            
         LA    RF,2(RF)            CREATE END CARD IN DECK                      
         STC   RF,ATCCARDS                                                      
         LA    R1,L'JESCARDS(R1)                                                
         MVC   0(L'JESEND,R1),JESEND                                            
*                                                                               
PUT14    TBIN  MILI                                                             
         ST    R1,ATCPTIME         SET POST TIME                                
         L     R1,ATCPCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ATCPCNT          SET POST COUNT                               
         LA    R1,ATCECB           POST EVENT COMPLETE                          
         POST  (1)                                                              
         NI    ATCSTAT1,255-ATCSSACT                                            
         B     PUTX                                                             
         EJECT                                                                  
* HANDLE ERRORS                                                                 
*                                                                               
PUTERR1  MVI   0(R2),X'FF'         SUBMIT FACILITY IS BUSY                      
         B     PUTX                                                             
PUTERR2  MVI   0(R2),X'FE'         INVALID ACTION CALL                          
         B     PUTFREE                                                          
PUTERR3  MVI   0(R2),X'FD'         INVALID ACTION SEQUENCE                      
         B     PUTFREE                                                          
PUTERR4  MVI   0(R2),X'FC'         TOO MANY CARDS TO PUT                        
         B     PUTFREE                                                          
PUTERR5  MVI   0(R2),X'FB'         NO CARDS TO SUBMIT                           
         B     PUTFREE                                                          
*                                                                               
PUTFREE  MVI   ATCCARDS,0          RESET ATC CONTROL FIELDS                     
         MVI   ATCTSKID,0                                                       
         XC    ATCTRMNO,ATCTRMNO                                                
         NI    ATCSTAT1,255-ATCSSACT                                            
         XC    ATCATCB,ATCATCB                                                  
         B     PUTX                                                             
*                                                                               
PUTX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
JESEND   DC    CL80'//'            JES TERMINATOR CARD                          
         SPACE 1                                                                
ACTTAB   DS    0XL4                *** CALLABLE ACTION TABLE ***                
         DC    AL1(ACTINI),C'INI'  INIT CALL  (ALLOCATE DECK TO USER)           
         DC    AL1(ACTPUT),C'PUT'  PUT A CARD (ADD TO JESCARDS)                 
         DC    AL1(ACTEND),C'END'  SUBMIT JOB (POST EVENT COMPLETE)             
         DC    AL1(ACTCLR),C'CLR'  CLEAR DECK (ABNORMAL TERMINATION)            
         DC    AL1(0)                                                           
         EJECT                                                                  
* DSECT TO COVER JESWORK                                                        
*                                                                               
JESWRKD  DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
AATC     DS    A                   A(ATC ENTRY)                                 
JESID    DS    CL8                                                              
JESNO    DS    H                                                                
ACTION   DS    X                   SYNCHRONOUS ACTION                           
ACTINI   EQU   X'80'                                                            
ACTPUT   EQU   X'40'                                                            
ACTEND   EQU   X'20'                                                            
ACTCLR   EQU   X'10'                                                            
JESWRKX  EQU   *                                                                
         SPACE 1                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* ATC BLOCK - NON STANDARD FIELD DEFINITION                                     
*                                                                               
ATCD     DSECT                                                                  
         ORG   ATCOTHER                                                         
ATCAIRDR DS    A                   A(ACB FOR INTERNAL READER)                   
ATCAORPL DS    A                   A(RPL FOR INTERNAL READER)                   
ATCACARD DS    A                   A(CARD DECK TO SUBMIT)                       
ATCTRMNO DS    H                   TERMINAL NUMBER IN PROCESS                   
ATCTSKID DS    C                   TASK NUMBER IN PROCESS                       
ATCCARDS DS    X                   NUMBER OF CARDS IN DECK SO FAR               
         SPACE 1                                                                
* IFGRPL                                                                        
         IFGRPL                                                                 
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002FAJESIO   06/30/87'                                      
         END                                                                    

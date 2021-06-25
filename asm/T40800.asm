*          DATA SET T40800     AT LEVEL 049 AS OF 05/01/02                      
*PHASE T40800A,+0,NOAUTO                                                        
         TITLE 'PRINTPAK COMMENT MAINTENANCE'                                   
T40800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 400,T40800                                                       
         USING GENOLD,RC                                                        
         USING T408FFD,RA                                                       
MDERR    EQU   13                  INVALID MEDIA                                
ACERR    EQU   12                  INVALID ACTION                               
CHGERR   EQU   142                                                              
RSIZERR  EQU   227                                                              
DUPERR   EQU   52                  DUPLICATE KEY ON ADD                         
MTCHERR  EQU   53                  RECORD NOT FOUND                             
MSERR    EQU   1                   MISSING INPUT                                
INVERR   EQU   2                   INVALID INPUT                                
*                                                                               
         SPACE 2                                                                
         BAS   RE,INITL                                                         
         XC    CMTMSG,CMTMSG                                                    
         FOUT  CMTMSGH                                                          
         EJECT                                                                  
*                                  EDIT ACTION                                  
         LA    R2,CMTACH                                                        
         LA    R3,ACERR                                                         
*                                  SET SO ERROR ON ACTION WILL NOT              
*                                  DISALLOW CHANGE NEXT TIME                    
         MVC   KEYSAVE,SVKEY                                                    
         BAS   RE,ANY                                                           
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         OC    CMTAC,BLANKS                                                     
         CLC   CMTAC(3),=C'ADD'                                                 
         BNE   ACT                                                              
         FOUT  CMTACH,=C'ADD'                                                   
         B     ACT4                                                             
ACT      CLC   CMTAC(3),=C'DIS'                                                 
         BNE   ACT2                                                             
         MVC   PREVCOM(3),CMTAC                                                 
         XC    CMTAC,CMTAC                                                      
         FOUT  CMTACH                                                           
         B     ACT8                                                             
ACT2     CLC   CMTAC(3),=C'CHA'                                                 
         BNE   ERROR                                                            
*                                                                               
*                                  IF CHANGE PREVIOUS COMMENT SHOULD            
*                                      HAVE BEEN DISPLAY.  IF NOT,              
*                                      TREAT CHANGE AS A DISPLAY.               
         CLC   PREVCOM(3),=C'DIS'                                               
         BE    ACT3                                                             
         LA    R3,CHGERR                                                        
         B     ERROR                                                            
ACT3     DS    0H                                                               
         MVI   CHSW,1               0=DIS,1=CHA,2=ADD                           
         B     ACT8                                                             
ACT4     MVC   PREVCOM(3),=C'DIS'                                               
         MVI   CHSW,2                                                           
ACT8     OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  EDIT MEDIA                                   
MED      LA    R2,CMTMDH                                                        
         LA    R3,MDERR                                                         
         TM    4(R2),X'20'                                                      
         BO    NUM                                                              
         NI    CMTMDH+4,X'DF'                                                   
         XC    CMTMDNM,CMTMDNM                                                  
         FOUT  CMTMDNMH                                                         
         BAS   RE,ANY                                                           
*                                  READ AGY MED REC                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,1                                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         FOUT  CMTMDNMH,PAGYMED                                                 
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  EDIT COMMENT NUMBER                          
NUM      LA    R2,CMTNOH                                                        
*****                                                                           
         LA    R3,2                      MISSING INPUT FIELD                    
         CLC   8(6,R2),=C'NVTEXT'        NEW TEXT?                              
         BNE   NUM30                                                            
         CLI   5(R2),6                   IF ONLY 'NVTEST' ENTERED,              
         BH    NUM20                     CLIENT = ALL                           
NUM10    XC    KEY,KEY                                                          
         MVC   KEY+4(3),=X'FFFFFF'       3X'FF'S IN REC KEY FOR ALL CLI         
         B     NUM25                                                            
NUM20    CLI   14(R2),C','               WAS CLIENT OR OFFICE SPECIFIED         
         BNE   ERROR                     (OR COULD BE 'ALL')                    
         OC    15(3,R2),BLANKS                                                  
         CLC   15(3,R2),BLANKS           IF ',BLANKS' ERROR                     
         BE    ERROR                                                            
         CLC   15(3,R2),=C'ALL'          WAS ALL SPECIFIED                      
         BE    NUM10                                                            
         CLI   15(R2),C'*'               WAS OFFICE SPECIFIED                   
         BNE   NUM23                                                            
         CLI   5(R2),9                   IF INPUT LONGER THAN THIS IS           
         BH    ERROR                     NOT A VALID OFFICE CODE                
         MVC   KEY+4(3),=X'FF4040'       FOR OFFICE CODE IN REC, STORE          
         MVC   KEY+5(1),16(R2)           X'FF' , OFFICE CODE, BLANK             
         B     NUM25                                                            
*                                                                               
NUM23    LA    R3,40                     CLIENT NOT ON FILE ERROR MSG           
         XC    KEY,KEY                   READ CLIENT HEADER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),15(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+4(3),15(R2)                                                  
NUM25    MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'41'                                                      
         B     NUM55                                                            
*****                                                                           
NUM30    LA    R3,MSERR                                                         
         BAS   RE,ANY                                                           
*                                  RIGHT JUSTIFY COMMENT NO                     
         CLI   5(R2),6                                                          
         BNH   NUM35                                                            
NUMERR   LA    R3,INVERR             MAX 6 CHARS                                
         B     ERROR                                                            
*                                                                               
NUM35    SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LA    R3,6                                                             
         SR    R3,R4                                                            
         LA    R5,WKNO(R3)                                                      
         MVC   WKNO(6),BLANKS                                                   
         EX    R4,MVNO                                                          
*                                                                               
NUM40    LA    R4,WKNO                                                          
         LA    R5,6                                                             
NUM45    CLI   0(R4),C','          DISALLOW COMMAS IN NUMBER                    
         BE    NUMERR                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,NUM45                                                         
*                                                                               
NUM46    LA    R4,WKNO                                                          
         LA    R5,6                                                             
NUM47    CLI   0(R4),C'('          DISALLOW COMMAS IN NUMBER                    
         BE    NUM48                                                            
NUM47A   LA    R4,1(R4)                                                         
         BCT   R5,NUM47                                                         
         B     NUM50                                                            
*                                                                               
NUM48    CLI   2(R4),C')'          DISALLOW (X) IN CODE FOR ANY X               
         BE    NUMERR                                                           
         B     NUM47A                                                           
*                                  READ COMMENT REC                             
NUM50    XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WKNO                                                    
NUM55    BAS   RE,HIGH                                                          
         CLI   CMTAC,C'A'                                                       
         BE    ADD00                                                            
*                                  IF DISPLAY OR CHANGE - REC                   
*                                      MUST BE FOUND                            
         LA    R3,MTCHERR                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         CLI   CHSW,1                                                           
         BE    CHNG                                                             
         EJECT                                                                  
*                                  GET ELEMENTS                                 
DISP     LA    R5,PCOMREC+33       COULD ALSO BE PNVTREC                        
         LA    R2,CMTCM1H                                                       
         SPACE 2                                                                
DISP2    CLI   0(R5),X'40'                                                      
         BNE   DISP4                                                            
         XC    8(70,R2),8(R2)      FIRST CLEAR LINE                             
         SR    R4,R4                                                            
         IC    R4,1(R5)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,MVCMT                                                         
         FOUT  (R2)                                                             
         LA    R2,78(R2)                                                        
         MVI   ELCODE,X'40'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   DISP4                                                            
         B     DISP2                                                            
DISP4    FOUT  CMTMSGH,=C'COMMENT DISPLAYED SUCCESSFULLY'                       
         BAS   RE,CLRSCRN          CLEAR REST OF SCREEN                         
         LA    R2,CMTACH                                                        
         CLI   CHSW,2                                                           
         BNE   *+8                                                              
         LA    R2,CMTCM1H                                                       
         B     EXIT                                                             
         SPACE 2                                                                
MVCMT    MVC   8(0,R2),2(R5)                                                    
MVNO     MVC   0(0,R5),8(R2)                                                    
         EJECT                                                                  
*                                  IF ACTION IS CHANGE COMPARE                  
*                                      TO PRIOR KEY                             
CHNG     CLC   SVKEY(10),KEYSAVE                                                
         BE    CHNG2                                                            
         LA    R2,CMTACH                                                        
         LA    R3,CHGERR                                                        
         B     ERROR                                                            
*                                  CREATE ALTERED REC                           
CHNG2    LA    R2,CMTCM1H                                                       
         LA    R3,MSERR                                                         
         B     ADD01                                                            
*                                  RETURN FROM ADD0I                            
CHNG4    BAS   RE,PUTREC                                                        
         FOUT  CMTMSGH,=C'COMMENT CHANGED SUCCESSFULLY'                         
         LA    R2,CMTACH                                                        
         B     REQ                                                              
         EJECT                                                                  
*                                  ON ADD MATCHING REC SHOULD NOT BE            
*                                      FOUND.                                   
ADD00    LA    R3,DUPERR                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BE    ERROR                                                            
*                                  THERE SHOULD BE AT LEAST ONE NON-            
*                                      BLANK COMMENT LINE.                      
         LA    R2,CMTCM1H                                                       
         LA    R3,MSERR                                                         
*                                  SET UP REC IN IOAREA                         
         MVC   PCOMKEY(10),KEYSAVE                                              
         XC    PCOMKEY+11(15),PCOMKEY+11                                        
ADD01    MVC   PCOMLEN(2),=H'33'                                                
*                                  SET UP ELEMENTS                              
         LA    R4,PCOMREC+33                                                    
         LR    RE,R4               CLEAR COMMENT AREA                           
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
ADD02    CLI   5(R2),0                                                          
         BE    ADD03                                                            
         OC    8(L'CMTCM1,R2),BLANKS                                            
         CLC   8(L'CMTCM1,R2),BLANKS                                            
         BNZ   ADD04                                                            
ADD03    BAS   R9,BMPFLD                                                        
         BE    ADD06                                                            
         B     ADD02                                                            
ADD04    MVI   ADDSW,1                                                          
         LA    R5,7+70(R2)                                                      
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         SH    R5,=H'5'                                                         
         STC   R5,ELEM+1                                                        
         MVI   ELEM,X'40'                                                       
         BCTR  R5,R0                                                            
         EX    R5,MVECM                                                         
         CLC   ELEM+2(7),=C'++START'                                            
         BE    ADD04E                                                           
         CLC   ELEM+2(5),=C'++END'                                              
         BE    ADD04E                                                           
         CLI   ELEM+2,C'+'                                                      
         BNE   ADD04E                                                           
         CLI   ELEM+3,C'1'                                                      
         BL    COMMERR                                                          
         CLI   ELEM+3,C'3'                                                      
         BH    COMMERR                                                          
*                                                                               
ADD04E   MVC   HALF,PCOMLEN                                                     
         LH    RE,HALF                                                          
         LA    RE,3(R5,RE)         NEW RECORD LENGTH                            
         CH    RE,=H'975'                                                       
         BL    ADD05                                                            
         LA    R3,RSIZERR                                                       
         B     ERROR                                                            
ADD05    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PCOMREC),ELEM,(R4)                                
         LA    R4,1(R4,R5)                                                      
         BAS   R9,BMPFLD                                                        
         BNE   ADD02                                                            
*                                  TEST IF ANY ELEMS ADDED                      
ADD06    LA    R2,CMTCM1H                                                       
         CLI   ADDSW,1                                                          
         BNE   ERROR                                                            
         MVI   ADDSW,0                                                          
         CLI   CHSW,1                                                           
         BE    CHNG4                                                            
*                                  ADD REC                                      
         MVC   KEY(25),PCOMKEY                                                  
         BAS   RE,ADDREC                                                        
         FOUT  CMTMSGH,=C'COMMENT ADDED SUCCESSFULLY'                           
         LA    R2,CMTACH                                                        
         B     REQ                                                              
         SPACE 3                                                                
MVECM    MVC   ELEM+2(0),8(R2)                                                  
*                                                                               
COMMERR  FOUT  CMTMSGH,=C'COMMENT LINE STARTING WITH ''+'' MUST BE FOLLX        
               OWED BY 1,2,OR 3',59                                             
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  ROUTINE TO GET NEXT ELEM                     
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
         LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
*                                                                               
*                                  ROUTINE TO GET NEXT INPUT FIELD              
BMPFLD   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BCR   8,R9                                                             
         CLI   0(R2),0                                                          
         BR    R9                                                               
         SPACE 2                                                                
CLRSCRN  DS    0H                                                               
         CLI   0(R2),9             CHEACK EOS                                   
         BNH   CSX                                                              
*                                                                               
         OC    8(70,R2),BLANKS                                                  
         CLC   8(70,R2),BLANKS                                                  
         BE    CS4                                                              
         MVC   8(70,R2),BLANKS                                                  
         FOUT  (R2)                                                             
CS4      DS    0H                                                               
         LA    R2,78(R2)                                                        
         B     CLRSCRN                                                          
CSX      DS    0H                                                               
         BR    RE                                                               
         SPACE 3                                                                
REQ      DS    0H                                                               
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'47'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),CMTMD                                                 
         MVC   QAREA+52(6),WKNO                                                 
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,47                                                       
         MVI   QCTL+14,106                                                      
         CLC   CMTNO(6),=C'NVTEXT'                                              
         BNE   REQ5                                                             
         MVC   QAREA(2),=C'NT'                                                  
         MVC   QAREA+52(3),KEY+4                                                
         MVC   QAREA+55(3),=3C' '                                               
         MVI   QCTL+10,122            FROM REQTAB IN PRREQ00                    
*                                     ENTRY FOR NT                              
*                                                                               
REQ5     GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    EXIT                                                             
         SR    R3,R3                                                            
         B     ERROR                                                            
         EJECT                                                                  
*                  INITIALISATION CODE                                          
         SPACE 3                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
*                                                                               
EXXMOD   MVC   SVKEY(10),KEYSAVE                                                
         MVI   CHSW,0                                                           
         XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*                                                                               
BLANKS   DC    70C' '                                                           
ELEM     DS    CL70                                                             
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE GENOLD                                                         
         SPACE 2                                                                
         ORG   IOAREA                                                           
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PCOMREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PNVTREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE T408FFD                                                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE PCMTTWA                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049T40800    05/01/02'                                      
         END                                                                    

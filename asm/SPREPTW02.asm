*          DATA SET SPREPTW02  AT LEVEL 020 AS OF 05/01/02                      
*PHASE SPTW02A,*                                                                
         TITLE 'SPTW02 CREATE NEW TRAFFIC TWX PROFILE RECS'                     
***********************************************************************         
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO OUTPUT TRAFFIC TWX PROFILE REC                         
*        R3 - POINTER TO OUTPUT TRAFFIC TWX PROFILE CURR ELEM                   
*        R4 -                                                                   
*        R5 - POINTER TO INPUT TRAFFIC STATION REC                              
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER                              
*        R7 - SECOND BASE REG                                                   
*        R8 -                                                                   
*        R9 - POINTER TO SPWORKD+4096                                           
*        RA - POINTER TO SPWORKD                                                
*        RB - FIRST BASE                                                        
*        RC -                                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        QOPT2=TEST PRINT OUT CONVERTED RECS IN HEX                             
*                                                                               
*  LEV 12    JAN25/88 CONVERT TEL NO. AND CREATE DATE OF CONVERSION             
*  LEV 13    JAN27/88 ADD CML TYPE ELEM IF CML TYPE                             
*                                                                               
***********************************************************************         
         EJECT                                                                  
SPTW02   START 0                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPTW02**,R7                                                    
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE                                                                  
         CLI   MODE,REQFRST                                                     
         BE    SPTRA10                                                          
EXIT     XIT1                                                                   
         SPACE                                                                  
SPTRA10  DS    0H                                                               
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         STM   R7,RB,SPTWRR                                                     
         OPEN  (TWXTP,(OUTPUT))                                                 
         L     R2,=A(WORKO)                                                     
         TITLE 'SPTW02 CREATE NEW TRAFFIC TWX PROFILE RECS-FILE OPEN'           
         L     R5,ADBUY                                                         
         ST    R5,AREC                                                          
         MVC   COMMAND,DMRDHI                                                   
         MVC   FILE,SPTDIR                                                      
         USING STARECD,R5          OLD REC FORMAT                               
         XC    STADDKEY,STADDKEY                                                
         MVC   STAKID,=X'0A28'       FILE CODE                                  
         MVC   STAKAM,BAGYMD        START THIS AGENCY                           
         MVC   KEY,STADDKEY                                                     
         MVC   KEY1,STADDKEY                                                    
         MVC   KEYSAVE,STADDKEY                                                 
         SPACE                                                                  
* DO FIRST READ                                                                 
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,                                           C        
               DMRDHI,             PASS RECORD LENGTH, COMMAND         C        
               SPTDIR,             FILE NAME                           C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
         GOTO1 DATAMGR,DMCB,                                           C        
               GETREC,             PASS RECORD LENGTH, COMMAND         C        
               SPTFILE,            FILE NAME                           C        
               KEY+14,             KEY ADDRESS                         C        
               (R5),               WORK AREA ADDRESS                   C        
               DMWORK                                                           
         TITLE 'SPTW02 CREATE NEW TRAFFIC TWX PROFILE RECS'                     
* CONVERT SPOT FILE STATION RECORDS LOOP                                        
         SPACE                                                                  
SPTRA40  DS    0H                                                               
         TM    DMCB+8,X'80'        IS THIS END OF FILE?                         
         BO    SPTRA80                                                          
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    RECRDCTR,=P'1'                                                   
         CLC   STAKID,=X'0A28'     IS THIS RECORD ID CODE                       
         BH    SPTRA90             NO, TREAT AS EOF                             
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLC   STAKAM,BAGYMD    THIS REQUESTED STATION FILE                     
         BL    SPTRA80             NO, GET NEXT REC                             
         BH    SPTRA90             TREAT AS EOF                                 
         AP    INCTR,=P'1'                                                      
         MVI   WORK,0                                                           
         MVN   WORK(1),STAKAM                                                   
         CLI    WORK,2             IS IT RADIO                                  
         BNE   SPTRA42              NO                                          
         AP    MEDRCTR,=P'1'                                                    
         B     SPTRA46                                                          
SPTRA42  CLI   WORK,1              IS IT TV                                     
         BNE   SPTRA44              NO, UNKNOWN                                 
         AP    MEDTCTR,=P'1'                                                    
         B     SPTRA46                                                          
SPTRA44  AP    MEDUCTR,=P'1'                                                    
         B     SPTRA80             BYPASS CONVERTING UNKNOWN                    
         SPACE                                                                  
* PRINT OUT INPUT RECORD IN HEX                                                 
         SPACE                                                                  
SPTRA46  DS    0H                                                               
         LR    R6,R5                                                            
         MVI   BYTE,X'10'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STADTAEL,R6                                                      
         CLI   STADTALN,119        OLD REC LEN                                  
         BE    SPTRA80                                                          
         OC    STATWX,STATWX       ANY TWX NUMBER                               
         BNZ   *+14                                                             
         OC    STATWXGC,STATWXGC   ANY DDS CODE (GRAPHNET)                      
         BZ    SPTRA80                                                          
         AP    CNVCTR,=P'1'        ADD TO RECS CONVERTED COUNT                  
         AP    OTCTR,=P'1'                                                      
         USING CSTREC,R2                                                        
         XC    CSTREC(256),CSTREC                                               
         MVI   CSTKTYP,C'6'                                                     
         MVC   CSTKSTA,STAKSTA                                                  
         MVC   CSTLEN,=H'75'       28 KEY+STATUS+LEN, 46 FOR TWX, 1 EOR         
         LA    R3,CSTDATA                                                       
         MVI   0(R3),X'10'                                                      
         MVI   1(R3),46                                                         
         MVC   2(20,R3),STATWX                                                  
         MVC   22(20,R3),STATWXAB                                               
         MVC   42(4,R3),STATWXGC                                                
         LA    R3,46(,R3)                                                       
         MVI   0(R3),X'21'                                                      
         MVI   1(R3),26                                                         
         MVC   2(24,R3),STALINE1                                                
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,26(,R1)                                                       
         STCM  R1,3,CSTLEN                                                      
         LA    R3,26(,R3)                                                       
         SPACE                                                                  
         OC    STALINE2,STALINE2                                                
         BZ    SPTRA47                                                          
         CLC   STALINE2,SPACES                                                  
         BE    SPTRA47                                                          
         MVI   0(R3),X'22'                                                      
         MVI   1(R3),26                                                         
         MVC   2(24,R3),STALINE2                                                
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,26(,R1)                                                       
         STCM  R1,3,CSTLEN                                                      
         LA    R3,26(,R3)                                                       
         SPACE                                                                  
SPTRA47  OC    STALINE3,STALINE3                                                
         BZ    SPTRA48                                                          
         CLC   STALINE3,SPACES                                                  
         BE    SPTRA48                                                          
         MVI   0(R3),X'23'                                                      
         MVI   1(R3),26                                                         
         MVC   2(24,R3),STALINE3                                                
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,26(,R1)                                                       
         STCM  R1,3,CSTLEN                                                      
         LA    R3,26(,R3)                                                       
         SPACE                                                                  
SPTRA48  OC    STALINE4,STALINE4                                                
         BZ    SPTRA50                                                          
         CLC   STALINE4,SPACES                                                  
         BE    SPTRA50                                                          
         SPACE                                                                  
* SEE IF LINE4 IS TELEPHONE *                                                   
         SPACE                                                                  
         BAS   RE,CKTEL                                                         
         BE    SPTRA50             YES, BYPASS LINE 4 TEL NO IS SAVED           
         SPACE                                                                  
         LA    RE,STALINE4                                                      
         MVI   0(R3),X'24'                                                      
         MVI   1(R3),26                                                         
         MVC   2(24,R3),STALINE4                                                
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,26(,R1)                                                       
         STCM  R1,3,CSTLEN                                                      
         LA    R3,26(,R3)                                                       
         SPACE                                                                  
SPTRA50  OC    SVTEL,SVTEL         TELEPHONE NUMBER                             
         BZ    SPTRA52                                                          
         MVI   0(R3),X'30'         TELEPHONE NUMBER                             
         MVI   1(R3),13+2                                                       
         MVC   2(13,R3),SVTEL                                                   
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,15(,R1)                                                       
         STCM  R1,3,CSTLEN                                                      
         LA    R3,15(,R3)                                                       
         SPACE                                                                  
SPTRA52  OC    STACMLT,STACMLT     CMML TYPE                                    
         BZ    SPTRA54                                                          
         CLC   STACMLT,SPACES      CMML TYPE                                    
         BE    SPTRA54                                                          
         MVI   0(R3),X'40'         CMML TYPE                                    
         MVI   1(R3),3+2                                                        
         MVC   2(13,R3),STACMLT                                                 
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,5(,R1)                                                        
         STCM  R1,3,CSTLEN                                                      
         LA    R3,5(,R3)                                                        
         SPACE                                                                  
SPTRA54  MVI   0(R3),X'F1'         DATE ELEM                                    
         MVI   1(R3),5                                                          
         GOTO1 DATCON,DMCB,(5,0),(3,2(R3))                                      
         SR    R1,R1                                                            
         ICM   R1,3,CSTLEN                                                      
         LA    R1,5(,R1)                                                        
         STCM  R1,3,CSTLEN                                                      
         LA    R3,5(,R3)                                                        
         MVI   0(R3),0             EOR                                          
         SPACE                                                                  
         MVC   P1+4(1),QMED                                                     
         MVC   P1+10(5),STAKSTA                                                 
         MVC   P1+19(20),STATWX                                                 
         MVC   P1+39(20),STATWXAB                                               
         MVC   P1+64(4),STATWXGC                                                
         MVC   P1+74(24),STALINE1                                               
         MVC   P2+74(24),STALINE2                                               
         MVC   P3+74(24),STALINE3                                               
         MVC   P4+74(24),STALINE4                                               
         MVC   P4+100(L'SVTEL),SVTEL                                            
         GOTO1 REPORT                                                           
         SPACE                                                                  
         CLC   QOPT2(4),=CL4'TEST' TEST RUN?                                    
         BNE   SPTRA70              NO                                          
         BAS   RE,PTNEWREC         PRINT CONVERTED RECORD IN HEX                
* PUT CONVERTED RECORD OUT TO TAPE                                              
         SPACE                                                                  
SPTRA70  SR    RE,RE                                                            
         ICM   RE,3,CSTLEN         GET LENGTH                                   
         LA    RE,4(,RE)           ADD 4 FOR VARIABLE FIELD                     
         STCM  RE,3,VARFLD                                                      
         PUT   TWXTP,VARFLD                                                     
         SPACE                                                                  
* GET NEXT NEW TRAFFIC SPOT FILE STATION RECORD                                 
         SPACE                                                                  
SPTRA80 GOTO1 DATAMGR,DMCB,                                            C        
               DMRSEQ,             READ SEQUENTIAL                     C        
               SPTDIR,             ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
         GOTO1 DATAMGR,DMCB,                                           C        
               GETREC,             PASS RECORD LENGTH, COMMAND         C        
               SPTFILE,            FILE NAME                           C        
               KEY+14,             KEY ADDRESS                         C        
               (R5),               WORK AREA ADDRESS                   C        
               DMWORK                                                           
         B     SPTRA40                                                          
         EJECT                                                                  
* FORMAT TOTALS & CLOSE OUTPUT TAPE                                             
         SPACE                                                                  
SPTRA90  DS    0H                                                               
         CLOSE (TWXTP)                                                          
         MVC   P(17),=CL17'TOTAL RECS READ ='                                   
         EDIT  RECRDCTR,(7,P+24),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P(18),=CL18'AGENCY RECS READ ='                                  
         EDIT  INCTR,(7,P+24),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         MVC   P(20),=CL20'MEDIA R RECS READ ='                                 
         EDIT  MEDRCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(20),=CL20'MEDIA T RECS READ ='                                 
         EDIT  MEDTCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(21),=CL21'MEDIA UNK RECS READ ='                               
         EDIT  MEDUCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(19),=CL19'TV RECS CONVERTED ='                                 
         EDIT  CNVTCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(22),=CL22'RADIO RECS CONVERTED ='                              
         EDIT  CNVRCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(16),=CL16'RECS CONVERTED ='                                    
         EDIT  CNVCTR,(7,P+24),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P(14),=CL14'ZIP ERR RECS ='                                      
         EDIT  ERRCTR,(7,P+24),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P(22),=CL22'RECS WRITTEN TO TAPE ='                              
         EDIT  OTCTR,(7,P+24),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ             FORCE END OF REQUEST                         
         DC    H'0'                SHOULD NEVER COME BACK HERE                  
         EJECT                                                                  
* SEE IF LINE4 IS TELEPHONE *                                                   
         SPACE                                                                  
CKTEL    NTR1                                                                   
         XC    SVTEL,SVTEL                                                      
         CLI   STALINE4,C'('       (123)123-1234                                
         BNE   CKTEL10             X                                            
         CLI   STALINE4+4,C')'     (123)123-1234                                
         BNE   CKTEL10                 X                                        
         CLI   STALINE4+8,C' '     (123)123-1234                                
         BE    CKTEL10                  X                                       
         CLI   STALINE4+8,C'-'     (123)123-1234                                
         BNE   CKTEL10                     X                                    
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(3),STALINE4+1                                               
         CLC   WORK(3),STALINE4+1  (123)123-1234                                
         BNE   CKTEL10              XXX                                         
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(3),STALINE4+5                                               
         CLC   WORK(3),STALINE4+5  (123)123-1234                                
         BNE   CKTEL10                  XXX                                     
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(4),STALINE4+9                                               
         CLC   WORK(4),STALINE4+9  (123)123-1234                                
         BNE   CKTEL10                                                          
         MVC   SVTEL,STALINE4                                                   
         B     CKTELEQ                      XXXX                                
         SPACE                                                                  
CKTEL10  CLI   STALINE4,C'('       (123) 123-1234                               
         BNE   CKTEL20             X                                            
         CLI   STALINE4+4,C')'     (123) 123-1234                               
         BNE   CKTELNE                 X                                        
         CLI   STALINE4+5,C' '     (123) 123-1234                               
         BNE   CKTELNE                  X                                       
         CLI   STALINE4+9,C'-'     (123) 123-1234                               
         BNE   CKTELNE                      X                                   
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(3),STALINE4+1                                               
         CLC   WORK(3),STALINE4+1  (123) 123-1234                               
         BNE   CKTELNE              XXX                                         
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(3),STALINE4+6                                               
         CLC   WORK(3),STALINE4+6  (123) 123-1234                               
         BNE   CKTELNE                   XXX                                    
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(4),STALINE4+10                                              
         CLC   WORK(4),STALINE4+10 (123) 123-1234                               
         BNE   CKTEL20                                                          
         MVC   SVTEL(5),STALINE4                                                
         MVC   SVTEL+5(8),STALINE4+6                                            
         B     CKTELEQ                      XXXX                                
CKTEL20  CLI   STALINE4+3,C'-'     123-123-1234                                 
         BNE   CKTELNE                X                                         
         CLI   STALINE4+7,C'-'     123-123-1234                                 
         BNE   CKTELNE                    X                                     
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(3),STALINE4                                                 
         CLC   WORK(3),STALINE4    123-123-1234                                 
         BNE   CKTELNE             XXX                                          
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(3),STALINE4+4                                               
         CLC   WORK(3),STALINE4+4  123-123-1234                                 
         BNE   CKTELNE                 XXX                                      
         MVC   WORK(4),=C'00000'                                                
         MVN   WORK(4),STALINE4+8                                               
         CLC   WORK(4),STALINE4+8  123-123-1234                                 
         BNE   CKTELNE                     XXXX                                 
         MVI   SVTEL,C'('                                                       
         MVC   SVTEL+1(3),STALINE4                                              
         MVI   SVTEL+4,C')'                                                     
         MVC   SVTEL+5(8),STALINE4+4                                            
CKTELEQ  MVC   STALINE4,SPACES                                                  
         CR    RB,RB                                                            
         B     EXIT                                                             
CKTELNE  CR    RB,RD                                                            
         B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
* PRINT OUTPUT RECORD IN HEX                                                    
         SPACE                                                                  
         USING STARECD,R5                                                       
PTNEWREC NTR1                                                                   
         MVC   P(6),=CL6'OUTPUT'                                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO,P,25,0,0       KEY                             
         GOTO1 HEXOUT,DMCB,WORKO+25,P+60,2,0,0  CONTROL                         
         MVC   P+70(3),=CL3'KEY'                                                
         MVC   P2(25),WORKO                                                     
         GOTO1 REPORT                                                           
         SPACE                                                                  
         LA    R2,WORKO+28                                                      
PTNEW10  LA    R3,EXPTABL                                                       
         ZIC   R4,1(R2)                                                         
PTNEW14  CLC   0(1,R3),0(R2)                                                    
         BE    PTNEW20                                                          
         LA    R3,16(,R3)                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   PTNEW14                                                          
         DC    H'0'                                                             
PTNEW20  MVC   P+95(15),1(R3)                                                   
         GOTO1 HEXOUT,DMCB,(R2),P,(R4),0,0  ELEM CODE/ELEM LEN                  
         GOTO1 REPORT                                                           
         BCTR  R4,0                                                             
         EX    R4,PTNEWMVC                                                      
         GOTO1 REPORT                                                           
         LA    R2,1(R2,R4)                                                      
         CLI   0(R2),0                                                          
         BNE   PTNEW10                                                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
PTNEWMVC MVC   P(0),0(R2)                                                       
         SPACE 3                                                                
         GETEL R6,24,BYTE                                                       
         DS    0D                                                               
         USING *,RF                                                             
         DROP  R7,R9,RA,RB                                                      
HDHK     NTR1                                                                   
         LM    R7,RB,SPTWRR                                                     
         USING SPWORKD,RA,R9                                                    
         USING SPTW02,RB,R7                                                     
         DROP  RF                                                               
         B     EXIT                                                             
SPTWRR   DC    5F'0'                                                            
RECRDCTR DC    PL5'0'              TOTAL RECS READ                              
INCTR    DC    PL5'0'              TOT RECS READ-THIS AGENCY                    
MEDRCTR  DC    PL5'0'              MEDIA - R RECS READ                          
MEDTCTR  DC    PL5'0'              MEDIA - T RECS READ                          
MEDUCTR  DC    PL5'0'              MEDIA - UNKNOWN RECS READ                    
CNVTCTR  DC    PL5'0'              TV RECS CONVERTED                            
CNVRCTR  DC    PL5'0'              RADIO RECS CONVERTED                         
CNVCTR   DC    PL5'0'              ALL RECS CONVERTED                           
MTRCTR   DC    PL5'0'              MASTER STATION RECS RD                       
OTCTR    DC    PL5'0'              RECS WRITTEN                                 
ERRCTR   DC    PL5'0'              ERRORS FOUND-RECS WRITTEN ANYWAY             
SVTEL    DC    XL13'00'                                                         
EXPTABL  DC    X'10',CL15'TWX/AB'                                               
         DC    X'21',CL15'ADDR LINE 1'                                          
         DC    X'22',CL15'ADDR LINE 2'                                          
         DC    X'23',CL15'ADDR LINE 3'                                          
         DC    X'24',CL15'ADDR LINE 4'                                          
         DC    X'30',CL15'TELEPHONE NO.'                                        
         DC    X'40',CL15'COMML TYPE'                                           
         DC    X'F1',CL15'ACT DATE'                                             
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
* OUTPUT TAPE - TO BE USED AS LOAD TAPE TO CONTROL FILE                         
TWXTP    DCB   BLKSIZE=8000,                                           C        
               BUFNO=2,                                                C        
               DDNAME=TWXTP,                                           C        
               DEVD=DA,                                                C        
               DSORG=PS,                                               C        
               LRECL=196,                                              C        
               MACRF=(PM),                                             C        
               RECFM=VB                                                         
VARFLD   DS    0F                                                               
         DC    H'0'                                                             
         DC    H'0'                                                             
WORKO    DS    CL256                                                            
STAKEYSV DS    CL17                                                             
         EJECT                                                                  
* NEW TRAFFIC SYSTEM STATION NAME AND ADDRESS RECORD LAYOUT                     
         SPACE                                                                  
       ++INCLUDE SPTRSTA                                                        
STAEND   EQU   *                                                                
         EJECT                                                                  
* NEW TRAFFIC CONTROL PROFILE TWX RECCORD LAYOUT                                
         SPACE                                                                  
       ++INCLUDE CTGENSTTWX                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
SPTW02   CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPREPTW02 05/01/02'                                      
         END                                                                    

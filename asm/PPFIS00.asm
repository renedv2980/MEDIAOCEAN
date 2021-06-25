*          DATA SET PPFIS00    AT LEVEL 100 AS OF 05/20/20                      
*PHASE T41B00A                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE SRCHCALL                                                               
*INCLUDE PPBVAL                                                                 
         TITLE 'T41B00 - CHANGE LOG'                                            
*                                                                               
* BPLA  04/08   FIX LENGTH OF COMPARISON OF FISCLT AND =C'ALL'                  
*                                                                               
* BOBY  01/07   ADD PLANNED COSTS                                               
*                                                                               
* SMYE  04/06   FOR STEWARD ESTIMATES REPLACE *TEST* WITH *STEW*                
*                                                                               
* BOBY  10/05   2 CHARACTER MEDIA OFFICE CODES                                  
*                                                                               
* SMYE  05/02   NEW LIMIT ACCESS SECURITY (INCLUDING TRAFFIC OFFICE)            
*                                                                               
* BPLA   6/01   DISALLOW REQUESTS FOR CLIENT=ALL,                               
*               CLT=*N, CLT=&N IF CLIENT GROUP                                  
*               LIMIT ACCESS IS ACTIVE                                          
*               SUPPORT CLT GRP LIMIT ACCESS FOR                                
*               ONE CLIENT REQUESTS                                             
*                                                                               
* BPLA  6/01    BILLNG GROUP LIMIT ACCESS FIX                                   
*                                                                               
* BPLA  4/99    ALSO ALLOW "COST2"                                              
*                                                                               
* KWAN 02/999   CODES FOR OPTION "OPEN" OR "COS2"                               
*                                                                               
* SMYE 10/97    GETINS MADE CORE-RESIDENT                                       
*                                                                               
* BPLA 1/96     CHANGES TO VALMAX TO HANDLE YEARS OVER 2000                     
*               (WHICH HAVE X'FA', X'FB', ETC IN FIRST BYTE                     
*               OF EBCDIC FORMAT                                                
*                                                                               
* BPLA 5/95     ALLOW NAME=YES OR NO (SAME AS N=YES OR NO)                      
*               FOR PUB NAME DISPLAY                                            
*                                                                               
* BPLA 5/95     WHEN CLEARING PREVKEY ALSO CLEAR PREVDSW                        
*                                                                               
* BPLA 1/95     CHANGES FOR NEW MAXIO LOGIC                                     
*                                                                               
* BPLA 8/8/94   FIX BUG IN EDTC3                                                
*                                                                               
* BPLA 3/18/94  OC SVPRD WITH SPACES                                            
*                                                                               
* BPLA 11/8/93  CHANGES TO ALL $N IN CLIENT                                     
*                                                                               
* BPLA 11/5/93  ALLOW ? IN CLIENT AND "ALL" IN PUB                              
*               ALSO ACCEPT $N IN CLIENT IF ? IS IN PRD OR EST OR PUB           
*                                                                               
* BPLA 11/4/93  IF CLT = ? AND LIMIT ACCESS IS FOR AN OFFICE LIST ($N)          
*               SET SVCLT TO $N (LIMIT ACCESS)                                  
*                                                                               
* BPLA 10/8/93  DISALLOW CLT = ? IF LIMIT ACCESS TO OFFICE LIST ($N)            
*                                                                               
* BPLA 8/23/91 ADD PUB NAME SEARCHING                                           
*                                                                               
* BPLA 7/18/91 FIX ROGER'S CLIENT SECURITY - DIDN'T WORK FOR OFFICE             
*              REQUESTS WITH OFFICE LIST SECURITY                               
* BPLA 4/5/91  ADD MAXIO CHECK                                                  
*                                                                               
* ROSA 2/7/90  ADD CLIENT SECURITY                                              
*                                                                               
         TITLE 'T41B00 - PRINTPAK FINANCIAL INFO SYSTEM'                        
T41B00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1200,T41B00,R9,RR=R8    NEED XTRA BASE REG                       
         USING GENOLD,RC                                                        
*                                                                               
         ST    R8,RELO                 WAS R9,RELO                              
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T41BFFD,RA                                                       
*                                                                               
         BAS   RE,INITL                                                         
*                                                                               
         MVI   GRPLACC,0    SET OFF GROUP LIMIT ACCESS SWITCH                   
*                                                                               
         CLI   6(RA),C'*'          CHECK FOR OFFICE OR GROUP ID                 
         BNE   VC6                                                              
*                                                                               
*        FIRST POSITION MAY NOT BE ALPHA FOR A GROUP                            
*                                                                               
*****    CLI   7(RA),C'A'          SEE IF ALPHA                                 
*****    BL    VC6                 NO - MUST BE OFFICE                          
*****    CLI   7(RA),C'Z'                                                       
*****    BH    VC6                                                              
         CLI   8(RA),C'0'          SEE IF 3RD BYTE IS NUMERIC                   
         BL    VC6                 NO - MUST BE OFFICE                          
         CLI   8(RA),C'9'                                                       
         BH    VC6                                                              
*                                                                               
         MVI   GRPLACC,C'Y'   CLT GROUP LIMIT ACCESS ACTIVE                     
*                                                                               
VC6      DS    0H                                                               
*                                                                               
         L     R0,=V(PPBVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,VPPBVAL                                                       
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
*        LOAD OFFICER                                                           
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFICER,DMCB          SAVE OFFICER ADDRESS                      
*                          GET AND STORE GETINS CORE-RESIDENT ADDRESS           
         MVC   DMCB+4(4),=X'D9000AAB'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETINS,DMCB        SAVE GETINS MODULE ADDRESS                   
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   GETFACT,CGETFACT                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB                                                          
         USING FACTSD,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,3,FATMAXIO                                                    
         SH    R1,=H'25'       25  BELOW MAXIMUM                                
         STH   R1,SVMAXIO                                                       
         DROP  R4                                                               
*                                                                               
         LA    R3,REC                                                           
         ST    R3,AREC                                                          
         XC    FISEMSG,FISEMSG                                                  
         FOUT  FISEMSGH                                                         
         MVI   PVSW,1                                                           
*                                                                               
         MVI   TRFAGSW,0           SET OFF TRAFFIC ID SWITCH                    
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    NOSECRET                                                         
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
         LA    R0,SECBLK           1024 BYTE "SECRET BLOCK"                     
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*  INITIALIZE SECURITY BLOCK                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SECBLK),0                                  
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
NOSECRET DS    0H                                                               
*                                                                               
         BRAS  RE,CKTRAFID         SEE IF "SIGN-ON" HAS A TRAFFIC ID            
         BNE   *+8                                                              
         MVI   TRFAGSW,C'Y'        YES - "SIGN-ON" HAS A TRAFFIC ID             
*                                                                               
         EJECT                                                                  
EDTMED   LA    R2,FISMDIAH         EDIT MEDIA                                   
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTCLT                                                           
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),8(R2)                                                   
         MVI   KEY+3,X'01'                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BE    EDTM10                                                           
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTM10   BAS   RE,GETREC                                                        
         MVC   SVAPROF,PAGYPROF                                                 
         FOUT  FISMDESH,PAGYMED,10                                              
         MVC   SVMED,8(R2)                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         EJECT                                                                  
EDTCLT   DS    0H                  EDIT CLIENT                                  
         LA    R2,FISCLTH          POINT TO CLIENT INPUT FIELD                  
*                                                                               
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTPRD                                                           
*                                                                               
         BAS   RE,UNVAL            UNVALIDATE ALL REMAINING KEY FIELDS          
         BAS   RE,ANY              READ IN FIELD                                
*                                                                               
         CLI   8(R2),C'?'          IF  LIST OF CLIENTS WANTED                   
         BNE   EDTC                                                             
*                                                                               
         CLI   GRPLACC,C'Y'        AND CLT GRP LIMIT ACCESS                     
         BE    EDTC4ER                 DISALLOW                                 
*                                                                               
         MVC   SVCLT,=C'ALL'           WILL BE RESET IF TERMINAL                
*                                      HAS LIMIT ACCESS                         
         XC    FISCLTN,FISCLTN                                                  
         B     EDTC4A                                                           
*                                                                               
EDTC     CLI   8(R2),C'*'          OFFICES                                      
         BE    EDTC2                                                            
*                                                                               
         CLI   8(R2),C'&&'         BILLING GROUP                                
         BE    EDTC1                                                            
*                                                                               
         CLC   8(3,R2),=C'ALL'     CAN DO ALL CLIENTS FOR ONE PUB               
*                                  OR PUB Q MARK DISPLAY                        
         BNE   EDTC5                                                            
*                                                                               
*                                  ALL CLIENT REQUEST                           
*                                                                               
         CLI   GRPLACC,C'Y'        CLT GRP LIMIT ACCESS                         
         BE    EDTC4ER             DISALLOW                                     
*                                                                               
         B     EDTC4                                                            
*                                                                               
EDTC1    DS    0H                  BILLING GROUP REQUESTS                       
*                                                                               
         CLI   5(R2),2             TESTS THAT ONLY 1 CH B GRP ENTERED           
         BNE   EDTC2ER                                                          
*                                                                               
         CLI   GRPLACC,C'Y'        IF CLT GRP LIMIT ACCESS                      
         BE    EDTC4ER             DISALLOW                                     
*                                                                               
         B     EDTC3                                                            
*                                                                               
EDTC2    DS    0H                  OFFICE REQUESTS                              
*                                                                               
         CLI   GRPLACC,C'Y'        CLT GRP LIMIT ACCESS                         
         BE    EDTC4ER             DISALLOW                                     
*                                                                               
         LA    R4,9(R2)            POINT TO OFFICE CODE                         
         ZIC   R5,5(R2)            TOTAL INPUT LENGTH                           
         BCTR  R5,0                DECREMENT LENGTH FOR *                       
*                                                                               
         CLI   9(R2),C'-'          ALL BUT                                      
         BNE   EDTC2A                                                           
*                                                                               
         CLI   5(R2),4             MAKE SURE OFFICE AT MOST 2 CH                
         BH    EDTC2ER                                                          
*                                                                               
         LA    R4,10(R2)           POINT TO OFFICE CODE                         
         BCTR  R5,0                DECREMENT FOR MINUS SIGN                     
*                                                                               
         B     EDTC2B                                                           
*                                                                               
EDTC2A   DS    0H                                                               
*                                                                               
*        VALIDATE FOR RANGE OF OFFICES                                          
*                                                                               
         CLI   5(R2),4             INPUT MUST BE AT LEAST 4 LONG                
         BL    EDTC2B              PROBABLY SINGLE OFFICE                       
*                                                                               
*        RANGE ENTRY MUST BE OF FORM *AA-BB (AA OR BB CAN BE 1 CH)              
*                                                                               
         CLI   1(R4),C'-'          IF OFFICE IS 1 CH                            
         BNE   *+12                                                             
         LHI   R5,1                   SET FIELD LENGTH                          
         B     EDTC2A1                                                          
*                                                                               
         CLI   2(R4),C'-'          IF OFFICE IS 2 CH                            
         BNE   EDTC2ER                   NOT A RANGE                            
         LHI   R5,2                   SET FIELD LENGTH                          
*                                                                               
EDTC2A1  DS    0H                  VALIDATE MEDIA OFFICE VIA OFFICER            
*                                                                               
         XC    SVCLT,SVCLT         INIT SAVEAREA                                
         MVI   SVCLT,C'*'          SET OFFICE INDICATOR                         
         XC    SVOFCST2,SVOFCST2                                                
         XC    SVOFCEN2,SVOFCEN2                                                
*                                                                               
         XC    WORK2,WORK2         INIT OFFICER CONTROL BLOCK                   
         LA    R3,WORK2                                                         
         USING OFFICED,R3          ESTABLISH OFFICER CONTROL BLOCK              
*                                                                               
         MVI   OFCSYS,C'P'         SET FOR PRINT SYSTEM                         
         MVC   OFCAUTH,6(RA)       PASS SECURITY INFO                           
         MVC   OFCLMT,6(RA)        PASS SECURITY INFO                           
         MVC   OFCAGY,AGYALPHA     SET AGENCY                                   
*                                                                               
         BCTR  R5,0                DECREMENT FOR EXECUTE                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   OFCOFC2(0),0(R4)       PASS OFFICE CODE                          
*                                                                               
         CLI   OFCOFC2+1,0         IF ONLY ONE CHARACTER                        
         BNE   *+8                                                              
         MVI   OFCOFC2+1,C' '         BLANK FILL FIELD                          
*                                                                               
*        VALIDATE START OFFICE ID AND GET OFFICE SHORT NAME                     
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS),          X        
               (C'S',SVOFCSTN)                                                  
*                                                                               
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BNE   EDTC2ER                                                          
*                                                                               
*        SAVE OFFICE DATA                                                       
*                                                                               
         MVC   SVOFCST2,OFCOFC2    SAVE OFFICE ALPHA CODE                       
         MVC   SVCLT+1(1),OFCOFC   SAVE OFFICE INTERNAL CODE                    
*                                                                               
         LA    R4,1+1(R5,R4)       POINT TO END OFFICE CODE                     
*                                                                               
         LR    RE,R4               CALCULATE REMAINING INPUT LENGTH             
         LA    RF,8(R2)                                                         
         SR    RE,RF                                                            
*                                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         SR    R5,RE               REMAINING LENGTH                             
         BNP   EDTC2ER             MUST BE SOMETHING LEFT                       
*                                                                               
         CHI   R5,2                MAX 2 CH'S                                   
         BH    EDTC2ER                                                          
*                                                                               
         XC    WORK2,WORK2         INIT OFFICER CONTROL BLOCK                   
         LA    R3,WORK2                                                         
         USING OFFICED,R3          ESTABLISH OFFICER CONTROL BLOCK              
*                                                                               
         MVI   OFCSYS,C'P'         SET FOR PRINT SYSTEM                         
         MVC   OFCAUTH,6(RA)       PASS SECURITY INFO                           
         MVC   OFCLMT,6(RA)        PASS SECURITY INFO                           
         MVC   OFCAGY,AGYALPHA     SET AGENCY                                   
*                                                                               
         BCTR  R5,0                DECREMENT FOR EXECUTE                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   OFCOFC2(0),0(R4)       PASS OFFICE CODE                          
*                                                                               
         CLI   OFCOFC2+1,0         IF ONLY ONE CHARACTER                        
         BNE   *+8                                                              
         MVI   OFCOFC2+1,C' '         BLANK FILL FIELD                          
*                                                                               
*        VALIDATE END OFFICE ID AND GET OFFICE SHORT NAME                       
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS),          X        
               (C'S',SVOFCENN)                                                  
*                                                                               
         CLI   0(R1),0             IF ERRORS                                    
         BNE   EDTC2ER                                                          
*                                                                               
*        SAVE OFFICE DATA                                                       
*                                                                               
         MVC   SVOFCEN2,OFCOFC2    SAVE OFFICE ALPHA CODE                       
         MVC   SVCLT+2(1),OFCOFC   SAVE OFFICE INTERNAL CODE                    
*                                                                               
         CLC   SVOFCST2,SVOFCEN2   START MUST BEFORE END                        
         BH    EDTC2ER                                                          
*                                                                               
         XC    FISCLTN,FISCLTN                                                  
         MVC   FISCLTN(8),SVOFCSTN SHOW NAMES IN RANGE                          
         MVI   FISCLTN+10,C'-'                                                  
         MVC   FISCLTN+12(8),SVOFCENN                                           
*                                                                               
         B     EDTC4A                                                           
*                                                                               
EDTC2B   DS    0H                                                               
*                                                                               
         XC    FISCLTN,FISCLTN     INT CLIENT NAME AREA                         
*                                                                               
         XC    WORK2,WORK2         INIT OFFICER CONTROL BLOCK                   
         LA    R3,WORK2                                                         
         USING OFFICED,R3          ESTABLISH OFFICER CONTROL BLOCK              
*                                                                               
         MVI   OFCSYS,C'P'         SET FOR PRINT SYSTEM                         
         MVC   OFCAUTH,6(RA)       PASS SECURITY INFO                           
         MVC   OFCLMT,6(RA)        PASS SECURITY INFO                           
         MVC   OFCAGY,AGYALPHA     SET AGENCY                                   
*                                                                               
         BCTR  R5,0                DECREMENT FOR EXECUTE                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   OFCOFC2(0),0(R4)       PASS OFFICE CODE                          
*                                                                               
         CLI   OFCOFC2+1,0         IF ONLY ONE CHARACTER                        
         BNE   *+8                                                              
         MVI   OFCOFC2+1,C' '         BLANK FILL FIELD                          
*                                                                               
*        VALIDATE OFFICE ID AND GET OFFICE LONG NAME                            
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS),          X        
               (C'L',FISCLTN)                                                   
*                                                                               
         CLI   0(R1),0             IF ERRORS                                    
         BNE   EDTC2ER                                                          
*                                                                               
*        SAVE OFFICE DATA                                                       
*                                                                               
         MVC   SVCLOFCN,FISCLTN    SAVE OFFICE LONG NAME                        
         XC    SVCLT,SVCLT         INIT SAVEAREA                                
         MVI   SVCLT,C'*'          SET OFFICE INDICATOR                         
*                                                                               
         LA    R1,SVCLT+1                                                       
*                                                                               
         CLI   9(R2),C'-'          IF ALL BUT OPTION                            
         BNE   *+12                                                             
         MVI   0(R1),C'-'             SET MINUS SIGN                            
         LA    R1,1(R1)               BUMP POINTER                              
*                                                                               
         MVC   0(1,R1),OFCOFC      SET 1 POSITION OFFCE ID                      
*                                                                               
         B     EDTC4A                                                           
*                                                                               
         DROP  R3                                                               
*                                                                               
EDTC2ER  LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTC3    DS    0H                  BILLING GROUP REQUESTS                       
         MVC   SVCLT,8(R2)                                                      
         OC    6(2,RA),6(RA)       CHK FOR LIMIT ACCESS                         
         BZ    EDTC4X                                                           
         CLI   6(RA),C'&&'         BILLING GROUP ACCESS                         
         BNE   EDTC4ER                                                          
         CLC   SVCLT(2),6(RA)      MUST MATCH                                   
         BNE   EDTC4ER                                                          
         B     EDTC4X                                                           
*                                                                               
EDTC4    DS    0H                                                               
*                                                                               
         MVC   SVCLT,8(R2)                                                      
         XC    FISCLTN,FISCLTN     INT CLIENT NAME AREA                         
*                                                                               
EDTC4A   OC    6(2,RA),6(RA)       CHK FOR LIMIT ACCESS                         
         BZ    EDTC4X                                                           
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDTC4ER                                                          
*                                                                               
         CLI   8(R2),C'?'          QMARK                                        
         BNE   EDTC4C                                                           
*                                                                               
*******  CLI   6(RA),C'$'          SEE IF OFFICE LIST LIMIT ACCESS              
*******  BE    EDTC4ER             DISALLOW                                     
*                                                                               
         MVC   SVCLT,6(RA)         USE CLT FROM TWA                             
*                                                                               
         B     EDTC4X                                                           
*                                                                               
EDTC4C   CLI   6(RA),C'*'          CHK FOR OFFICE LIMIT ACCESS                  
         BNE   EDTC4F                                                           
*                                                                               
         CLI   SVCLT+1,C'-'        CAN'T BE ALL BUT                             
         BE    EDTC4ER                                                          
*                                                                               
         CLI   SVCLT+2,0           CAN'T BE A RANGE OF OFFICES                  
         BNE   EDTC4ER                                                          
*                                                                               
         CLC   SVCLT+1(1),7(RA)    OFFICE MUST MATCH                            
         BE    EDTC4X                                                           
*                                                                               
EDTC4ER  LA    R3,ACCERR           NOT AUTHORIZED                               
         B     ERROR                                                            
*                                                                               
EDTC4F   CLI   6(RA),C'$'          OFFICE LIST LIMIT ACCESS                     
         BNE   EDTC4ER                                                          
*                                                                               
         CLI   SVCLT+1,C'-'        CAN'T BE ALL BUT                             
         BE    EDTC4ER                                                          
*                                                                               
*****    CLI   5(R2),2                                                          
*****    BNE   EDTC4ER                                                          
*                                                                               
         CLI   SVCLT,C'*'          MUST BE ASKING FOR AN OFFICE                 
         BNE   EDTC4ER                                                          
*                                                                               
         CLI   SVCLT+2,0           CAN'T BE A RANGE OF OFFICES                  
         BNE   EDTC4ER                                                          
*                                                                               
         MVC   PCLTOFF,SVCLT+1     SAVE AS THE CLIENT OFFICE                    
*                                                                               
         BAS   RE,PPCLIVER          CHK OFFICE SECURITY                         
         BNE   EDTC4ER             NOT AUTHORIZED                               
*                                                                               
EDTC4X   OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  FISCLTNH                                                         
         B     EDTPRD                                                           
*                                                                               
EDTC5    DS    0H                                                               
*                                                                               
         CLI   8(R2),C'$'        SEE IF OFFICE LIST                             
         BNE   EDTC8                                                            
*                                                                               
         CLI   5(R2),2                                                          
         BNE   EDTC2ER           MUST BE $N                                     
*                                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    EDTC5C                                                           
*                                                                               
         CLC   8(2,R2),6(RA)     MUST MATCH                                     
         BNE   EDTC4ER           NOT AUTHORIZED                                 
*                                                                               
EDTC5C   MVC   SVCLT(2),8(R2)                                                   
         B     EDTC4X                                                           
*                                                                               
EDTC8    XC    KEY,KEY           SINGLE CLT REQUEST                             
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),8(R2)                                                   
         OC    KEY+4(3),=3C' '                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BE    EDTC10                                                           
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTC10   BAS   RE,GETREC                                                        
*                                                                               
         MVC   SVCLT,KEY+4                                                      
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    ACCX                                                             
*                                                                               
         CLI   TRFAGSW,C'Y'        TRAFFIC AGENCY ID ?                          
         BNE   EDTC30              NO                                           
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R6,REC+33                                                        
*                                                                               
EDTC20   CLI   0(R6),0            END OF REC ?                                  
         BE    EDTC30             YES                                           
*                                                                               
         CLI   0(R6),X'50'        CLT TRAFFIC OFFICE ELEM CODE                  
         BE    EDTC25             YES                                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0              BUMP TO NEXT ELEM                             
         B     EDTC20                                                           
*                                                                               
EDTC25   MVC   PCLTOFF,2(R6)    REPLACE CLT OFFICE WITH TRAFFIC OFFICE          
*                                                                               
EDTC30   DS    0H                                                               
         CLI   6(RA),C'$'                                                       
         BE    ACC2                CALLING OFFICER FOR LIMIT ACCESS             
         CLI   6(RA),C'&&'         CHK FOR BILLING GROUP ACCESS                 
         BE    ACCB                                                             
*                                                                               
         CLI   6(RA),C'*'          CHK FOR OFFICE LIMIT ACCESS                  
         BNE   ACC2                                                             
*                                                                               
         CLI   GRPLACC,C'Y'        CLT GRP LIMIT ACCESS                         
         BE    ACC2                                                             
*                                                                               
         CLI   PCLTBLGP,C'*'       TO IGNORE CHK                                
         BE    ACCX                                                             
         B     ACC2                                                             
*                                                                               
ACCE     LA    R3,ACCERR           NOT AUTHORIZED                               
         B     ERROR                                                            
*                                                                               
ACCB     DS    0H                                                               
         CLC   PCLTBLGP(1),7(RA)     BILL GROUP MUST MATCH                      
         BE    ACCX                                                             
         B     ACCE                                                             
*                                                                               
ACC2     DS    0H                                                               
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BNE   ACCE                                                             
ACCX     DS    0H                                                               
         FOUT  FISCLTNH,PCLTNAME,20                                             
         MVC   SVCLT,KEY+4                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
*        READ B2B PROFILE FOR CLIENT                                            
*                                                                               
         XC    WORKCBLD,WORKCBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKPBLD,WORKPBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKEBLD,WORKEBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKEALD,WORKEALD   INIT ACTUALIZATION     DATES                 
         XC    WORKPCYN,WORKPCYN   INIT USING PC BILLING                        
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB2B'    REQUEST B2B PROFILE                          
         NI    WORK,X'FF'-X'40'    MAKE 'P' LOWERCASE                           
*                                  NEEDED FOR 3 CH PROF IDS                     
         MVC   WORK+4(2),PCLTKAGY  AGENCY                                       
         MVC   WORK+6(1),PCLTKMED  MEDIA                                        
         MVC   WORK+7(3),PCLTKCLT  CLIENT                                       
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF  CLIENT OFFICE                                
*                                                                               
         L     RF,ACOMFACS         COMFACS ADDRESS                              
         L     R0,CDATAMGR-COMFACSD(RF)    DATAMGR ADDRESS                      
         L     RF,CGETPROF-COMFACSD(RF)    GETPROF ADDRESS                      
*                                                                               
         GOTO1 (RF),DMCB,WORK,B2BPROF,(R0),0,0                                  
*                                                                               
         MVC   WORKPCYN,B2BPROF+12   SAVE PC BILLING OPTION                     
         MVC   WORKCBLD,B2BPROF+13   SAVE CLIENT BILLING EFF DATE               
*                                                                               
         OC    WORKCBLD,WORKCBLD   SKIP IF NONE                                 
         BZ    EDTCLT95                                                         
*                                                                               
         CLI   WORKCBLD,80         SET CENTURY                                  
         BNL   EDTCLT95                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORKCBLD         SET CENTURY                                  
         AHI   RF,100                                                           
         STC   RF,WORKCBLD                                                      
*                                                                               
EDTCLT95 DS    0H                                                               
*                                                                               
         EJECT                                                                  
EDTPRD   DS    0H                  EDIT PRODUCT                                 
         LA    R2,FISPRDH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTEST                                                           
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         MVC   SVPRD,8(R2)                                                      
         OC    SVPRD,=C'   '                                                    
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDTP10                                                           
         CLI   8(R2),C'?'          ACCEPT Q MARK                                
         BNE   EDTP2                                                            
         MVC   SVPRD,=C'ALL'       CHG TO ALL IN SVPRD                          
         B     EDTP10                                                           
EDTP2    DS    0H                                                               
*                                                                               
         CLC   SVCLT,=C'ALL'                                                    
         BE    EDTP10                                                           
         CLI   SVCLT,C'*'       OFFICE                                          
         BE    EDTP10                                                           
         CLI   SVCLT,C'$'       OFFICE LIST                                     
         BE    EDTP10                                                           
         CLI   SVCLT,C'&&'      BILLING GROUP                                   
         BE    EDTP10                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         MVC   KEY+7(3),SVPRD                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDTP5                                                            
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTP5    BAS   RE,GETREC                                                        
         FOUT  FISPRDNH,PPRDNAME,20                                             
         MVC   SVPRD,KEY+7                                                      
         B     EDTP12                                                           
*                                                                               
EDTP10   XC    FISPRDN,FISPRDN                                                  
         FOUT  FISPRDNH                                                         
*                                                                               
EDTP12   OI    4(R2),X'20'                                                      
         EJECT                                                                  
EDTEST   DS    0H                  EDIT ESTIMATE                                
         LA    R2,FISESTH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTST                                                            
         XC    FISETST,FISETST                                                  
         FOUT  FISETSTH                                                         
*                                                                               
         XC    PREVKEY,PREVKEY                                                  
         MVI   PREVDSW,0                                                        
*                                                                               
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         XC    SVESTRT(12),SVESTRT CLEAR EST DATES                              
         XC    SVEST,SVEST                                                      
         MVI   TESTSW,C'N'         SET IN SVEST+2                               
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   EDTE2                                                            
         CLI   FISCLT,C'?'         CHK FOR ? IN CLIENT                          
         BE    EDTE0                                                            
         CLI   FISPRD,C'?'         CHK FOR Q MARK IN PRD                        
         BNE   EDTE1                                                            
EDTE0    MVI   SVEST+2,C'?'        Q MARK                                       
         B     EDTE10                                                           
*                                                                               
EDTE1    DS    0H                                                               
         MVC   SVEST,8(R2)                                                      
         B     EDTE10                                                           
*                                                                               
EDTE2    CLI   8(R2),C'?'          Q MARK                                       
         BNE   EDTE3                                                            
         MVC   SVEST+2(1),8(R2)                                                 
         CLI   5(R2),1                                                          
         BE    EDTE10                                                           
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         LA    R3,9(R2)                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
EDTE2C   CLI   0(R3),C'0'          CHK FOR NUMERICS                             
         BL    ESTINV                                                           
         CLI   0(R3),C'9'                                                       
         BH    ESTINV                                                           
         MVC   0(1,R4),0(R3)                                                    
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,EDTE2C                                                        
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   SVESTB,HALF                                                      
         B     EDTE10                                                           
         SPACE 2                                                                
EDTE3    DS    0H                                                               
         BAS   RE,PACK                                                          
         STH   R0,HALF                                                          
         MVC   SVESTB,HALF                                                      
*                                                                               
         CLC   SVCLT,=C'ALL'                                                    
         BE    EDTP10                                                           
         CLI   SVCLT,C'*'       OFFICE                                          
         BE    EDTP10                                                           
         CLI   SVCLT,C'$'       OFFICE LIST                                     
         BE    EDTP10                                                           
         CLI   SVCLT,C'&&'      BILLING GROUP                                   
         BE    EDTP10                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(6),SVCLT      CLT AND PRD                                  
         CLC   SVPRD,=C'ALL'                                                    
         BNE   *+10                                                             
         MVC   KEY+7(3),=C'ZZZ'    TRY FOR ZZZ                                  
         BAS   RE,PACK                                                          
         CP    DUB,=P'0'                                                        
         BNE   EDTE5                                                            
ESTINV   LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTE5    STH   R0,HALF                                                          
         MVC   KEY+10(2),HALF                                                   
         MVC   SVESTB,HALF                                                      
         CLI   FISCLT,C'?'         CHK FOR ? IN CLIENT                          
         BE    EDTE6                                                            
         CLI   FISPRD,C'?'         CHK FOR Q MARK IN PRD                        
         BNE   EDTE9                                                            
EDTE6    MVI   SVEST+2,C'?'        SET Q MARK                                   
         B     EDTE10              SKIP EST READ                                
*                                                                               
EDTE9    DS    0H                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BE    EDTE8                                                            
         CLC   SVPRD,=C'ALL'                                                    
         BE    EDTE10                                                           
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTE8    DS    0H                                                               
         LA    R0,ESTREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,GETREC                                                        
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         MVC   SVESTRT,PESTST                                                   
         MVC   SVEEND,PESTEND                                                   
         MVC   FISESTN,PESTNAME                                                 
         TM    PESTTEST,X'80'       SEE IF TEST ESTIMATE                        
         BZ    EDTE12                                                           
         MVI   TESTSW,C'Y'          SET TO PASS TEST ESTS                       
         FOUT  FISETSTH,=C'*TEST*',6                                            
         TM    PESTTEST,X'40'       SEE IF STEWARDSHIP ESTIMATE                 
         BZ    EDTE12                                                           
         FOUT  FISETSTH,=C'*STEW*',6     REPLACE *TEST*                         
         B     EDTE12                                                           
*                                                                               
EDTE10   XC    FISESTN,FISESTN                                                  
EDTE12   FOUT  FISESTNH                                                         
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
EDTST    DS    0H                  EDIT START DATE                              
         LA    R2,FISSTDEH                                                      
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTEND                                                           
         BAS   RE,UNVAL                                                         
         MVI   PRSW,0                                                           
         MVI   SUBSW,0                                                          
         BAS   RE,ANY                                                           
         CLC   8(2,R2),=C'ES'                                                   
         BNE   EDTST5                                                           
         CLI   SVEST+2,C'?'        Q MARK                                       
         BE    EDTST3              EST DATES NOT REQ                            
         CLI   SVESTRT,0           SEE IF I HAVE EST DATES                      
         BNE   EDTST3              YES - OK                                     
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTST3   MVC   SVSTRT,SVESTRT      USE EST DATES                                
         MVC   SVEND,SVEEND                                                     
         MVI   PRSW,1              SET PRSW AND SUBSW FOR ES                    
         MVI   SUBSW,1                                                          
         CLC   FISEDDE(2),=C'ES'                                                
         BE    EDTST4                                                           
         CLI   FISEDDEH+5,0                                                     
         BE    EDTST4                                                           
         LA    R2,FISEDDEH                                                      
         B     DATERR                                                           
*                                                                               
EDTST4   OI    FISEDDEH+4,X'20'                                                 
         B     EDTST10                                                          
*                                                                               
EDTST5   GOTO1 VDATVAL,DMCB,(2,8(R2)),SVSTRT                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   EDTST7                                                           
*                                                                               
DATERR   LA    R3,DTEINV                                                        
         B     ERROR                                                            
*                                                                               
EDTST7   CLC   5(1,R2),DMCB+3                                                   
         BE    EDTST10             NO MORE INPUT                                
         LA    R4,8(R2)                                                         
         ZIC   R5,DMCB+3                                                        
         AR    R4,R5                                                            
         CLC   0(2,R4),=C'-P'                                                   
         BNE   DATERR                                                           
         MVI   PRSW,1                                                           
*                                                                               
EDTST10  OI    4(R2),X'20'                                                      
         EJECT                                                                  
EDTEND   DS    0H                  EDIT END DATE                                
         LA    R2,FISEDDEH                                                      
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTPUB                                                           
         MVI   SUBSW,0             SET OFF SUBSEQUENT SW                        
         BAS   RE,UNVAL                                                         
         MVC   SVEND,SVSTRT        SET END TO START                             
         CLI   5(R2),0             CHK FOR INPUT                                
         BE    EDTEND12            NO                                           
         CLC   8(2,R2),=C'ES'                                                   
         BNE   EDTEND5                                                          
         MVI   SUBSW,1             SET ON SUBSEQUENT SW                         
         MVC   SVEND,SVEEND        USE EST DATE                                 
         CLI   SVEND,0                                                          
         BNE   EDTEND12                                                         
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTEND5  GOTO1 VDATVAL,DMCB,(2,8(R2)),SVEND                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
*                                                                               
EDTEND10 CLC   5(1,R2),DMCB+3                                                   
         BE    EDTEND12            NO MORE INPUT                                
         LA    R4,8(R2)                                                         
         ZIC   R0,DMCB+3                                                        
         AR    R4,R0                                                            
         CLC   0(2,R4),=C'-S'                                                   
         BNE   DATERR                                                           
         MVI   SUBSW,1                                                          
EDTEND12 DS    0H                                                               
         CLC   SVSTRT,SVEND                                                     
         BNH   EDTEND14                                                         
         LA    R3,DATEERR          START AFTER END                              
         B     ERROR                                                            
*                                                                               
*                                                                               
EDTEND14 DS    0H                                                               
EDTEND20 OI    4(R2),X'20'                                                      
         EJECT                                                                  
EDTPUB   DS    0H                  EDIT PUB                                     
         LA    R2,FISPUBH                                                       
         TM    4(R2),X'20'                                                      
         BO    EDTOPT                                                           
**                                                                              
**       NAME SEARCH CALL                                                       
**                                                                              
         SR    R2,RA               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,SVMED                                                   
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         DROP  R3                                                               
         LA    R2,FISPUBH                                                       
*                                                                               
         MVI   PVSW,0              UNVALIDATE PREV VAL SW                       
         XC    PREVKEY,PREVKEY                                                  
         MVI   PREVDSW,0                                                        
         XC    FISPUBN,FISPUBN                                                  
         XC    FISPUBC,FISPUBC                                                  
         BAS   RE,ANY                                                           
         XC    SVPUB,SVPUB                                                      
         MVI   SVPQMK,0                                                         
         MVC   SVPUB(3),8(R2)                                                   
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   EDTPUB1                                                          
*                                                                               
*              PUB CAN'T BE 'ALL' IF CLT AND PRD AND EST  ARE                   
*                                                                               
*        NOTE - MUST CHK INPUT FIELDS NOT SV FIELDS                             
*                                                                               
         CLC   FISCLT(3),=C'ALL'                                                
         BE    EDTPUB0                                                          
         CLI   FISCLT,C'*'         OR OFFICE                                    
         BE    EDTPUB0                                                          
         CLI   FISCLT,C'&&'        OR BILLING GROUP                             
         BE    EDTPUB0                                                          
         CLI   FISCLT,C'$'         OR OFFICE LIST                               
         BNE   EDTPUB0X                                                         
*                                                                               
EDTPUB0  CLC   FISPRD,=C'ALL'                                                   
         BNE   EDTPUB04                                                         
         CLI   SVEST+2,C'?'        QMARK                                        
         BE    EDTPUB0X                                                         
         B     EPUBERR                                                          
EDTPUB04 DS    0H                                                               
         CLI   FISPRD,C'?'                                                      
         BE    EDTPUB0X                                                         
         B     EPUBERR      DISALLOW CLT=ALL,*,$,&                              
*                           WITH ONE PRD AND ALL ESTS/ALL PUBS                  
*          EST MUST BE ONE OR ALL- ALSO AN ERROR                                
*                                                                               
EDTPUB05 B     EPUBERR             NO ALL,ALL,ALL,ALL                           
*                                                                               
EDTPUB0X CLC   SVPRD,=C'ZZZ'       NO ZZZ AND ALL PUBS (NO ZZZ BUCS)            
         BNE   EDTPUB10                                                         
         B     EPUBERR                                                          
*                                                                               
EDTPUB1  DS    0H                                                               
         ZIC   R4,5(R2)                                                         
         LA    R5,8(R2)                                                         
         CLI   SVPUB,C'?'          QUESTION MARK                                
         BNE   EDTPUB2                                                          
         MVI   SVPQMK,C'?'         QUESTION MARK                                
         XC    SVPUB,SVPUB                                                      
         CLI   5(R2),1                                                          
         BE    EDTPUB10                                                         
         CLC   SVPRD,=C'ALL'       PRD CAN'T BE ALL WITH START AT PUB           
         BE    EPUBERR                                                          
         BCTR  R4,0                                                             
         LA    R5,9(R2)                                                         
EDTPUB2  GOTO1 =V(PUBVAL),DMCB,((R4),0(R5)),(0,SVPUB),RR=RELO                   
         CLI   DMCB,X'FF'                                                       
         BNE   EDTPUB3                                                          
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         SH    R0,=H'4'                                                         
         BM    EPUBERR                                                          
         LA    R4,8(R2)                                                         
         AR    R4,R0                                                            
         CLC   0(4,R4),=C',ALL'     CHK FOR ALL ZONES,EDTS                      
         BNE   EPUBERR                                                          
         CLI   SVPQMK,0          NO ALL ZONES,EDTS FOR QMARK                    
         BNE   EPUBERR                                                          
         GOTO1 =V(PUBVAL),DMCB,((R0),8(R2)),(0,SVPUB),RR=RELO                   
         CLI   0(R1),X'FF'                                                      
         BE    EPUBERR                                                          
         OC    SVPUB+4(2),SVPUB+4                                               
         BNZ   EPUBERR                                                          
         MVC   SVPUB+4(2),=X'FFFF'                                              
         B     EDTPUB3                                                          
EPUBERR  LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTPUB3  DS    0H                                                               
         CLI   SVPQMK,C'?'         QMARK                                        
         BE    EDTPUB10            BYPASS PUB READ FOR QMARK                    
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVMED                                                     
         MVC   KEY+1(6),SVPUB                                                   
         LA    R5,6                                                             
         CLI   SVPUB+4,X'FF'       SEE IF DOING ALL PUBS/EDTS                   
         BNE   EDTPUB3C                                                         
EDTPUB3B DS    0H                                                               
         LA    R5,4                                                             
         XC    KEY+5(2),KEY+5                                                   
EDTPUB3C DS    0H                                                               
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
EDTPUB4  BAS   RE,HIGHPUB                                                       
EDTPUB4A EX    R5,*+8                                                           
         BNE   PUBERR                                                           
         CLC   KEY(0),KEYSAVE                                                   
         CLC   KEY+7(3),KEYSAVE+7  CHK AGY                                      
         BE    EDTPUB8             HAVE FOUND A VALID PUB                       
*                                                                               
EDTPUB6  CLI   SVAPROF+16,C'0'                                                  
         BE    EDTPUB6B                                                         
         CLC   KEY+7(2),=C'ZZ'     SEE IF I FOUND DEFAULT                       
         BE    EDTPUB8                                                          
EDTPUB6B BAS   RE,SEQPUB                                                        
         B     EDTPUB4A                                                         
*                                                                               
EDTPUB8  LA    R8,REC                                                           
         ST    R8,APUBIO                                                        
         BAS   RE,GETPUB                                                        
         MVC   FISPUBN,PUBNAME                                                  
         MVC   FISPUBC,PUBCITY                                                  
EDTPUB10 FOUT  FISPUBNH                                                         
         FOUT  FISPUBCH                                                         
         OI    4(R2),X'20'                                                      
         B     EDTOPT                                                           
PUBERR   LA    R3,NOFNDERR         PUB NOT FOUND                                
         B     ERROR                                                            
         EJECT                                                                  
EDTOPT   DS    0H                  EDIT OPTIONAL DATA                           
         LA    R2,FISOPTH                                                       
         MVC   WORK(4),SVREP       SAVE OLD SPECIAL REP                         
         MVI   DOLSW,C'1'                                                       
*                                                                               
         MVI   COS2SW,0            COST2 OR OPEN OPTION SWITCH                  
         MVI   PLNCSTSW,0          INIT PLANNED COST SWITCH                     
*                                                                               
         MVI   MTHSW,C'B'          SET DEFAULTS                                 
         MVI   PNAMESW,C'N'        PUB NAME SW                                  
         XC    SVREP,SVREP         CLEAR SPECIAL REP                            
*                                 (FIRST 4 BYTES OF SVAPROF)                    
         CLI   5(R2),0                                                          
         BE    EDTO15                                                           
*                                 (FIRST 4 BYTES OF SVAPROF)                    
         GOTO1 VSCANNER,DMCB,FISOPTH,(6,SCNWRK)                                 
         CLI   DMCB+4,0                                                         
         BNE   EDTO3                                                            
*                                 (FIRST 4 BYTES OF SVAPROF)                    
OPTERR   LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTO3    ZIC   R8,DMCB+4           SET FOR BCT                                  
         LA    R6,SCNWRK                                                        
*                                                                               
EDTO5    DS    0X                                                               
*                                                                               
         CLC   12(6,R6),=C'PCBILL'  PLANNED COSTS?                              
         BNE   EDTO5N                                                           
*                                                                               
         MVI   PLNCSTSW,C'P'          REPORT PLANNED COSTS                      
*                                                                               
         B     EDTO10                 NEXT FIELD                                
*                                                                               
EDTO5N   DS    0X                                                               
*                                                                               
         CLC   12(4,R6),=C'COS2'   COS2?                                        
         BE    EDTO5C                                                           
         CLC   12(5,R6),=C'COST2'  OR COST2                                     
         BE    EDTO5C                                                           
         CLC   12(4,R6),=C'OPEN'   OPEN?                                        
         BNE   EDTO5M                                                           
EDTO5C   CLC   FISPUB(3),=C'ALL'   PUB IS ALL?                                  
* * * *  BNE   *+8                                                              
* * * *  B     OPTERR              CANNOT HAVE COS2 OR OPEN IF PUB=ALL          
         MVI   COS2SW,C'Y'         TURN ON SWITCH                               
         B     EDTO10              NEXT FIELD                                   
*                                                                               
EDTO5M   CLI   1(R6),0             2 HALF REQUIRED                              
         BE    OPTERR                                                           
         CLI   0(R6),1                                                          
         BE    EDTO6                                                            
         CLC   12(4,R6),=C'NAME'   ALTERNATIVE FOR PUB NAME REQUEST             
         BE    EDTO6               (N=YES OR NO)                                
         CLC   12(4,R6),=C'PNAM'   ALTERNATIVE FOR PUB NAME REQUEST             
         BE    EDTO6               (N=YES OR NO)                                
         CLC   12(4,R6),=C'PUBN'   ALTERNATIVE FOR PUB NAME REQUEST             
         BE    EDTO6               (N=YES OR NO)                                
*                                                                               
         CLI   0(R6),4                                                          
         BNE   OPTERR                                                           
*                                                                               
         CLC   12(4,R6),=C'SREP'                                                
         BNE   OPTERR                                                           
         OC    SVREP,SVREP         SEE IF REP ALREADY INPUT                     
         BNZ   OPTERR                                                           
         TM    3(R6),X'80'         MUST BE NUMERIC                              
         BNO   OPTERR                                                           
         L     R0,8(R6)                                                         
         CH    R0,=H'9999'         MAX REP                                      
         BH    OPTERR                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVREP,DUB                                                        
         B     EDTO10              NEXT FIELD                                   
*                                                                               
EDTO6    LA    R3,TAB1                                                          
         LA    R1,DOLSW                                                         
         CLI   12(R6),C'$'                                                      
         BE    EDTO7                                                            
         LA    R3,TAB2                                                          
         LA    R1,MTHSW                                                         
         CLI   12(R6),C'M'                                                      
         BE    EDTO7                                                            
         LA    R3,TAB3                                                          
         LA    R1,PNAMESW                                                       
         CLC   12(4,R6),=C'NAME'   PUB NAME                                     
         BE    EDTO7                                                            
         CLC   12(4,R6),=C'PNAM'   PUB NAME                                     
         BE    EDTO7                                                            
         CLC   12(4,R6),=C'PUBN'   PUB NAME                                     
         BE    EDTO7                                                            
         CLI   12(R6),C'N'         N=    ALSO PUB NAME                          
         BNE   OPTERR                                                           
*                                                                               
EDTO7    ZIC   R5,1(R6)                                                         
         BCTR  R5,0                                                             
EDTO8    CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    OPTERR                                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),22(R6)                                                   
         BE    EDTO9                                                            
         LA    R3,10(R3)                                                        
         B     EDTO8                                                            
*                                                                               
EDTO9    MVC   0(1,R1),9(R3)       SAVE CODE                                    
EDTO10   LA    R6,32(R6)           NEXT SCANNER LINE                            
         BCT   R8,EDTO5                                                         
*                                                                               
*                                                                               
EDTO15   CLI   SVEST+2,C'?'        SEE IF DOING PRD/EST DISPLAY                 
         BE    EDTO18                                                           
         CLI   FISCLT,C'?'         OR CLT/PRD/EST - QMARK                       
         BE    EDTO18                                                           
         CLI   FISPRD,C'?'         QMARK                                        
         BNE   EDTX                                                             
EDTO18   CLI   SVPQMK,C'?'         PUB CAN'T BE QMARK IF CLT                    
*                                  PRD OR EST IS                                
         BNE   EDTO19                                                           
         LA    R2,FISPUBH                                                       
         B     EPUBERR                                                          
*                                                                               
EDTO19   CLC   LDOLSW,DOLSW        SEE IF SAME DOLSW                            
         BNE   EDTX                                                             
         MVI   PVSW,0              SO I'LL CONTINUE PED/EST DISPLAY             
*                                                                               
EDTX     DS    0H                  GOTO 01 PHASE TO PROCESS DATA                
         CLI   SVSTRT,0            MAY NOT HAVE DATES FOR EST DIS               
         BE    EDTX5                                                            
         GOTO1 VDATCON,DMCB,(0,SVSTRT),(3,SVSTRTB)                              
         GOTO1 VDATCON,DMCB,(0,SVEND),(3,SVENDB)                                
EDTX5    XC    DMCB(4),DMCB                                                     
         MVI   DMCB,2                                                           
         CLI   SVPQMK,C'?'         Q MARK - PUB DISPLAY                         
         BE    EDTX25                                                           
         MVI   DMCB,3              EST DISPLAY                                  
         CLI   SVEST+2,C'?'        Q MARK -                                     
         BE    EDTX8                                                            
         CLI   FISPRD,C'?'         QMARK                                        
         BE    EDTX8                                                            
         CLI   FISCLT,C'?'         QMARK                                        
         BE    EDTX8                                                            
         B     EDTX20              NORMAL BUY OR BUCKET DISPLAY                 
*                                                                               
EDTX8    CLC   FISPUB(3),=C'ALL'   PRD/EST DISPLAY - BUCKETS                    
         BE    EDTX25                                                           
EDTX10   MVI   DMCB,4              NEW CLT/PRD/EST DISPLAY - BUYS               
         CLC   FISPUB(3),=C'ALL'   MUST BE ONE PUB                              
         BNE   EDTX25                                                           
         LA    R2,FISPUBH                                                       
         B     EPUBERR             INVALID                                      
*                                                                               
EDTX20   MVI   DMCB,1              $ DISPLAY                                    
         CLC   LCOS2SW,COS2SW      SEE IF COS2 OPTION CHANGED                   
         BE    *+8                                                              
         MVI   PVSW,0              SET NOT PREVIOUSLY VALIDATED                 
*                                                                               
*                                  MONTH DISPLAY SO LIMIT TO 13 MTHS            
*                                                                               
         LA    R5,6                          CONVERT TO BINARY AT TEMP          
         LA    R6,SVSTRT                                                        
         LA    R7,TEMP                                                          
*                                                                               
VALMAX1  CLI   0(R6),X'F9'                                                      
         BNH   VALMAX3                                                          
         PACK  DUB,1(1,R6)    FIRST JUST DO SECOND DIGIT OF YEAR                
         MVI   DUB+6,X'10'                                                      
         CLI   0(R6),X'FA'    2000'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'11'                                                      
         CLI   0(R6),X'FB'    2010'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'12'                                                      
         CLI   0(R6),X'FC'    2020'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'13'                                                      
         CLI   0(R6),X'FD'    2030'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'14'                                                      
         CLI   0(R6),X'FE'    2040'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'15'    MUST BE X'FF' - 2050'S                            
         B     VALMAX3X                                                         
*                                                                               
VALMAX3  PACK  DUB,0(2,R6)                                                      
VALMAX3X CVB   R0,DUB                                                           
         STC   R0,0(R7)                                                         
         LA    R6,2(R6)                                                         
         LA    R7,1(R7)                                                         
         BCT   R5,VALMAX1                                                       
*                                                                               
         SR    R5,R5                         CALCULATE DELTA MONTHS             
         SR    R6,R6                                                            
         IC    R5,TEMP+1                                                        
         IC    R6,TEMP+4                                                        
         SR    R6,R5                         R6=(END-STR) MONTH                 
         SR    R7,R7                                                            
         IC    R5,TEMP                                                          
         IC    R7,TEMP+3                                                        
         SR    R7,R5                         R7=(END-STR) YEAR                  
         BM    VALMAXE                                                          
         LR    R5,R6                                                            
         LA    R6,12                                                            
         MR    R6,R6                         R7=(END-STR) YEAR * 12             
         AR    R5,R7                                                            
         STC   R5,WORK                                                          
         CLI   WORK,X'0D'                                                       
         BL    VALMAXX                                                          
VALMAXE  LA    R3,SEDBIG           MAX 13 MTHS                                  
         LA    R2,FISEDDEH         CURSOR TO END DATE                           
         B     ERROR                                                            
*                                                                               
VALMAXX  DS    0H                                                               
         CLC   WORK(4),SVREP        SEE IF SPECIAL REP CHANGED                  
         BE    VALMAXZ                                                          
         MVI   PVSW,0              SET OFF PREV VALIDATED SWITCH                
*                                                                               
VALMAXZ  OC    SVREP,SVREP         SEE IF SPECIAL REP SPECIFIED                 
         BZ    CALLOL              NO                                           
         CLC   FISPUB(3),=C'ALL'                                                
         BNE   CALLOL                                                           
         LA    R2,FISOPTH          SPECIAL REP ONLY FOR ONE PUB                 
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
*                                                                               
EDTX25   OC    SVREP,SVREP                                                      
         BZ    CALLOL                                                           
         LA    R2,FISOPTH          SPECIAL REP NOT ALLOWED                      
         LA    R3,FLDINV           FOR OVERLAYS 2,3,4                           
         B     ERROR                                                            
*                                                                               
CALLOL   DS    0H                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         MVC   LCOS2SW,COS2SW         SAVE ON RETURN                            
         XMOD1 1                                                                
         EJECT                                                                  
TAB1     DC    CL9'GROSS',C'1'                                                  
         DC    CL9'NET',C'3'                                                    
         DC    CL9'GROSS-CD',C'2'                                               
         DC    CL9'G-CD',C'2'                                                   
         DC    CL9'GLCD',C'2'                                                   
         DC    CL9'NET-CD',C'4'                                                 
         DC    CL9'N-CD',C'4'                                                   
         DC    CL9'NLCD',C'4'                                                   
         DC    CL9'NET/NET',C'4'                                                
         DC    CL9'CDISCOUNT',C'5'                                              
         DC    CL9'CASH DISC',C'5'                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
TAB2     DC    CL9'BILLING',C'B'                                                
         DC    CL9'INSERTION',C'I'                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
TAB3     DC    CL9'YES',C'Y'                                                    
         DC    CL9'NO',C'N'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
UNVAL    NTR1                                                                   
         MVI   PVSW,0              CANCEL PREV VAL SW                           
         LA    R4,FISOPTH                                                       
UNV2     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R4                                                            
         BH    UNVX                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    UNV2                YES - SKIP                                   
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     UNV2                                                             
*                                                                               
UNVX     XIT1                                                                   
         EJECT                                                                  
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
*        GO HERE WHEN LIMIT ACCESS IS BY OFFICE LIST                            
*        6(RA) = $N                                                             
*                                                                               
*NOP*PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                      
         SPACE 2                                                                
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)          LIMIT ACCESS                              
         MVC   OFCLMT,6(RA)           LIMIT ACCESS                              
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK2),(0,ACOMFACS)                          
         CLI   0(R1),0                                                          
         XIT1                                                                   
*******************************************************                         
         EJECT                                                                  
*                                                                               
PPCLIVER NTR1                   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF     CLT OR CLT TRAFFIC OFFICE CODE                
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT,6(RA)                                                     
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD         A("SECRET BLOCK")                             
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),(0,ACOMFACS)                          
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENEROL                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         L     R0,AREC                                                          
         LHI   R1,1600                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AREC                                                          
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPFISWRK                                                       

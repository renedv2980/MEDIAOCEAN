*          DATA SET DDOFFICER  AT LEVEL 030 AS OF 11/27/18                      
*PHASE T00A38A                                                                  
*INCLUDE LISTIO                                                                 
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-12580  05/03/17 ADD SUPPORT FOR NET CLIENT GROUPS         *         
***********************************************************************         
* 30NOV16 - SUPPORT TWO CHAR MEDIA OFFICE LISTS                                 
* 16MAR11 - SUPPORT FOR DATA ACCESS                                             
* 25OCT05 - DO NOT READ MEDIA 'N' PROFILE RECORDS                               
* 12JUL05 - SUPPORT TWO CHAR MEDIA OFFICE RECORDS                               
* 21NOV03 - SUPPORT B PROFILE FOR SPOTPAK                                       
***********************************************************************         
*                                                                               
* PARAMETERS ARE AS FOLLOWS                                                     
*                                                                               
* P1  BYTE  0     ON ENTRY C'N'  = NEW VALIDATION CALL                          
*                 ON ENTRY C'2'  = NEW VALIDATION CALL WITH OFFICE              
*                                  VALIDATION/CONVERSION/DATA RETURN            
*                                  TWO CHAR OFFICES AND OFFICE LISTS            
*                                                                               
*                 ON EXIT, X'00' = AUTHORIZED                                   
*                          X'FF' = NOT AUTHORIZED                               
*                                                                               
*     BYTE 1-3    A(8 BYTE AREA) (NOTE: FOR ACCPAK IT'S 10 BYTES)               
*                                (NOTE: C'N' CALL USES DDOFFICED)               
*                                (NOTE: C'2' CALL USES DDOFFICED)               
*                 SYSTEM ID          (1)                                        
*                 X'00'                                                         
*                 ID AUTH VALUE      (2)                                        
*                 X'00'                                                         
*                 AGENCY CODE        (2)                                        
*                 CLIENT OFFICE CODE (1)   TO BE VALIDATED                      
*                 ALPHA AGENCY       (2)   ACCPAK ONLY                          
*                                                                               
*                                                                               
* P2  BYTE 0      X'80' = P3 IS A(32 BYTE LIST)                                 
*                 X'C0' = P3 IS A(48 BYTE LIST)                                 
*                 X'D0' = P3 IS A(128 BYTE LIST)                                
*                                                                               
*                 USED WITH P1 BYTE 0 = C'2'                                    
*                 X'E0' = P3 IS A(512 BYTE INDEXED LIST)                        
*                 X'F0' = P3 IS A(2560 BYTE INDEXED LIST W/NAMES)               
*                 X'01' = P3 IS A(RETURN DATA) - DO NOT USE WITH LISTS          
*                                                                               
*     BYTE 1-3    A(COMFACS)                                                    
*                                                                               
*                                                                               
* P3  BYTE 0      IF P1 BYTE 0 = C'2' AND P2 BYTE 0 = X'01'                     
*                  C'S'= P3 IS A( 8 BYTE AREA= OFFICE SHORT NAME)               
*                  C'L'= P3 IS A(20 BYTE AREA= OFFICE LONG NAME)                
*                  C'W'= P3 IS A(40 BYTE AREA= WEBDAV ID(20) PSSWRD(20)         
*                                                                               
*     BYTE 1-3    A(LIST AREA OR ZERO TO READ CTFILE)                           
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'OFFICER - TEST LIMIT ACCESS VALUES'                             
OFFICER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*OFFICER,RR=RE                                       
         USING WORKD,RC                                                         
*                                                                               
         LM    R2,R4,0(R1)         GET A(OFFICED/COMFACS/SAVEAREA)              
         ST    RE,RELO                                                          
*                                                                               
         MVI   INFLAG,0            INIT INTERNAL FLAG                           
*                                                                               
         USING OFFICED,R2                                                       
         USING COMFACSD,R3                                                      
*                                                                               
         LR    RA,R1               SAVE PARMS ADDRESS                           
         USING PARMSD,RA                                                        
*                                                                               
         MVC   CALLTYPE,P1         SAVE CALL TYPE YOU IDIOT                     
         MVI   P1,X'FF'            PRESET FOR NO AUTH                           
*                                                                               
         CLI   OFCLMT,C'='         DATA ACCESS                                  
         BE    VAL010                                                           
         CLI   OFCAUTH,C'='        DATA ACCESS?                                 
         BE    VAL010                                                           
         CLI   CALLTYPE,C'2'       NEW CALL, W/ 2 BYTE OFFICES                  
         BE    VAL010                                                           
         CLI   CALLTYPE,C'N'       TEST NEW CALL                                
         BE    VAL050                                                           
*                                                                               
         OI    INFLAG,INNOOFFD     INDICATE NO OFFICED AVAILABLE                
         BAS   RE,VALOFL           OLD CALL - VALIDATE OFCLIST                  
         B     EXIT                                                             
*                                                                               
VAL010   TM    OFCINDS,OFCIMOLC    HERE FOR AN OFFICE LIST CONVERSION           
         BZ    VAL015              NO: SKIP IT                                  
         BRAS  RE,SETOFL           SET OFFICE LIST VALUES                       
         BNE   EXIT                                                             
         MVI   P1,0                SET AUTHORIZATION CONDITION CODE             
         B     EXIT                                                             
*                                                                               
VAL015   BRAS  RE,FSYSLA           FIND SYSTEM # AND SECURITY LIMIT             
*                                                                               
         TM    SEATYPE,SEADACQ     DATA ACCESS                                  
         BO    VAL600              . YES                                        
*                                                                               
         TM    P2,X'E0'            USING AN INDEXED LIST?                       
         BNO   VAL020              . NO                                         
         CLI   0(R4),X'FF'         TEST ALREADY HAVE LIST?                      
         BE    VAL020              . YES                                        
         BRAS  RE,BLDOFT           BUILD OFFICE TABLE/LIST                      
*                                                                               
VAL020   NI    OFCINDS,X'FF'-OFCIOINV TURN OFF INVALID OFFICE                   
         TM    SEATYPE,SEAOFFQ     TEST OFFICE SECURITY                         
         BO    VAL050              . YES                                        
         TM    SEATYPE,SEAOLIQ     TEST OFFICE LIST SECURITY                    
         BO    VAL050              . YES                                        
*                                                                               
         CLI   OFCACCSC,C' '       USING CLIENT ACCESS LIST                     
         BH    VAL050              . YES                                        
         CLI   OFCOFC,0            TEST ONE BYTE OFFICE                         
         BNE   VAL030              . YES                                        
         CLC   OFCOFC2,SPACES      TEST TWO CHARACTER OFFICE                    
         BNH   VAL050              . NO, NO OFFICE CONVERSION NEEDED            
VAL030   BRAS  RE,VALMOF           VALIDATE USING MEDIA OFFICE RECORDS          
         BL    EXIT                . INVALID OFFICE                             
*                                                                               
VAL050   OC    OFCLMT(2),OFCLMT    TEST ANY SECURITY LIMIT                      
         BZ    VALOK                                                            
*                                                                               
         CLI   OFCACCSC,X'FF'      TEST IGNORE CLIENT SECURITY                  
         BNE   VAL060                                                           
         CLI   OFCLMT,C'+'         TEST MKT LIST SECURITY                       
         BE    VAL400                                                           
         CLI   OFCLMT+2,C'+'                                                    
         BE    VAL400                                                           
         B     VALOK                                                            
*                                                                               
VAL060   CLI   OFCLMT,C'+'         TEST MARKET LIST SECURITY                    
         BE    VAL400                                                           
         CLI   OFCLMT,C'*'         TEST OFFICE SECURITY                         
         BE    VAL200                                                           
         CLI   OFCLMT,C'$'         TEST OFFICE LIST SECURITY                    
         BE    VAL500                                                           
*                                                                               
         CLI   OFCLMT+2,C'+'       ALT LOC FOR MKT SECURITY                     
         BE    VAL400                                                           
*                                                                               
         CLI   OFCSYS,C'S'         TEST SPOT                                    
         BE    VAL100                                                           
         CLI   OFCSYS,C'N'         TEST NET                                     
         BE    VAL100                                                           
         CLI   OFCSYS,C'P'         TEST PRINT                                   
         BE    VAL100                                                           
         B     EXIT                EXIT NOT AUTHORIZED                          
                                                                                
*======================================================================         
* CLIENT SECURITY                                                               
*======================================================================         
VAL100   CLC   OFCLMT(3),OFCCLT    MATCH CLIENT                                 
         BE    VALOK               EXIT AUTHORIZED                              
         CLI   OFCSYS,C'S'         TEST SPOT                                    
         BE    VAL110                                                           
         CLI   OFCSYS,C'N'         TEST NET                                     
         BE    VAL110                                                           
         CLI   OFCSYS,C'P'         TEST PRINT                                   
         BE    VAL140                                                           
         DC    H'0'                                                             
*----------------------------------                                             
* TRY TO MATCH TO PACKED CLIENT CODE FOR NET/SPOT                               
*----------------------------------                                             
VAL110   TM    OFCINDS,OFCI2CSC    TEST 2 CHARACTER CLIENT CODE PASSED          
         BZ    *+14                NO - CLPACK THE 3 CHARACTER ONE              
         MVC   DUB(L'OFCCLT2),OFCCLT2                                           
         B     VAL120                                                           
         GOTO1 CCALLOV,DMCB,0,X'D9000A14'                                       
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,OFCCLT,DUB                                             
*                                                                               
VAL120   CLC   OFCLMT(2),DUB       MATCH CLIENT                                 
         BE    VALOK                                                            
*----------------------------------                                             
* SPOT/NET = CHECK CLIENT LIST IN SECBLK IF PRESENT                             
*----------------------------------                                             
         ICM   R4,15,OFCSECD                                                    
         BZ    EXIT                                                             
         USING SECD,R4                                                          
         LA    R1,SECCLAL          POINT TO ACCESS LIST                         
         LA    R0,L'SECCLAL/3      MAX ENTRIES                                  
         DROP  R4                                                               
*                                                                               
VAL130   CLC   0(2,R1),DUB         MATCH PACKED CLIENT CODE                     
         BE    VALOK                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,VAL130                                                        
         B     EXIT                                                             
*----------------------------------                                             
* PRINT - CHECK CLIENT LIST IN SECBLK IF PRESENT                                
*----------------------------------                                             
VAL140   ICM   R4,15,OFCSECD                                                    
         BZ    EXIT                                                             
         USING SECD,R4                                                          
         LA    R1,SECCLAL          POINT TO ACCESS LIST                         
         LA    R0,L'SECCLAL/3      MAX ENTRIES                                  
         DROP  R4                                                               
*                                                                               
VAL150   CLC   0(3,R1),OFCCLT      MATCH  CLIENT CODE                           
         BE    VALOK                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,VAL150                                                        
         B     EXIT                                                             
                                                                                
*======================================================================         
* LIMIT ACCESS OF *A MEANS OFFICE A                                             
* LIMIT ACCESS OF *XPP MEANS CLIENT GROUP (HEX CODE/TWO BYTE PWOS)              
* LIMIT ACCESS OF +X MEANS MATCH TO MKTREC LIMIT ACCESS (SPOT)                  
*======================================================================         
VAL200   CLI   OFCLMT+1,C'*'       TWO CHARACTER OFFICE VALUE                   
         BE    VAL300                                                           
         OC    OFCLMT+2(2),OFCLMT+2 ZEROES MEAN ONE BYTE OFFICE                 
         BZ    VAL300                                                           
         CLC   OFCLMT+2(2),SPACES   SPACES MEAN ONE BYTE OFFICE                 
         BE    VAL300                                                           
*                                                                               
         CLI   OFCSYS,C'S'         TEST SPOT                                    
         BNE   *+12                                                             
         BRAS  RE,VALCGS           VALIDATE SPOT/NET CLTGRP                     
         B     EXIT                                                             
*                                                                               
         CLI   OFCSYS,C'N'         TEST NET                                     
         BNE   *+12                                                             
         BRAS  RE,VALCGS                                                        
         B     EXIT                                                             
*                                                                               
         CLI   OFCSYS,C'P'         TEST PRINT                                   
         BNE   *+12                                                             
         BRAS  RE,VALCGP                                                        
         B     EXIT                                                             
*                                                                               
         DC    H'0' ??????                                                      
                                                                                
*======================================================================         
* MATCH OFFICE                                                                  
*======================================================================         
VAL300   CLI   CALLTYPE,C'2'       TWO CHAR OFFICE CALL?                        
         BNE   VAL310              . NO                                         
*                                                                               
         CLI   OFCACCSC,C' '       CLIENT ACCESS LIST?                          
         BH    VAL310              . YES                                        
         BRAS  RE,VALMOF           VALIDATE USING MEDIA OFFICE RECORDS          
         BL    EXIT                . OFFICE INVALID                             
         TM    P2,X'E0'            USING INDEXED TABLE?                         
         BO    VAL340              . YES, THEN GOOD                             
*                                                                               
VAL310   LA    R1,OFCACCSC         USE CLIENT ACCESS LIST                       
         LA    R0,3                                                             
         CLI   0(R1),C' '          UNLESS IT ISN'T THERE                        
         BH    *+12                                                             
         LA    R1,OFCOFC                                                        
         LA    R0,1                                                             
*                                                                               
VAL320   CLC   OFCLMT+1(1),0(R1)   TEST RIGHT OFFICE                            
         BE    VAL340                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VAL320                                                        
         B     EXIT                EXIT - NOT AUTHORIZED                        
*                                                                               
VAL340   CLI   OFCLMT+2,C'+'       TEST MKT SECURITY TOO                        
         BNE   VALOK               NO - EXIT AUTHORIZED                         
         B     VAL400                                                           
                                                                                
*======================================================================         
* LIMIT ACCESS OF +X MEANS X MUST APPEAR IN MARKET LIST                         
*              OR *A+X                                                          
*======================================================================         
VAL400   CLI   OFCACCSM,X'FF'      TEST IGNORE MARKET ACCESS                    
         BE    VALOK               YES - EXIT AUTH                              
*                                                                               
         LA    R1,OFCACCSM         USE ACCESS LIST                              
         LA    R0,3                                                             
         LA    RF,OFCLMT           POINT TO NORMAL LOCATION                     
         CLI   0(RF),C'+'                                                       
         BE    VAL420                                                           
         LA    RF,OFCLMT+2                                                      
*                                                                               
VAL420   CLC   0(1,R1),1(RF)       TEST RIGHT OFFICE                            
         BE    VALOK                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VAL420                                                        
         B     EXIT                EXIT - NOT AUTHORIZED                        
                                                                                
*======================================================================         
* LIMIT ACCESS OF $X MEANS VALIDATE AGAINST OFFICE LIST                         
*======================================================================         
VAL500   CLI   CALLTYPE,C'2'                                                    
         BNE   VAL560                                                           
*                                                                               
         CLI   OFCACCSC,C' '       USING CLIENT ACCESS LIST                     
         BH    VAL560                                                           
         BRAS  RE,VALMOF           VALIDATE USING MEDIA OFFICE RECORDS          
         BL    EXIT                  OFFICE INVALID                             
         TM    P2,X'E0'             USING INDEXED TABLE?                        
         BO    VAL580                                                           
*                                                                               
VAL560   BAS   RE,VALOFL                                                        
         CLI   P1,0                TEST AUTHORIZED                              
         BNE   EXIT                                                             
VAL580   CLI   OFCLMT+2,C'+'       TEST MKT SECURITY TOO                        
         BNE   EXIT                NO - EXIT AUTHORIZED                         
         MVI   P1,X'FF'            SET UNAUTHORIZED                             
         B     VAL400              AND GO VALIDATE MARKET ACCESS                
                                                                                
*======================================================================         
* LIMIT ACCESS OF ==XX MEANS DATA ACCESS                                        
*======================================================================         
VAL600   BRAS  RE,VALDAC                                                        
         BE    VALOK                                                            
         MVI   P1,X'FF'            SET UNAUTHORIZED                             
         B     EXIT                                                             
*                                                                               
VALOK    MVI   P1,0                SET ACCESS AUTHORIZED                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* VALIDATE OFFICE LIST ACCESS                                                   
***********************************************************************         
R        USING CTUREC,IO                                                        
VALOFL   NTR1                                                                   
         BRAS  RE,SETOFL                                                        
         BNE   EXITX                                                            
*                                                                               
         LA    R5,IO               R5 POINTS TO PROFILE RECORD                  
*                                                                               
         MVI   PASS,0              SET FIRST PASS SWITCH                        
         L     R4,P3               P3=ADDRESS OF LIST BUFFER                    
         TM    P2,X'80'                                                         
         BZ    OFL10               DON'T PASS LIST                              
         LHI   R0,32               YES - WE HAVE BOTH PROFILES                  
         TM    P2,X'C0'            TEST 48 BYTE LIST PASSED                     
         BNO   *+8                                                              
         LHI   R0,48                                                            
         TM    P2,X'D0'            TEST 128 BYTE LIST PASSED                    
         BNO   *+8                                                              
         LHI   R0,128                                                           
         STC   R0,INBYTE           LENGTH OF CALLER'S LIST BUFFER               
         OC    0(32,R4),0(R4)      TEST ALREADY HAVE LIST                       
         BZ    OFL10               NO                                           
         MVI   PASS,X'FF'                                                       
         B     OFL060                                                           
*                                                                               
OFL10    XC    IO(256),IO                                                       
         MVI   R.CTUKTYP,C'U'                                                   
         MVC   R.CTUKSYS,OFCSYS    SYSTEM                                       
         MVI   R.CTUKPROG+1,C'$'   OFFICE LIST                                  
         MVC   R.CTUKPROG+2(1),SVOFLST                                          
         MVC   R.CTUKAGY,OFCAGY    AGENCY                                       
*                                                                               
         CLI   OFCSYS,C'N'         NETWORK SYSTEM?                              
         BNE   *+8                                                              
         MVI   R.CTUKSYS,C'S'      NET USES SPOT PROFILES                       
*                                                                               
         CLI   PASS,0              FIRST PASS?                                  
         BE    OFL20               YES: GO GET RECORD                           
*                                                                               
         MVI   R.CTUKPROG,C'$'          READ PROFILE                            
         MVC   R.CTUKPROG+1(1),SVOFLST  OFFICE LIST VALUE                       
         MVC   R.CTUKPROG+2(1),PASS     PASS LETTER FOR PROFILE RECORDS         
         CLI   0(R5),X'72'         CHECK POINTING AT FIRST PROFILE              
         BNE   OFL20               NO: GET ADDITIONAL RECORD                    
         LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),X'72'         CHECK SECOND PROFILE ELEMENT                 
         BE    OFL22                                                            
*                                                                               
OFL20    CLI   R.CTUKSYS,C'A'      FOR ACCPAK                                   
         BNE   *+10                                                             
         MVC   R.CTUKAGY,OFCALPHA  USE ALPHA ID                                 
*                                                                               
         MVC   KEY,IO                                                           
         GOTO1 CDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                   
         CLI   8(R1),0             EXIT IF NOT FOUND                            
         BNE   EXITX                                                            
*                                                                               
         LA    R5,IO                                                            
         LA    R5,CTUDATA-CTUREC(,R5)  R5 NOW POINTS TO ELEMENT                 
OFL21    CLI   0(R5),0             END OF RECORD                                
         BE    OFL100              DONE                                         
         CLI   0(R5),CTPVELQ       GET X'72' PROFILE ELEMENT                    
         BE    OFL22                                                            
         CLI   0(R5),CTOFELQ       GET X'75' MEDIA OFFICE LIST ELEMENT          
         BE    OFL33                                                            
         LLC   RF,1(R5)            GET NEXT ELEMENT                             
         AR    R5,RF                                                            
         B     OFL21                                                            
*                                                                               
OFL22    TM    P2,X'80'            TEST IF CALLER WANTS LIST                    
         BO    OFL23               YES                                          
         LA    R4,CTPVALUE-CTPVD(R5) ELSE, POINT TO THE LIST                    
         B     OFL30                                                            
*                                                                               
OFL23    CLI   PASS,0              TEST FIRST TIME                              
         BE    OFL27               YES                                          
*                                                                               
         L     R4,P3               NO - RETURN 'A' LIST                         
         LA    R4,16(R4)                                                        
         CLI   PASS,C'A'           ALWAYS HAVE ROOM FOR 32 BYTES                
         BE    OFL27                                                            
         LA    R4,16(R4)           POINT TO WHERE IT COULD GO                   
         CLI   PASS,C'B'                                                        
         BNE   OFL24                                                            
         TM    P2,X'C0'            NOW TEST IT'S A 48 BYTE AREA                 
         BO    OFL27               YES - PROCEED                                
         B     OFL26                                                            
OFL24    TM    P2,X'D0'            TEST IT'S AN 128 BYTE AREA                   
         BNO   OFL26               NO: CAN'T PASS BACK TABLE                    
         LA    R4,16(R6)                                                        
         CLI   PASS,C'C'                                                        
         BE    OFL27                                                            
         LA    R4,16(R6)                                                        
         CLI   PASS,C'D'                                                        
         BE    OFL27                                                            
         DC    H'0'                                                             
*                                                                               
OFL26    L     R4,P3               ELSE CLEAR ENTIRE LIST                       
         XC    0(32,R4),0(R4)                                                   
         LA    R4,CTPVALUE-CTPVD(R5) AND POINT TO LIST IN RECORD                
         B     OFL30                                                            
*                                                                               
OFL27    MVC   0(16,R4),CTPVALUE-CTPVD(R5)                                      
*                                                                               
OFL30    LHI   R0,16                                                            
         B     OFL060                                                           
*                                                                               
OFL33    LLC   RF,1(R5)                                                         
         AHI   RF,-(CTPVALUE-CTPVD) REMOVE ELEMENT OVERHEAD                     
         MVI   PASS,X'FF'          ONLY ONE PASS FOR NEW LIST FORMAT            
*                                                                               
         TM    P2,X'80'            CALLER WANTS THE LIST?                       
         BNO   OFL050              NO                                           
         L     R4,P3               RETURN LIST                                  
         LLC   R0,INBYTE           CALLER'S PASSED LIST BUFFER LENGTH           
         CR    RF,R0               DID WE GET A BIG ENOUGH BUFFER?              
         BH    OFL040              NO: ACTUAL LIST BIGGER THAN BUFFER           
         LR    R0,RF               COPY THIS MANY OFFICES                       
         AHI   RF,-1               YES: COPY LIST TO BUFFER                     
         EX    RF,*+8              OFFICE LIST WILL FIT                         
         B     *+10                                                             
         MVC   0(0,R4),CTPVALUE-CTPVD(R5)                                       
         AHI   RF,1                ADD BACK THE ONE FOR THE EXECUTE             
         B     OFL060                                                           
*                                                                               
OFL040   XC    0(32,R4),0(R4)                                                   
*                                                                               
OFL050   LA    R4,CTPVALUE-CTPVD(R5) POINT TO THE LIST                          
         LR    R0,RF                 LENGTH OF OFFICE DATA                      
*                                                                               
OFL060   CLI   CALLTYPE,C'2'       NEW CALL, W/ TWO CHAR OFFICES                
         BE    *+12                                                             
         CLI   CALLTYPE,C'N'       TEST NEW STYLE CALL                          
         BNE   OFL070              IF NOT, NO ACCESS LIST                       
*                                                                               
         LA    RE,OFCACCSC         USE CLIENT ACCESS LIST                       
         LHI   RF,3                                                             
         CLI   0(RE),C' '          UNLESS IT ISN'T THERE                        
         BH    *+12                                                             
OFL070   LA    RE,OFCOFC                                                        
         LHI   RF,1                                                             
*                                                                               
         CLI   0(R4),0             NEVER MATCH TO X'00'                         
         BE    OFL090                                                           
*                                                                               
OFL080   CLC   0(1,R4),0(RE)       MATCH VALUE                                  
         BE    OKEXIT                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,OFL080                                                        
*                                                                               
OFL090   LA    R4,1(R4)                                                         
         BCT   R0,OFL060                                                        
*                                                                               
OFL100   CLI   0(R5),CTOFELQ       MEDIA OFFICE LIST ELEMENT?                   
         BE    EXITX               YES: THEN WE ARE DONE                        
*                                                                               
         CLI   PASS,0              TEST FIRST TIME                              
         BNE   *+12                NO - LEAVE                                   
         MVI   PASS,C'A'           READ 'A' PROFILE                             
         B     OFL10                                                            
         CLI   PASS,C'A'           TEST READING 'A' PROFILE                     
         BNE   *+12                                                             
         MVI   PASS,C'B'           SET TO READ 'B' PROFILE                      
         B     OFL10                                                            
         CLI   PASS,C'B'           TEST READING 'B' PROFILE                     
         BNE   *+12                                                             
         MVI   PASS,C'C'           SET TO READ 'C' PROFILE                      
         B     OFL10                                                            
         CLI   PASS,C'C'           TEST READING 'C' PROFILE                     
         BNE   EXITX                                                            
         MVI   PASS,C'D'           SET TO READ 'D' PROFILE                      
         B     OFL10                                                            
*                                                                               
OKEXIT   MVI   P1,0                SET VALID AUTH                               
         CLI   PASS,0              TEST FIRST TIME                              
         BNE   OKEXITA             NO                                           
         TM    P2,X'80'            TEST USER WANTS LIST                         
         BNO   EXITX               NO                                           
         MVI   PASS,C'A'           SET PASS FOR PASS 2                          
         B     OFL10                                                            
*                                                                               
OKEXITA  CLI   PASS,C'A'           TEST THIS IS PASS 2                          
         BNE   OKEXITB                                                          
         TM    P2,X'C0'            TEST USER WANTS 48 BYTE LIST                 
         BNO   EXITX                                                            
         MVI   PASS,C'B'           SET FOR PASS 3                               
         B     OFL10                                                            
*                                                                               
OKEXITB  CLI   PASS,C'B'           TEST THIS IS PASS 2                          
         BNE   OKEXITC                                                          
         TM    P2,X'D0'            TEST USER WANTS 128 BYTE LIST                
         BNO   EXITX                                                            
         MVI   PASS,C'C'           SET FOR PASS 3                               
         B     OFL10                                                            
*                                                                               
OKEXITC  CLI   PASS,C'C'           TEST THIS IS PASS 2                          
         BNE   EXITX                                                            
         TM    P2,X'D0'            TEST USER WANTS 128 BYTE LIST                
         BNO   EXITX                                                            
         MVI   PASS,C'D'           SET FOR PASS 3                               
         B     OFL10                                                            
         DROP  R                                                                
                                                                                
***********************************************************************         
* SET MEDIA OFFICE LIST VALUES                                                  
***********************************************************************         
SETOFL   NTR1                                                                   
*                                                                               
         XC    SVOFLST,SVOFLST                                                  
         XC    SVOFLST2,SVOFLST2                                                
*                                                                               
         TM    INFLAG,INNOOFFD     OFFICED UNAVAILABLE?                         
         BZ    SO005               WE HAVE OFFICED, PROCESS ACCORDINGLY         
         CLI   OFCAUTH,C'$'        IS THIS OFFICE LIMIT ACCESS                  
         BNE   SONEX               NO: SHOULDN'T BE HERE                        
         MVC   SVOFLST,OFCAUTH+1   ONE BYTE OFFICE LIST                         
         B     SO050               GET TWO BYTE VALUE                           
*                                                                               
SO005    MVC   SVOFLST,OFCMOL      SAVE OFFICE LIST VALUES INTERNALLY           
         MVC   SVOFLST2,OFCMOL2                                                 
*                                                                               
         TM    OFCINDS,OFCIMOLC    OFFICE LIST FOR CONVERSION                   
         BZ    SO010               NO: THEN USE LIMIT ACCESS FIELD              
         CLI   SVOFLST,0           DO WE HAVE A ONE BYTE OFFICE LIST?           
         BNE   SO050               YES: GET TWO BYTE VALUE                      
         B     SO020               NO: USE 2 BYTE VALUE TO GET 1 BYTE           
*                                                                               
SO010    OC    OFCLMT,OFCLMT       ELSE: GET LIST FROM LIMIT ACCESS             
         BNZ   *+10                YES                                          
         MVC   OFCLMT(2),OFCAUTH   NO: MUST BE IN AUTH FIELD                    
*                                                                               
         CLI   OFCLMT,C'$'         OFFICE LIMIT ACCESS                          
         BNE   SONEX               NO OFFICE LIST                               
         MVC   SVOFLST,OFCLMT+1    ASSUME ONE BYTE OFFICE LIST                  
         CLI   OFCLMT+2,C'+'       LIMIT ACCESS WITH MARKET                     
         BE    SO050               YES: THEN OFFICE LIST IS ONE BYTE            
         CLI   OFCLMT+2,C' '       TWO CHARACTER OFFICE LIST? ($XX)             
         BNH   SO050               NO: OFFICE LIST IS ONE BYTE                  
         MVC   SVOFLST2,OFCLMT+1   MUST BE TWO BYTE OFFICE LIST                 
*                                                                               
SO020    LA    RE,0                                                             
         LA    RF,OFLTAB           OFFICE LIST TABLE                            
SO030    CLC   SVOFLST2,0(RF)      FIND A MATCH OF THE TWO BYTE LIST            
         BE    SO040                                                            
         LA    RF,L'OFLTAB(,RF)                                                 
         LA    RE,1(,RE)                                                        
         CHI   RE,X'FF'            MAXIMUM OF 255                               
         BNE   SO030                                                            
         B     SONEX               MEDIA OFFICE LIST CODE NOT FOUND             
SO040    STC   RE,SVOFLST          ONE BYTE VALUE                               
         B     SO060                                                            
*                                                                               
SO050    LLC   RE,SVOFLST          ONE BYTE MEDIA OFFICE LIST VALUE             
         MHI   RE,L'SVOFLST2       LENGTH OF TWO OFFICE CODE                    
         LA    RF,OFLTAB           OFFICE LIST CODE TABLE                       
         LA    RF,0(RE,RF)         ONE BYTE VALUE IS INDEX IN TABLE             
         MVC   SVOFLST2,0(RF)      TWO BYTE MEDIA OFFICE LIST                   
*                                                                               
SO060    CLI   SVOFLST2,C' '       DID WE FIND AN OFFICE LIST                   
         BNH   SONEX               NO                                           
*                                                                               
         TM    INFLAG,INNOOFFD     OFFICED UNAVAILABLE?                         
         BO    SOEQX               NO OFFICE, EXIT HERE                         
         MVC   OFCMOL,SVOFLST      ONE BYTE OFFICE LIT                          
         MVC   OFCMOL2,SVOFLST2    TWO BYTE OFFICE LIT                          
*                                                                               
SOEQX    CR    RB,RB               EXIT EQUAL                                   
         J     XIT                                                              
SONEX    LTR   RB,RB               EXIT NOT EQUAL                               
         J     XIT                                                              
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXIT     TM    INFLAG,INSWITCH     SYSTEM SWITCH                                
         BZ    EXITX               NO                                           
         ICM   RE,15,CMASTC        MASTC RESOLVED?                              
         BZ    EXITX               . NO, EXIT                                   
         USING MASTD,RE                                                         
         L     R1,MCUTL            R1=A(UTL)                                    
         USING UTLD,R1                                                          
         MVC   TSYS,SVTSYS         RESTORE PHYSICAL SYSTEM NUMBER               
         DROP  R1,RE                                                            
*                                                                               
EXITX    CLI   P1,0                                                             
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
* OFFICE LIST TABLE                                                             
*  C'TWO-BYTE-OFFICE-LIST-EQUIVALENT' INDEXED BY ONE BYTE VALUE                 
*  ******  DO NOT USE HEX CODES X'40' OR BELOW, OR X'5C'      ********          
***********************************************************************         
*                0-1-2-3-4-5-6-7-8-9-A-B-C-D-E-F-                               
OFLTAB   DS    0CL2                              '                              
         DC    C'                                ' 00-0F                        
         DC    C'                                ' 10-1F                        
         DC    C'                                ' 20-2F                        
         DC    C'                                ' 30-3F                        
         DC    C'  A1A2A3A4  B1B2B3B4    <       ' 40-4F                        
         DC    C'  C1C2C3C4  D1D2D3D4  E1  E2E3E4' 50-5F DO NOT USE 5C          
         DC    C'  F1F2F3F4  G1G2G3G4  H1H2H3> H4' 60-6F                        
         DC    C'  I1I2I3I4  J1J2J3J4  # @       ' 70-7F                        
         DC    C'  K1K2K3K4  L1L2L3L4  M1M2M3M4  ' 80-8F                        
         DC    C'  N1N2N3N4  O1O2O3O4  P1P2P3P4  ' 90-9F                        
         DC    C'  Q1Q2Q3Q4  R1R2R3R4  S1S2S3S4  ' A0-AF                        
         DC    C'  T1T2T3T4  U1U2U3U4  V1V2V3V4  ' B0-BF                        
         DC    C'  A B C D E F G H I   W1W2W3W4  ' C0-CF                        
         DC    C'  J K L M N O P Q R   X1X2X3X4  ' D0-DF                        
         DC    C'    S T U V W X Y Z   Y1Y2Y3Y4  ' E0-EF                        
         DC    C'0 1 2 3 4 5 6 7 8 9   Z1Z2Z3Z4  ' F0-FF                        
OFLTABL  EQU   *-OFLTAB                                                         
***********************************************************************         
* BUILD OFFICE LIST BASED ON SECURITY LIMIT & MEDIA OFFICE RECORDS              
*        ON NTRY R4=A(LIST AREA) (P3)                                           
*        ON EXIT P3 IS A(INDEXED OFFICE LIST) LIST IS TWO BYTE OFFICE           
*                INDEXED BY ONE BYTE OFFICE VALUE                               
***********************************************************************         
O        USING MOFRECD,IO                                                       
BLDOFT   NTR1                                                                   
         TM    P2,X'E0'             WANT AN INDEXED LIST?                       
         BNO   BLDOFTX              . NO                                        
*                                                                               
         XC    OLWRK,OLWRK          CLEAR OFFICE LIST WORK AREA                 
*                                                                               
         TM    SEATYPE,SEAOLIQ      SECURITY LIMIT AN OFFICE LIST?              
         BNO   BOFT010              . YES                                       
         BAS   RE,VALOFL            BUILD OFCLIST                               
         MVC   OLWRK,0(R4)          SAVE OFFICE LIST                            
         XC    0(DOFFMAXQ,R4),0(R4) CLEAR LIST AREA                             
         B     BOFT030                                                          
*                                                                               
BOFT010  TM    SEATYPE,SEAOFFQ      SECURITY LIMIT IS AN OFFICE?                
         BNO   BOFT020                                                          
         MVC   OLWRK,OFCLMT+1       SECURITY LIMIT OFFICE                       
         B     BOFT030                                                          
*                                                                               
BOFT020  MVC   OLWRK,DOFFTAB        DEFAULT OFFICE LIST                         
*                                                                               
BOFT030  MVI   PASS,C'T'            FIRST TABLE PASS                            
BOFT040  LHI   R1,DOFFMAXQ          MAXIMUM OFFICES IN LIST                     
BOFT050  STC   R1,OLWRKC            OFFICES IN LIST                             
         LHI   RF,DOFFMAXQ          MAX OFFICES IN LIST/DEFAULT LIST            
         SR    RF,R1                DISPLACEMENT INTO OFFICE LIST               
         LA    R1,OLWRK(RF)         ADDRESS OF NEXT OFFICE IN LIST              
*                                                                               
         TM    OFCINDS,OFCINOLA     NO OFFICE RECORDS                           
         BO    BOFT150                                                          
*                                                                               
         XC    O.MOFKEY,O.MOFKEY                                                
         MVI   O.MOFKTYP,MOFKTYPQ   MEDIA OFFICE RECORD                         
         MVI   O.MOFKSUB,MOFKS1Q    ONE BYTE OFFICE RECORD TYPE                 
         MVC   O.MOFKAGY,OFCAGY     AGENCY ALPHA                                
         MVC   O.MOFKSYS,SYSNUM     SYSTEM NUMBER                               
*                                                                               
         CLI   PASS,C'T'            FIRST CALL FOR TABLE BUILD?                 
         BE    BOFT100              DO NOT FILL IN OFFICE                       
*                                                                               
         TM    SEATYPE,SEAOLIQ      SECURITY LIMIT AN OFFICE LIST?              
         BO    BOFT090                                                          
         TM    SEATYPE,SEAOFFQ      SECURITY LIMIT IS AN OFFICE?                
         BNO   BOFT100                                                          
BOFT090  MVC   O.MOFK1OF,0(R1)      OFFICE FROM LIST                            
*                                                                               
BOFT100  MVC   KEY,IO                                                           
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEY,IO                       
         CLI   8(R1),0              EXIT IF NOT FOUND                           
         BNE   BOFTX                                                            
         B     BOFT120                                                          
*                                                                               
BOFT110  GOTO1 CDATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEY,IO                       
         CLI   8(R1),0              EXIT IF NOT FOUND                           
         BNE   BOFTX                                                            
*                                                                               
BOFT120  CLI   PASS,C'T'            FIRST CALL FOR TABLE BUILD?                 
         BNE   BOFT130                                                          
         MVI   PASS,C' '                                                        
         CLC   O.MOFKEY(MOFK1OF-MOFKEY),KEY                                     
         BE    BOFT040                        OFFICE RECORDS FOUND              
         OI    OFCINDS,OFCINOLA               NO OFFICE RECORDS                 
         B     BOFT040                                                          
*                                                                               
BOFT130  CLC   O.MOFKEY(MOFK1OF-MOFKEY),KEY                                     
         BE    BOFT134                                                          
*                                                                               
         TM    SEATYPE,SEAOFFQ      OFFICE LIMIT ACCESS?                        
         BO    BOFTX                                                            
         TM    SEATYPE,SEAOLIQ      LIST LIMIT ACCESS?                          
         BO    BOFT210                                                          
         TM    OFCINDS,OFCINOLA     NO OFFICE RECORDS?                          
         BO    BOFT210              . NO OFFICE RECORDS, USE DEFAULT            
         B     BOFTX                FINISHED PROCESSING OFFICER RECS            
*                                                                               
BOFT134  TM    SEATYPE,SEAOLIQ      OFFICE LIST LIMIT?                          
         BNO   BOFT140              . YES                                       
         CLC   O.MOFKEY,KEY         FOUND OFFICE?                               
         BE    BOFT150                YES                                       
         B     BOFT200                                                          
*                                                                               
BOFT140  TM    SEATYPE,SEAOFFQ      OFFICE LIMIT?                               
         BNO   BOFT144              . NO                                        
         CLC   O.MOFKEY,KEY         FOUND OFFICE?                               
         BE    BOFT150              . YES                                       
         B     BOFTX                                                            
BOFT144  CLC   O.MOFKSYS,SYSNUM     CORRECT SYSTEM?                             
         BNE   BOFT110              . NO                                        
*                                                                               
BOFT150  L     R4,P3                R4=A(START OF LIST)                         
         SR    RE,RE                                                            
         IC    RE,0(R1)             ONE CHARACTER OFFICE                        
*                                                                               
         TM    OFCINDS,OFCINOLA     NO OFFICE RECORDS                           
         BO    BOFT160                                                          
         IC    RE,O.MOFK1OF         ONE BYTE OFFICE FROM RECORD                 
*                                                                               
BOFT160  TM    P2,X'F0'             INDEXED LIST WITH NAME?                     
         BO    BOFT164              . NO                                        
         MHI   RE,L'MOFK2OF         JUST TWO BYTE OFFICE                        
         B     BOFT170                                                          
BOFT164  MHI   RE,L'MOFK2OF+L'MONAMSH   ADDING 2 BYTE OFFICE AND NAME           
*                                                                               
BOFT170  AR    R4,RE                INDEX INTO LIST USING OFFICE #              
         MVC   0(1,R4),0(R1)                                                    
         MVI   1(R4),C' '                                                       
*                                                                               
         TM    OFCINDS,OFCINOLA     NO OFFICE RECORDS                           
         BO    BOFT200                                                          
         MVC   0(L'MOFK2OF,R4),O.MOFKC2OF                                       
*                                                                               
BOFT174  TM    P2,X'F0'             INDEXED LIST WITH NAME?                     
         BNO   BOFT200              . NO                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',O.MOFKDA,IO,DMWORK           
         CLI   8(R1),0                                                          
         BNE   BOFT200              EXIT IF NOT FOUND                           
*                                                                               
         LA    R1,IO+MOFFIRST                                                   
BOFT180  CLI   0(R1),0                                                          
         BE    BOFT200                                                          
         CLI   0(R1),MONAMELQ       OFFICE NAME ELEMENT                         
         BE    BOFT190                                                          
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BOFT180                                                          
*                                                                               
         USING MONAMD,R1                                                        
BOFT190  MVC   2(L'MONAMSH,R4),MONAMSH     SHORT NAME                           
         DROP  R1                                                               
*                                                                               
BOFT200  TM    SEATYPE,SEAOFFQ      OFFICE LIMIT ACCESS?                        
         BO    BOFTX                . YES, DONE                                 
         TM    SEATYPE,SEAOLIQ      LIST LIMIT ACCESS?                          
         BO    BOFT210                                                          
         TM    OFCINDS,OFCINOLA     NO OFFICE RECORDS?                          
         BNO   BOFT110              . OFFICE RECS ARE BEING USED                
*                                                                               
BOFT210  SR    R1,R1                                                            
         IC    R1,OLWRKC            COUNTDOWN OF OFFICES                        
         BCT   R1,BOFT050                                                       
*                                                                               
BOFTX    L     R4,P3                R4=A(LIST AREA)                             
         MVI   0(R4),X'FF'          FLAG TABLE AS BUILT                         
BLDOFTX  J     JXIT                                                             
         DROP  O                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE ACCESS USING DATA ACCESS                                             
*       EXIT EQUAL, ACCESS GRANTED                                              
*       EXIT NOT EQUAL, UNAUTHORIZED                                            
***********************************************************************         
G        USING SALAREC,IO                                                       
VALDAC   NTR1                                                                   
         XC    DUB,DUB                                                          
*                                                                               
         XC    SVDERV,SVDERV                                                    
         XC    LISTIOB(LISTIOBL),LISTIOB                                        
         STCM  R3,15,LISTACOM                                                   
         MVC   LISTIAGY,SVSAGY     SECURITY AGENCY                              
         MVC   LISTIGRP,OFCLMT+2   GROUP NUMBER IN LIMIT ACCESS FIELD           
         MVC   LISTISYS,SYSNUM     SYSTEM NUMBER                                
*                                                                               
         MVC   LISTITYP,=AL2(LISTTCLT) CLIENT                                   
         MVI   LISTACTN,LISTAINF   GET LIST INFO FOR LIST NUMBER                
         GOTO1 =V(LISTIO),LISTIOB,RR=RELO                                       
         BNE   VDACNEX             LIST DOESN'T EXIST, NO ACCESS                
         MVC   SVDERV,LISTIDER     SAVE OFF ATTACHED DERIVED LISTS              
*                                                                               
VDA010   XC    LISTDVAL,LISTDVAL                                                
         TM    OFCINDS,OFCI2CSC    TEST 2 CHARACTER CLIENT CODE PASSED          
         BZ    VDA020              NO                                           
         MVC   LISTDVAL(L'OFCCLT2),OFCCLT2                                      
         OI    LISTFLAG,LISTFRAW   NOT TEXT, RAW RECORD VALUE                   
         B     VDA022                                                           
*                                                                               
VDA020   MVC   LISTDVAL(L'OFCCLT),OFCCLT                                        
VDA022   MVI   LISTACTN,LISTACHK                                                
         GOTO1 =V(LISTIO),LISTIOB,RR=RELO                                       
         BE    VDA030              INCLUDED IN THE LIST                         
         TM    LISTDATT,LDDANOQ    EXPLICITLY EXCLUDED FROM LIST?               
         BO    VDACNEX             EXCLUDED, THEN NO ACCESS                     
*                                                                               
         OC    SVDERV,SVDERV       ANY DERIVED (OFFICE FOR NOW) LIST            
         BZ    VDACNEX             . THEN NO ACCESS                             
*                                                                               
         MVC   SVLMT,OFCLMT        SAVE LIMIT ACCESS VALUE                      
         MVC   OFCLMT,SVDERV       REPLACE LIMIT ACCESS VALUE WITH              
         MVC   OFCAUTH,OFCLMT      DERIVED (OFFICE LIST ONLY FOR NOW)           
*                                                                               
         BRAS  RE,VALOFL           CHECK OFFICE LIST, CC EQUAL IF FOUND         
*                                                                               
         MVC   OFCLMT,SVLMT        RESTORE LIMIT ACCESS                         
         MVC   OFCAUTH,SVLMT                                                    
         J     JXIT                EXIT WITH CC SET FROM VALOFL                 
*                                                                               
VDA030   B     VDACEQX             FOR NOW NO MORE LISTS-ACCESS GRANTED         
*                                                                               
VDACNEX  LTR   RB,RB                                                            
         J     JXIT                                                             
VDACEQX  CR    RB,RB                                                            
         J     JXIT                                                             
         DROP  G                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
SPACES   DC    CL16' '                                                          
         LTORG                                                                  
*                                                                               
MAXOFFS  EQU   254                                                              
*                                                                               
DOFFTAB  DS    0C                   DEFAULT OFFICE TABLE                        
         DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         DC    C'`=º\;'',./~!@#$%^&&*()_+{}|:"<>?'                             
DOFFMAXQ EQU   *-DOFFTAB                                                        
*                                                                               
JXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* FIND SYSTEM NUMBER FROM FIRST LETTER OF NAME                                  
* DECIPHER SECURITY LIMIT ACCESS TYPE                                           
* READ ACCESS RECORD TO DETERMINE IF AGENCY USING 2 CHAR MEDIA OFFICES          
*                                                                               
* LIMIT ACCESS OF NULL MEANS NO LIMITITATION                                    
* LIMIT ACCESS OF AAA  MEANS CLIENT                                             
* LIMIT ACCESS OF *X   MEANS OFFICE A                                           
* LIMIT ACCESS OF *A1  MEANS CLIENT GROUP A1                                    
* LIMIT ACCESS OF $A   MEANS VALIDATE AGAINST OFFICE LIST                       
* LIMIT ACCESS OF +A   MEANS MATCH TO MKTREC LIMIT ACCESS (SPOT)                
* LIMIT ACCESS OF +A   MEANS X MUST APPEAR IN MARKET LIST                       
* LIMIT ACCESS OF *A+B MEANS OFFICE AND MARKET LIMIT                            
* LIMIT ACCESS OF ==XX MEANS DATA ACCESS                                        
***********************************************************************         
         USING SYSLSTD,R1                                                       
FSYSLA   NTR1  BASE=*,LABEL=*                                                   
         XC    SEATYPE,SEATYPE                                                  
*                                                                               
         CLI   OFCSYS,C'N'          NETWORK?                                    
         BNE   FSL06                                                            
         MVI   OFCSYS,C'S'          YES - READ FOR SPOT OFFICES                 
*                                                                               
FSL06    MVI   SYSNUM,X'FF'         INIT SYSTEM FIELD                           
FSL08    LA    R1,SYSLST                                                        
         LA    R1,6(R1)             R1=A(SYSTEM LIST)                           
FSL10    CLI   0(R1),0              TEST E-O-T                                  
         BE    FSL40                                                            
         CLI   SYSLNUM,1            IGNORE SERVICE SYSTEM                       
         BE    FSL20                                                            
         CLC   SYSLNAME(1),OFCSYS                                               
         BE    FSL22                                                            
FSL20    LA    R1,SYSLLEN(R1)       NO - BUMP TO NEXT TABLE ENTRY               
         B     FSL10                                                            
FSL22    MVC   SYSNUM,SYSLNUM       SET SYSTEM NUMBER                           
         DROP  R1                                                               
*                                                                               
FSL40    OC    OFCLMT,OFCLMT        ANY SECURITY LIMIT?                         
         BZ    FSL42                . NO                                        
         MVC   OFCAUTH,OFCLMT       MAKE SURE OFCAUTH & OFCLMT EQUAL            
         B     FSL50                                                            
*                                                                               
FSL42    OC    OFCAUTH,OFCAUTH      ANY SECURITY LIMIT?                         
         BZ    FSL100                                                           
         MVC   OFCLMT(L'OFCAUTH),OFCAUTH                                        
*                                                                               
FSL50    CLI   OFCLMT,C'='          DATA ACCESS SECURITY                        
         BNE   FSL60                                                            
         OI    SEATYPE,SEADACQ                                                  
         ICM   R1,15,OFCSECD        SECRET BLOCK                                
         BZ    FSL100                                                           
         USING SECD,R1                                                          
         MVC   SVSAGY,SECAGY        SECURITY AGENCY                             
         MVC   SYSNUM,SECOSYS       SYSTEM NUMBER                               
         B     FSL100                                                           
         DROP  R1                                                               
*                                                                               
FSL60    CLI   OFCLMT,C'$'          OFFICE LIST SECURITY?                       
         BNE   *+8                                                              
         OI    SEATYPE,SEAOLIQ      OFFICE LIST LIMIT                           
*                                                                               
         CLI   OFCLMT,C'+'          MARKET SECURITY?                            
         BE    *+12                                                             
         CLI   OFCLMT+2,C'+'        OFFICE/MARKET                               
         BNE   *+8                                                              
         OI    SEATYPE,SEAMKTQ      MARKET LIMIT                                
*                                                                               
         CLI   OFCLMT,C'*'          OFFICE/CLIENT GROUP SECURITY?               
         BNE   FSL70                                                            
         CLI   OFCLMT+1,C'*'        TWO CHARACTER OFFICE?                       
         BE    FSL68                                                            
         CLI   OFCLMT+2,C'+'        OFFICE/MARKET                               
         BE    FSL68                                                            
         OC    OFCLMT+2(2),OFCLMT+2 ZEROES MEAN ONE BYTE OFFICE                 
         BZ    FSL68                                                            
         CLC   OFCLMT+2(2),=CL2' '  SPACES MEAN ONE BYTE OFFICE                 
         BE    FSL68                                                            
         OI    SEATYPE,SEACLGQ      CLIENT GROUP LIMIT                          
         B     FSL70                                                            
*                                                                               
FSL68    OI    SEATYPE,SEAOFFQ      OFFICE LIMIT ACCESS                         
*                                                                               
FSL70    CLI   SEATYPE,0            ALREADY HAVE SECURITY LIMIT?                
         BNE   FSL100               . YES                                       
         OI    SEATYPE,SEACLIQ      . NO, MUST BE CLIENT LIMIT                  
                                                                                
*-----------------------------------                                            
* SEC AGENCY & IF MEDIA OFFICES IN USE                                          
*-----------------------------------                                            
FSL100   ICM   RE,15,CMASTC         MASTC RESOLVED?                             
         BZ    FSL110               . NO, EXIT                                  
         USING MASTD,RE                                                         
         L     R1,MCUTL             R1=A(UTL)                                   
         USING UTLD,R1                                                          
         MVC   SVTSYS,TSYS          SAVE SYSTEM OF CALLING PROGRAM              
         MVI   TSYS,X'0A'           CONTROL                                     
         OI    INFLAG,INSWITCH      FLAG SYSTEM SWITCH                          
         DROP  R1,RE                                                            
*                                                                               
FSL110   TM    SEATYPE,SEADACQ      DATA ACCESS                                 
         BZ    FSL112               . NO                                        
         ICM   R1,15,OFCSECD        SECRET BLOCK PASSED?                        
         BNZ   FSYSLAX                                                          
*                                                                               
FSL112   OI    OFCINDS,OFCINOLA     START WITH NO MEDIA OFFICE RECORDS          
         MVC   SVSAGY,OFCAGY        START WITH ALPHA AS SECURITY AGENCY         
*                                                                               
A        USING CT5REC,IO            CHECK ACCESS RECORD                         
         XC    A.CT5KEY,A.CT5KEY                                                
         MVI   A.CT5KTYP,CT5KTYPQ   C'5' ACCESS RECORD                          
         MVC   A.CT5KALPH,OFCAGY    AGENCY ALPHA                                
*                                                                               
         MVC   KEY,IO                                                           
         GOTO1 CDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                 INVALID AGENCY??? CAN'T BE                  
*                                                                               
         LA    R1,A.CT5DATA                                                     
FSL120   CLI   0(R1),0                                                          
         BE    FSYSLAX                                                          
         CLI   0(R1),CTSEAELQ       X'B8' SECURITY AGENCY                       
         BE    FSL140                                                           
         CLI   0(R1),CTAADELQ       X'B9' AGENCY ACCESS DETAILS                 
         BE    FSL150                                                           
FSL130   XR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FSL120                                                           
*                                                                               
         USING CTSEAD,R1                                                        
FSL140   MVC   SVSAGY,CTSEAAID      SECURITY AGENCY                             
         B     FSL130                                                           
*                                                                               
         USING CTAADD,R1                                                        
FSL150   TM    CTAADFLG,CTAAD2OF    AGENCY USING MEDIA OFFICE RECORDS?          
         BZ    FSL130                 . NO                                      
         NI    OFCINDS,X'FF'-OFCINOLA . YES, TURN OFF "NOT USED" FLAG           
         B     FSL130                                                           
         DROP  A,R1                                                             
*                                                                               
FSYSLAX  J     JXIT                                                             
         LTORG                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE ACCESS TO SPOT OR NET CLIENT GROUP                                   
* REMEMBER THERE CAN ONLY BE ONE CLIENT PASSIVE FOR EACH GROUP ID               
* SO IF YOU FIND A PASSIVE THE GROUP NUMBER MUST AGREE                          
***********************************************************************         
VALCGS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,CMASTC        OFFLINE                                      
         BZ    VALCGS05            NO                                           
         USING MASTD,RE                                                         
         MVC   SVOVSYS,MCOVSYS     SAVE SYSTEM                                  
         TM    INFLAG,INSWITCH     SYSTEM SWITCH                                
         BZ    VALCGS10            NO                                           
         L     R1,MCUTL            R1=A(UTL)                                    
         USING UTLD,R1                                                          
         MVC   TSYS,SVTSYS         SWITCH BACK TO SYS OF CALLER                 
         DROP  R1,RE                                                            
*                                                                               
VALCGS05 GOTOR CGETFACT,DMCB,(X'80',SVOVSYS),F#TOVSYS                           
*                                                                               
VALCGS10 TM    OFCLMT+2,X'F0'      CHARACTER NUMBER/OLD STYLE?                  
         BNO   VALCGS15            NO: NEW STYLE                                
*----------------------------------------------------------------------         
* SPOT CLIENT GROUP (OLD STYLE / C'*AN' A=ALPHA,N=NUMBER)                       
*----------------------------------------------------------------------         
K        USING SGRPRECD,KEY        CLTGRP PASSIVE                               
         XC    KEY,KEY                                                          
         MVI   K.SGRPPTYP,SGRPPTYPQ    TYPE                                     
         MVI   K.SGRPPSTYP,SGRPPCTYQ   SUB                                      
         MVC   K.SGRPPAGMD(1),OFCSAGMD A/M                                      
         MVC   K.SGRPPVAL(3),OFCCLT    CLT                                      
         OC    K.SGRPPVAL,=CL6' '      BLANK PADDED                             
         MVC   K.SGRPPID(1),OFCLMT+1   GROUP ID                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                  
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VALCGS20                                                         
*                                                                               
         NI    K.SGRPPID+1,X'F0'   DROP SECOND DIGIT IN KEY                     
         PACK  DUB(1),OFCLMT+2(1)  LEFT ALIGN 1 DIGIT GROUP                     
         NI    DUB,X'F0'                                                        
         CLC   DUB(1),K.SGRPPID+1  TEST SAME GROUP NUMBER                       
         BNE   VALCGS50            NO                                           
         MVI   P1,0                SET ACCESS AUTHORIZED                        
         B     VALCGS50                                                         
         DROP  K                                                                
*----------------------------------------------------------------------         
* SPOT/NET CLI GROUP(NEW STYLE/C'*'X'CC9999/F' CC=ID,9999=PWOS,F=DELIM)         
*----------------------------------------------------------------------         
VALCGS15 MVC   HALF,OFCLMT+2       CLIENT GROUP CODE                            
         TM    HALF,X'0F'          TURN OFF NIBBLE X'F' DELIMITER               
         BNO   *+8                 .                                            
         NI    HALF,X'F0'          .                                            
         TM    HALF+1,X'F0'        .                                            
         BNO   *+8                 .                                            
         NI    HALF+1,X'0F'        .                                            
         TM    HALF+1,X'0F'        .                                            
         BNO   *+8                 .                                            
         NI    HALF+1,X'F0'        .                                            
*                                                                               
VALCGS20 CLI   SVOVSYS,X'03'       NET SYSTEM?                                  
         BE    VALCGS30            YES                                          
*----------------------------------------------------------------------         
* SPOT CLIENT GROUP                                                             
*----------------------------------------------------------------------         
K        USING SGRPRECD,KEY        CLTGRP PASSIVE                               
         XC    KEY,KEY                                                          
         MVI   K.SGRPPTYP,SGRPPTYPQ    TYPE                                     
         MVI   K.SGRPPSTYP,SGRPPCTYQ   SUB                                      
         MVC   K.SGRPPAGMD(1),OFCSAGMD A/M                                      
         MVC   K.SGRPPVAL(3),OFCCLT    CLT                                      
         OC    K.SGRPPVAL,=CL6' '      BLANK PADDED                             
         MVC   K.SGRPPID,OFCLMT+1      GROUP ID                                 
         MVC   K.SGRPPCODE,HALF        GROUP CODE (PWOS)                        
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                  
         CLC   KEY(12),KEYSAVE     DID WE FIND THE PASSIVE KEY?                 
         BNE   VALCGS50            NO - LEAVE AS NOT AUTORIZED                  
         MVI   P1,0                SET ACCESS AUTHORIZED                        
         B     VALCGS50            DONE                                         
         DROP  K                                                                
*----------------------------------------------------------------------         
* NET CLIENT GROUP                                                              
*----------------------------------------------------------------------         
K        USING CLGRECD,KEY         NET CLTGRP PASSIVE DSECT                     
VALCGS30 XC    KEY,KEY             CLEAR THE KEY                                
         MVI   K.CLGCTYP,X'0D'     TYPE (X'0D')                                 
         MVI   K.CLGCTYP+1,X'86'   SUB-TYPE (X'86')                             
         MVC   K.CLGCAGMD,OFCSAGMD A/M                                          
         MVC   K.CLGCID,OFCLMT+1   GROUP ID (ONLY SUPPORTING 1 CHAR)            
         MVC   K.CLGCGRP,HALF      PACKED 2 BYTES LEFT ALIGNED PWOS             
*                                                                               
         MVC   DUB(2),OFCCLT2      PACKED CLIENT CODE                           
         TM    OFCINDS,OFCI2CSC    2 CHARACTER CLIENT CODE PASSED?              
         BNZ   VALCGS40            YES                                          
*                                                                               
         GOTO1 CCALLOV,DMCB,0,X'D9000A14'                                       
         L     RF,0(R1)            A(CLPACK)                                    
         GOTO1 (RF),DMCB,OFCCLT,DUB                                             
*                                                                               
VALCGS40 MVC   K.CLGCCLT,DUB       CLIENT CODE (PACKED)                         
*                                                                               
         MVC   KEYSAVE,KEY         SAVE OFF THE KEY                             
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                  
         CLC   KEY(12),KEYSAVE     DID WE FIND THE PASSIVE KEY?                 
         BNE   VALCGS50            NO - LEAVE AS NOT AUTORIZED                  
         MVI   P1,0                SET ACCESS AUTHORIZED                        
         DROP  K                   DROP CLTGRP PASSIVE USING                    
*                                                                               
VALCGS50 TM    INFLAG,INSWITCH     SYSTEM SWITCH                                
         BZ    VALCGSX             NO                                           
         ICM   RE,15,CMASTC        OFFLINE                                      
         BZ    VALCGSX             NO                                           
         USING MASTD,RE                                                         
         L     R1,MCUTL            R1=A(UTL)                                    
         USING UTLD,R1                                                          
         MVI   TSYS,X'0A'          SWITCH TO CONTROL                            
         DROP  R1,RE                                                            
*                                                                               
VALCGSX  CLI   P1,0                CHECK VALIDATION ON EXIT                     
         J     JXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* FOR PRINT                                                                     
***********************************************************************         
VALCGP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    INFLAG,INSWITCH     SYSTEM SWITCH                                
         BZ    VALCGP10                                                         
         ICM   RE,15,CMASTC        OFFLINE                                      
         BZ    VALCGP10            NO                                           
         USING MASTD,RE                                                         
         L     R1,MCUTL            R1=A(UTL)                                    
         USING UTLD,R1                                                          
         MVC   TSYS,SVTSYS         SWITCH BACK TO SYS OF CALLER                 
         DROP  R1,RE                                                            
*                                                                               
VALCGP10 TM    OFCLMT+2,X'F0'      CHARACTER NUMBER/OLD STYLE?                  
         BNO   VALCGP20            NO: NEW STYLE                                
*----------------------------------------------------------------------         
* PRINT CLIENT GROUP (OLD STYLE / C'*ANN' A=ALPHA,N=NUMBER)                     
*----------------------------------------------------------------------         
K        USING PGRPRECD,KEY        CLIENT GROUP PASSIVE POINTER                 
         XC    KEY,KEY                                                          
         MVI   K.PGRPPTYP,PGRPPCGQ   RECORD TYPE                                
         MVC   K.PGRPPAGY,OFCAGY     AGY                                        
         MVC   K.PGRPPMED,OFCPMED    MED                                        
         MVC   K.PGRPPVAL(3),OFCCLT  CLT                                        
         OC    K.PGRPPVAL,=CL6' '    SPACE PADDED                               
         MVC   K.PGRPPID(1),OFCLMT+1 GROUP ID                                   
         PACK  DUB(2),OFCLMT+2(3)    GROUP CODE + "NEXT" BYTES                  
         MVC   K.PGRPPCODE(1),DUB    GROUP CODE MUST BE "NN00" (PWOS)           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEYSAVE,KEY                  
         CLC   KEY(16),KEYSAVE     CHECK THROUGH GROUP CODE                     
         BNE   VALCGP50            IF MATCH, ACCESS OK                          
         MVI   P1,0                SET ACCESS AUTHORIZED                        
         B     VALCGP50                                                         
         DROP  K                                                                
*----------------------------------------------------------------------         
* PRINT CLIENT GROUP (NEW STYLE/C'*'X'CC9999/F' CC=ID,9999=PWOS,F=DELIM         
*----------------------------------------------------------------------         
VALCGP20 MVC   HALF,OFCLMT+2       CLIENT GROUP CODE                            
         TM    HALF,X'0F'          TURN OFF NIBBLE X'F' DELIMITER               
         BNO   *+8                 .                                            
         NI    HALF,X'F0'          .                                            
         TM    HALF+1,X'F0'        .                                            
         BNO   *+8                 .                                            
         NI    HALF+1,X'0F'        .                                            
         TM    HALF+1,X'0F'        .                                            
         BNO   *+8                 .                                            
         NI    HALF+1,X'F0'        .                                            
*                                                                               
K        USING PGRPRECD,KEY        CLIENT GROUP PASSIVE POINTER                 
VALCGP30 XC    KEY,KEY                                                          
         MVI   K.PGRPPTYP,PGRPPCGQ   RECORD TYPE                                
         MVC   K.PGRPPAGY,OFCAGY     AGY                                        
         MVC   K.PGRPPMED,OFCPMED    MED                                        
         MVC   K.PGRPPVAL(3),OFCCLT  CLT                                        
         OC    K.PGRPPVAL,=CL6' '    SPACE PADDED                               
         MVC   K.PGRPPID(1),OFCLMT+1 GROUP ID                                   
         MVC   K.PGRPPCODE,HALF      PWOS GROUP CODE                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEYSAVE,KEY                  
         CLC   KEY(16),KEYSAVE     CHECK THROUGH GROUP CODE                     
         BNE   VALCGP50            IF MATCH, ACCESS OK                          
         MVI   P1,0                SET ACCESS AUTHORIZED                        
         DROP  K                                                                
*                                                                               
VALCGP50 TM    INFLAG,INSWITCH     SYSTEM SWITCH                                
         BZ    VALCGPX             NO                                           
         ICM   RE,15,CMASTC        OFFLINE                                      
         BZ    VALCGPX             NO                                           
         USING MASTD,RE                                                         
         L     R1,MCUTL            R1=A(UTL)                                    
         USING UTLD,R1                                                          
         MVI   TSYS,X'0A'          SWITCH TO CONTROL                            
         DROP  R1,RE                                                            
*                                                                               
VALCGPX  CLI   P1,0                CHECK AUTHORIZATION ON EXIT                  
         J     JXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* VALIDATE OFFICE USING MEDIA OFFICE RECORDS                                    
*       EXIT EQUAL, OFFICE VALID                                                
*       EXIT LOW,   OFFICE INVALID                                              
*       EXIT HIGH,  MEDIA OFFICE RECORD NOT FOUND                               
*                                                                               
*       ON EXIT EQUAL OR HIGH, OFCOFC2 WILL CONTAIN TWO CHARACTER               
*                              OFFICE CODE REPRESENTATION                       
***********************************************************************         
O        USING MOFRECD,IO                                                       
VALMOF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    P2,X'E0'             USING INDEXED TABLE?                        
         BNO   VMOF020              . NO                                        
         BRAS  RE,VOFUT             VALIDATE OFFICE USING TABLE                 
         BE    VMOFEX                 AUTHORIZED                                
         B     VMOFLX                 UNAUTHORIZED                              
*                                                                               
VMOF020  TM    OFCINDS,OFCINOLA     ANY MEDIA OFFICE RECORDS?                   
         BO    VMOF060              . NO                                        
*                                                                               
O        USING MOFRECD,IO                                                       
         XC    O.MOFKEY,O.MOFKEY                                                
         MVI   O.MOFKTYP,MOFKTYPQ   C'O' MEDIA OFFICE RECORD                    
         MVI   O.MOFKSUB,MOFKS1Q    ONE BYTE OFFICE RECORD                      
         MVC   O.MOFKAGY,OFCAGY     AGENCY ALPHA                                
*                                                                               
         MVC   KEY,IO                                                           
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEY,IO                       
         CLI   8(R1),0                        EXIT IF NOT FOUND                 
         BNE   VMOF060                        NO OFF LIM ACC RECS FOUND         
         CLC   O.MOFKEY(MOFK1OF-MOFKEY),KEY   OFFICE LIM ACC REC?               
         BNE   VMOF060                        . NO, NONE FOUND                  
*                                                                               
         XC    O.MOFKEY,O.MOFKEY                                                
         MVI   O.MOFKTYP,MOFKTYPQ   C'O' MEDIA OFFICE RECORD                    
         MVI   O.MOFKSUB,MOFKS1Q    ONE BYTE OFFICE RECORD                      
         MVC   O.MOFKAGY,OFCAGY     AGENCY ALPHA                                
         MVC   O.MOFK1OF,OFCOFC     ONE BYTE OFFICE                             
         MVC   O.MOFKSYS,SYSNUM     SYSTEM NUMBER                               
         CLI   OFCOFC,0             VALIDATING ONE CHAR OFFICE CODE?            
         BNE   VMOF030              . YES                                       
         MVI   O.MOFKSUB,MOFKS2Q    TWO CHAR OFFICE RECORD                      
         MVC   O.MOFK2OF,OFCOFC2    TWO CHAR OFFICE                             
         OC    O.MOFK2OF,=C'  '     MAKE SURE OF CHARACTERS                     
*                                                                               
VMOF030  MVC   KEY,IO                                                           
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEY,IO                       
         CLI   8(R1),0              EXIT IF NOT FOUND                           
         BNE   VMOF070              NO MEDIA OFFICE RECORDS FOUND               
*                                                                               
VMOF040  CLC   O.MOFKEY,KEY         FOUND THE OFFICE?                           
         BNE   VMOF070              . NO, INVALID OFFICE                        
*                                                                               
         CLI   OFCOFC,0             ONE CHAR OFFICE?                            
         BE    VMOF050                                                          
         MVC   OFCOFC2,O.MOFKC2OF                                               
         B     VMOF100                                                          
VMOF050  MVC   OFCOFC,O.MOFKC1OF                                                
         B     VMOF100                                                          
*-----------------------------------                                            
* EXIT HIGH - NO MEDIA OFFICE RECS                                              
*-----------------------------------                                            
VMOF060  OI    OFCINDS,OFCINOLA     SET NO OFF LIM RECS FOR NEXT CALL           
         CLI   OFCOFC,0             VALIDATING ONE CHAR OFFICE CODE?            
         BNE   VMOF064              . YES                                       
         MVC   OFCOFC,OFCOFC2       . NO, RETURN IN OFCOFC                      
         B     VMOFHX               EXIT HIGH, NO OFF LIM ACCESS RECS           
VMOF064  MVC   OFCOFC2(1),OFCOFC    OFCOFC2 ALWAYS PRINTABLE 2 CHAR             
         MVI   OFCOFC2+1,C' '                                                   
         B     VMOFHX                                                           
*-----------------------------------                                            
* EXIT LOW - INVALID OFFICE                                                     
*-----------------------------------                                            
VMOF070  OI    OFCINDS,OFCIOINV     TURN ON INVALID OFFICE                      
         CLI   OFCOFC,0             VALIDATING ONE BYTE OFFICE CODE?            
         BE    VMOFLX               . NO                                        
         XC    OFCOFC2,OFCOFC2      IF INVALID, MAKE SURE NO TRANSLATED         
         B     VMOFLX                OFFICE IS RETURNED                         
*-----------------------------------                                            
* EXIT EQUAL - VALID OFFICE                                                     
*-----------------------------------                                            
VMOF100  TM    P2,X'01'             WANT OFFICE INFORMATION?                    
         BZ    VMOFEX               . NO                                        
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',O.MOFKDA,IO,DMWORK           
         CLI   8(R1),0                                                          
         BNE   VMOFEX               EXIT IF NOT FOUND                           
*                                                                               
         LA    R1,IO+MOFFIRST                                                   
         L     RF,P3                RF=A(OUTPUT AREA)                           
VMOF110  CLI   0(R1),0                                                          
         BE    VMOFEX                                                           
         CLI   0(R1),MONAMELQ                                                   
         BE    VMOF120                                                          
         CLI   0(R1),MOUIPELQ                                                   
         BE    VMOF130                                                          
VMOF112  SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VMOF110                                                          
*                                                                               
         USING MONAMD,R1                                                        
VMOF120  CLI   P3,C'S'              WANT OFFICE SHORT NAME?                     
         BNE   VMOF124                                                          
         MVC   0(L'MONAMSH,RF),MONAMSH                                          
         AHI   RF,L'MONAMSH                                                     
         B     VMOF112                                                          
VMOF124  CLI   P3,C'L'              WANT OFFICE LONG NAME?                      
         BNE   VMOF112                                                          
         MVC   0(L'MONAMLO,RF),MONAMLO                                          
         AHI   RF,L'MONAMLO                                                     
         B     VMOF112                                                          
         DROP  R1                                                               
*                                                                               
         USING MOUIPD,R1                                                        
VMOF130  CLI   P3,C'W'              WANT WEBDAV USER ID AND PASSWORD?           
         BNE   VMOF112              . NO                                        
         MVC   0(L'MOUIPUID,RF),MOUIPUID                                        
         MVC   L'MOUIPUID(L'MOUIPASS,RF),MOUIPASS                               
         AHI   RF,L'MOUIPUID+L'MOUIPASS                                         
         B     VMOF112                                                          
         DROP  R1                                                               
*                                                                               
VMOFLX   LHI   R1,0                                                             
         B     VMOFX                EXIT LOW, OFFICE INVALID                    
VMOFHX   LHI   R1,2                                                             
         B     VMOFX                EXIT HIGH, NO OFF LIM ACCESS RECS           
VMOFEX   LHI   R1,1                 EXIT EQUAL, OFFICE VALID                    
*                                                                               
VMOFX    MVI   PASS,0               RESET PASS                                  
         CHI   R1,1                                                             
         J     JXIT                                                             
         DROP  O                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE THE OFFICE USING THE OFFICE TABLE                         
***********************************************************************         
VOFUT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,P3                                                            
         LHI   R1,MAXOFFS+1              MAX OFFICES + 1 (SKIP 0)               
*                                                                               
         CLI   OFCOFC,0                  ONE BYTE OFFICE PASSED?                
         BE    VOFT24                    . NO                                   
*                                                                               
         SR    R1,R1                                                            
         IC    R1,OFCOFC                                                        
         TM    P2,X'F0'                  OFFICE LIST WITH NAMES?                
         BO    VOFT10                    . YES                                  
         MHI   R1,L'MOFK2OF              . NO, JUST INDEX BY OFFICE             
         B     VOFT12                                                           
VOFT10   MHI   R1,L'MOFK2OF+L'MONAMSH                                           
*                                                                               
VOFT12   AR    R4,R1                     USE ONE BYTE OFF AS INDEX              
         CLI   0(R4),C' '                ANYTHING THERE?                        
         BNH   VOFTERX                   . NO                                   
         MVC   OFCOFC2,0(R4)                                                    
         B     VOFTOKX                                                          
*                                                                               
VOFT20   CLC   0(L'OFCOFC2,R4),OFCOFC2                                          
         BNE   VOFT24                                                           
         LHI   R0,MAXOFFS+1              MAX OFFICES + 1 (SKIP 0)               
         SR    R0,R1                     SUBTRACT TO GET 1 BYTE OFF #           
         STC   R0,OFCOFC                                                        
         B     VOFTOKX                                                          
*                                                                               
VOFT24   LA    R4,L'MOFK2OF(R4)          NEXT OFFICE                            
         TM    P2,X'F0'                  OFFICE LIST WITH NAMES?                
         BNO   *+8                       . NO                                   
         LA    R4,L'MONAMSH(R4)          SKIP NAME AS WELL                      
         BCT   R1,VOFT20                                                        
*                                                                               
VOFTERX  LTR   RB,RB                                                            
         B     VOFTX                                                            
VOFTOKX  CR    RB,RB                                                            
VOFTX    J     JXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DMCB     DS    6F                                                               
RELO     DS    A                                                                
CALLTYPE DS    C                                                                
PASS     DS    C                                                                
SVSAGY   DS    CL2                 SECURITY AGENCY                              
SYSNUM   DS    X                   SYSTEM NUMBER OF SYSTEM PASSED               
SVTSYS   DS    X                   SE NUMBER OF CALLING PROGRAM                 
SVLMT    DS    XL4                 SAVE LIMIT ACCESS BYTES                      
SVOFLST  DS    X                   SAVE OFFICE LIST                             
SVOFLST2 DS    CL2                 SAVE TWO BYTE OFFICE LIST                    
INBYTE   DS    X                   INTERNAL BYTE                                
INFLAG   DS    X                   INTERNAL PROGRAM FLAG                        
INSWITCH EQU   X'80'               . SYSTEM SWITCH                              
INNOOFFD EQU   X'40'               . OFFICED NOT AVAILABLE (OLD STYLE)          
SEATYPE  DS    X                   SECURITY LIMIT ACCESS TYPE                   
SEACLIQ  EQU   X'80'               . CLIENT LIMIT                               
SEAOFFQ  EQU   X'40'               . OFFICE LIMIT                               
SEAOLIQ  EQU   X'20'               . OFFICE LIST LIMIT                          
SEAMKTQ  EQU   X'10'               . MARKET LIMIT                               
SEACLGQ  EQU   X'08'               . CLIENT GROUP LIMIT                         
SEADACQ  EQU   X'04'               . DATA ACCESS                                
OLWRKC   DS    X                   OFFICE COUNT IN OFFICE WORK LIST             
OLWRK    DS    XL(DOFFMAXQ)        WORK AREA FOR OFFICE LISTS                   
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BITS     DS    X                                                                
KEY      DS    CL42                                                             
KEYSAVE  DS    CL42                                                             
DMWORK   DS    XL42                                                             
*                                                                               
SVDERV   DS    XL4                 SAVE DERIVED LIST                            
*                                                                               
SVOVSYS  DS    X                   SYSTEM NUMBER                                
*                                                                               
       ++INCLUDE DDLISTD           LIST BLOCK FOR LISTIO                        
*                                                                               
         DS    0D                                                               
IO       DS    2000C                                                            
WORKX    EQU   *                                                                
*                                                                               
PARMSD   DSECT                                                                  
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*PREFIX=S                                                                       
       ++INCLUDE SPGENGRP                                                       
*PREFIX=                                                                        
       ++INCLUDE SPGENCLG                                                       
*PREFIX=P                                                                       
       ++INCLUDE PGENGRP                                                        
*PREFIX=                                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030DDOFFICER 11/27/18'                                      
         END                                                                    

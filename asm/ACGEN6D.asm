*          DATA SET ACGEN6D    AT LEVEL 042 AS OF 05/28/13                      
*PHASE T00A6DC                                                                  
BAT6D    TITLE 'BATCH FACILITIES - OVERLAY 3'                                   
BAT6D    CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
ROUT     NMOD1 250,**ROU3**,R8,R7,R6,CLEAR=YES                                  
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         LHI   R5,BSVALS-TWAD                                                   
         LA    R5,TWAD(R5)                                                      
         USING BSVALS,R5           R5=A(TWA SAVE AREA)                          
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OVRSCR              OVERLAY A SCREEN                             
         B     OVRPHS              LOAD A SUB-CONTROLLER PHASE                  
         B     OVRBIT              LOAD A BATCH INPUT TYPE OVERLAY              
         B     FVAL                VALIDATE AN INPUT FIELD                      
         B     IOEX                DISK I/O                                     
         B     READ                READ ACCOUNT FILE RECORD                     
         B     READL               READ & LOCK ACCOUNT FILE RECORD              
         B     RDHI                READ HIGH ACCOUNT FILE RECORD                
         B     RDHIL               READ HIGH & LOCK ACCOUNT FILE                
         B     WRITE               WRITE ACCOUNT FILE RECORD                    
         B     ADD                 ADD ACCOUNT FILE RECORD                      
         B     SEQ                 READ SEQ ACCOUNT FILE                        
         B     SEQL                READ SEQ & LOCK ACCOUNT FILE                 
         B     NTRSES              PROGRAM SESSION SAVE    (UPLEVEL)            
         B     XITSES              PROGRAM SESSION RESTORE (DOWNLEVEL)          
         B     RECACT              SET NEW RECORD/ACTION WORDS                  
         B     TSARIO              INTERFACE TO TSAR                            
         B     SETMSK              SET VALID ACTION MASK FOR A RECORD           
         B     PUTRAC              PUT RECORD ACTIVITY ELS. INTO RECORD         
         B     GETRAC              GET RECORD ACTIVITY ELS. FROM RECORD         
         B     TSTTAX              TEST TAX FOR SE POSTINGS                     
         B     CHKBMO              CHECK BATCH MONTH AGAINST PROFILE            
         B     EDTRAT              EDIT OUT EXCHANGE RATE(S)                    
         B     DELGIN              DELETE GROUP INVOICE PASSIVES & TXS          
         B     GETGIN              GET NEXT GROUP INVOICE NUMBER                
         B     ORDAUD              UPDATE ORDER AUDIT RECORD                    
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY A SCREEN INTO TWA                                *         
*                                                                     *         
* NTRY - P1=AL1(SCREEN OVERLAY NUMBER),AL3(LOAD POINT)                *         
***********************************************************************         
*                                                                               
OVRSCR   ICM   R1,15,0(R1)         SET A(LOAD POINT)                            
         STCM  R1,8,BCBYTE1                                                     
         XC    BCPARM(16),BCPARM                                                
         STCM  R1,7,BCPARM+1       SET LOAD POINT                               
         MVI   BCPARM+4,C'R'                                                    
         MVC   BCPARM+5(2),=X'061B'                                             
         MVC   BCPARM+7(1),BCBYTE1 SET SCREEN OVERLAY NUMBER                    
         GOTO1 VCOLY,BCPARM                                                     
         BAS   RE,CHKOLY           TEST SCREEN LOADED OK                        
         BNE   ROUTH                                                            
         LA    R1,BASMSGH                                                       
         LA    RF,OSVALS-1                                                      
         TM    CSBIND8,TYPIXOVL    TEST EXTRA AREA FOR SCREEN                   
         BNO   *+8                                                              
         LA    RF,OSSAVE-1                                                      
         SR    RE,RE                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                SCREEN TOO LARGE                             
         CLI   BCBYTE1,HEADSCRN                                                 
         BE    OVRSCR02                                                         
         CLC   TWASCRN,BCBYTE1     TEST SCREEN SAME AS LAST                     
         BNE   *+10                                                             
         XC    1(2,R1),1(R1)       YES - DON'T SET BEFORE/AFTER                 
OVRSCR02 MVC   TWASCRN,BCBYTE1                                                  
         MVI   TWASCRF,0                                                        
         CR    RE,RE               SET CONDITION CODE TO EQUAL                  
OVRSCRX  B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOAD APPLICATION OVERLAY                                 *         
*                                                                     *         
* NTRY - R1=A(PHASE NUMBER TO BE LOADED)                              *         
***********************************************************************         
*                                                                               
OVRPHS   CLI   0(R1),0             ENSURE OVERLAY NUMBER IS RESOLVED            
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CSOVER,0(R1)                                                     
         GOTO1 VCOLY,BCPARM,(CSOVER,0),0,0                                      
         BAS   RE,CHKOLY           TEST OVERLAY LOADED OK                       
         BNE   ROUTH                                                            
         MVC   BCNTRYA,0(R1)       SET OVERLAY ADDRESS                          
OVRPHSX  B     ROUTE                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO LOAD BATCH INPUT TYPE OVERLAY                            *         
***********************************************************************         
*                                                                               
OVRBIT   GOTO1 VCOLY,BCPARM,(CSBITO,0),0,0                                      
         BAS   RE,CHKOLY           TEST OVERLAY LOADED OK                       
         BNE   ROUTH                                                            
         MVC   BONTRYA,0(R1)       SET OVERLAY ADDRESS                          
OVRBITX  B     ROUTE                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO CHECK IF OVERLAY IS LOADED OK AND BUILD ERROR MESSAGE    *         
* NTRY - R1=A(CALL OVERLAY PARAMETER LIST)                            *         
* EXIT - CC=EQUAL IF LOADED OK, CC=NOT EQUAL IF NOT OK AND ERROR SET  *         
***********************************************************************         
*                                                                               
CHKOLY   CLI   4(R1),FF            TEST PHASE LOADED OK                         
         BE    *+8                                                              
         CR    RE,RE                                                            
         BR    RE                                                               
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT AND PRE-VALIDATE A TWA INPUT FIELD               *         
*                                                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
*        FVMINL=MINIMUM INPUT FIELD LENGTH (ZERO=OPTIONAL FIELD)      *         
*        FVMAXL=MAXIMUM INPUT FIELD LENGTH (ZERO=MAXIMUM LENGTH)      *         
*        FVXTRA=NARRATIVE TO BE ATTACHED TO ERROR IF FIELD IS INVALID *         
*                                                                     *         
* EXIT - FVADDR=A(TWA FIELD HEADER)                                   *         
*        FVINDX=ZERO                                                  *         
*        FVSUBX=ZERO                                                  *         
*        FVMINL=ZERO                                                  *         
*        FVMAXL=ZERO                                                  *         
*        FVXTRA=SPACES                                                *         
*        FVIHDR=EXTRACTED INPUT FIELD HEADER (SEE FVIHDR IN WORKD)    *         
*        FVIFLD=EXTRACTED & SPACE FILLED INPUT FIELD                  *         
*        FVFIELD=SPECIAL FIELD TYPE (RECORD, ACTION ETC.)             *         
*        FVMSGNO=SET TO STANDARD ERROR NUMBER (SEE FVMSGNO EQUATES)   *         
*        CC=LOW IF FIELD IS NOT INPUT                                 *         
*        CC=EQUAL IF FIELD IS INPUT AND VALID                         *         
*        CC=HIGH IF INPUT TOO SHORT/LONG ETC.                         *         
***********************************************************************         
*                                                                               
FVAL     MVI   FVINDS,0                                                         
         LTR   R1,R1               TEST A(TWA FIELD HEADER) PASSED              
         BNZ   *+10                                                             
         LR    RF,R0               SET FIELD LENGTH IN RF                       
         B     FVAL4                                                            
         ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVI   FVOMTYP,0           RESET MESSAGE TYPE                           
         MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
*                                                                               
         MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         XC    FVIXHDR,FVIXHDR                                                  
         SR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         LA    R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    FVAL2                                                            
         LA    RE,1(R1,RF)                                                      
         SR    RE,R0                                                            
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
         LA    R0,L'FVIHDR+L'FVIHDR+1                                           
*                                                                               
FVAL2    SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         EX    RF,*+8              EXTRACT FIELD DATA                           
         B     *+10                                                             
         MVC   FVIFLD(0),L'FVIHDR(R1)                                           
*                                                                               
FVAL4    LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         LA    RF,1(RF)            RF=LOOP COUNT                                
FVAL6    CLI   0(R1),C' '          LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FVAL8                                                            
         MVI   0(R1),C' '          SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCT   RF,FVAL6                                                         
FVAL8    STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(FVFSHRT) ENSURE NOT TOO SHORT OR LONG               
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         CLM   RF,1,FVMINL                                                      
         BL    FVAL22                                                           
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         CLM   RF,1,FVMAXL                                                      
         BH    FVAL22                                                           
         NI    FVIIND,FF-FVINUM-FVIALF-FVIHEX                                   
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     FVAL20                                                           
*                                  SET FIELD VALIDITY BITS                      
         MVC   FVMSGNO,=AL2(FVFOK) INDICATE FIELD IS OK                         
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
FVAL10   TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FVAL14                                                           
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,FF-FVINUM-FVIALF-FVIHEX                                   
         B     FVAL12                                                           
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,FF-FVIALF                                                 
         B     FVAL12                                                           
         NI    FVIIND,FF-FVINUM                                                 
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,FF-FVIHEX                                                 
FVAL12   BCTR  R1,0                                                             
         BCT   RF,FVAL10                                                        
FVAL14   IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         TM    FVIIND,FVINUM                                                    
         BZ    FVAL16                                                           
         CLI   FVILEN,8            TEST INPUT NOT LONGER THAN 8 BYTES           
         BNH   *+12                                                             
         NI    FVIIND,FF-FVINUM                                                 
         B     FVAL16                                                           
         EX    RF,*+8              SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  BCDUB,FVIFLD(0)                                                  
         CVB   R0,BCDUB                                                         
         ST    R0,BCFULL                                                        
FVAL16   MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   FVIFLD(1),BCQUEST   TEST IF QUESTION MARK WAS INPUT              
         BE    FVAL18                                                           
         CLI   FVILEN,2                                                         
         BL    FVAL20                                                           
         CLI   FVILEN,L'UC@HELP                                                 
         BH    FVAL20                                                           
         EX    RF,*+8                                                           
         BE    FVAL18                                                           
         CLC   FVIFLD(0),UC@HELP   OR 'HE(LP)' WAS INPUT                        
         B     FVAL20                                                           
*                                                                               
FVAL18   OI    FVINDS,FVIHELP      SET HELP REQUESTED                           
         CLI   FVFIELD,0           TEST SPECIAL INPUT FIELD                     
         BNE   HELP                                                             
*                                                                               
FVAL20   MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVXTRA,C' '                                                      
         MVC   FVXTRA+1(L'FVXTRA-1),FVXTRA                                      
*                                                                               
FVAL22   TM    TWAINDS1,TWAIHELP   TEST HELP ON THIS FIELD LAST TIME            
         BZ    *+14                                                             
         CLC   FVFIELD,TWAHELPT                                                 
         BE    HELP                                                             
         MVI   FVFIELD,0                                                        
         CLC   FVMSGNO,=AL2(FVFNONE)                                            
         BE    FVAL24                                                           
         MVI   FVFLAG,0                                                         
         CLI   FVILEN,0                                                         
         BE    FVALX                                                            
         MVI   FVFLAG,1                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    FVALX                                                            
FVAL24   MVI   FVFLAG,2                                                         
FVALX    CLI   FVFLAG,1            SET CONDITION CODE FOR CALLER                
         MVI   FVFLAG,0                                                         
         B     ROUTX               RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* DO HELP FOR SPECIAL FIELDS (RECORD, ACTION ETC.)                    *         
***********************************************************************         
*                                                                               
HELP     TM    TWAINDS1,TWAIOHIP   EXIT FROM OPTION HELP IF NECESSARY           
         BZ    HELP1                                                            
         GOTO1 AXITSES                                                          
         NI    TWAINDS1,FF-(TWAIOHIP+TWAIOHRE)                                  
         SR    RE,RE                                                            
         ICM   RE,3,BCDSPOPT                                                    
         LA    RE,TWAD(RE)                                                      
         CLC   BCQUEST,L'FVIHDR(RE)                                             
         BE    HELP1                                                            
         MVC   L'FVIHDR(L'TWAOSAVE,RE),TWAOSAVE                                 
         OI    FVOIND-FVIHDR(RE),FVOXMT                                         
*                                                                               
HELP1    CLI   TWAHELPT,0          TEST HELP IN PROGRESS                        
         BE    HELP6                                                            
         CLC   TWAHELPT,FVFIELD    TEST HELP FOR SAME FIELD AS LAST             
         BNE   HELP6                                                            
         TM    BCINDS1,BCINREC+BCINACT                                          
         BNZ   HELP6                                                            
         SR    R0,R0                                                            
         ICM   R0,3,TWAHELPD                                                    
         CLM   R0,3,BCEFFS         TEST LAST HELP ON SCREEN                     
         BE    HELP1A                                                           
         SR    R3,R3                                                            
         ICM   R3,3,BCDSPOVR                                                    
         LA    R3,TWAD(R3)                                                      
         USING HELPD,R3                                                         
         TWAXC HELPL1H,PROT=Y                                                   
         B     HELP10                                                           
*                                                                               
HELP1A   TM    TWAINDS1,TWAIHELP   RESTORE SAVED SCREEN (IF SET)                
         BZ    HELP2                                                            
         GOTO1 AXITSES             RESTORE SAVED SESSION                        
*                                                                               
HELP2    L     R1,FVADDR                                                        
         OI    FVOIND-FVIHDR(R1),FVOXMT+FVOCUR                                  
         TM    FVINDS,FVIHELP      TEST NEED TO RESET INPUT FIELD               
         BZ    HELP4                                                            
         SR    RE,RE               RESTORE SAVED FIELD VALUE TO TWA             
         ICM   RE,3,TWAHELPF                                                    
         LA    RE,WORKD(RE)        RE=A(SAVED FIELD VALUE)                      
         SR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB,FVAXTND                                                   
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R1),0(RE)                                             
*                                                                               
HELP4    NI    TWAINDS1,FF-TWAIHELP                                             
         XC    TWAHELPV(TWAHELPL),TWAHELPV                                      
         XC    BASMSG,BASMSG       CLEAR & TRANSMIT MESSAGE FIELD               
         OI    BASMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R0,BSVALS           SAVE GLOBAL W/S VALUES IN TWA0               
         LA    RE,BCVALS                                                        
         LA    R1,BCVALSL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RD,BCSVRD           EXIT TO FACPAK                               
         B     ROUTX                                                            
*                                                                               
HELP6    MVC   TWAHELPT,FVFIELD    SET HELP FIELD NUMBER                        
         MVC   TWAHELPF,FVSAVE     SET DISPLACEMENT TO SAVED FIELD              
*                                                                               
         TM    TWAINDS1,TWAIHELP   RESTORE SAVED SCREEN (IF SET)                
         BNZ   HELP8                                                            
         GOTO1 ANTRSES,0           SAVE SESSION                                 
         OI    TWAINDS1,TWAIHELP   RESTORE SAVED SCREEN (IF SET)                
*                                                                               
HELP8    SR    R3,R3                                                            
         ICM   R3,3,BCDSPOVR                                                    
         LA    R3,TWAD(R3)                                                      
         GOTO1 AOVRSCR,BCPARM,(BCHELPSO,HELPD)                                  
         BNE   ROUTH                                                            
         SR    R2,R2                                                            
         IC    R2,TWAHELPT                                                      
         SLL   R2,2                                                             
         LA    R2,HELPHEAD-L'HELPHEAD(R2)                                       
*                                                                               
         LA    R1,BCPARM                                                        
         USING GETTXTD,R1          USE GETTXT FOR HELP HEADINGS                 
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,0(R2)                                                    
         MVI   GTMTYP,GTMSCR                                                    
         LA    R0,HELPH1H                                                       
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT,GETTXTD                                                  
*                                                                               
         MVC   GTMSGNO,2(R2)                                                    
         LA    R0,HELPH2H                                                       
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMAXL,0                                                         
         MVI   GT1INDS,0                                                        
         MVI   GT2INDS,0                                                        
         BASR  RE,RF                                                            
         DROP  R1                                                               
         SR    R0,R0               SET START OF HELP                            
*                                                                               
HELP10   SR    RF,RF                                                            
         IC    RF,TWAHELPT                                                      
         SLL   RF,2                                                             
         LA    R3,HELPL1H                                                       
         USING HELPL1H,R3          R3=A(CURRENT OUTPUT LINE)                    
         B     *(RF)                                                            
*                                                                               
         B     HELPREC             RECORD HELP                                  
         B     HELPACT             ACTION HELP                                  
         B     HELPOPT             OPTION HELP                                  
         B     HELPSEL             SELECT HELP                                  
*                                                                               
HELPHEAD DS    0XL4                TEXT NUMBERS FOR HELP HEADINGS               
HELPHREC DC    AL2(2000,2001)                                                   
HELPHACT DC    AL2(2100,2101)                                                   
HELPHOPT DC    AL2(2200,2201)                                                   
HELPHSEL DC    AL2(2300,2301)                                                   
         EJECT                                                                  
***********************************************************************         
* HELP FOR RECORD TYPE                                                *         
***********************************************************************         
*                                                                               
HELPREC  L     R2,ARECTAB                                                       
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
         ST    R2,BCFULL           SAVE A(TABLE)                                
         AR    R2,R0               ADD DISPLACEMENT TO NEXT ENTRY               
*                                                                               
HELPREC2 CLI   RECTABD,EOT         TEST END OF TABLE                            
         BE    HELPX                                                            
         TM    RECINDS1,RECINOH    TEST DON'T DISPLAY HELP                      
         BNZ   HELPREC4                                                         
         GOTO1 ATSTREC,RECNUMB     TEST RECORD VALID                            
         BNE   HELPREC4                                                         
         CLI   HELPL1H+(FVTLEN-FVIHDR),HELPL1L                                  
         BNE   HELPX               EXIT IF END OF TWA REACHED                   
         MVC   HELPL1(RECNAMLQ),BCWORK                                          
         LA    R1,BCPARM                                                        
         USING GETTXTD,R1          BUILD GETTXT BLOCK                           
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,RECHELP                                                  
         MVI   GTMAXL,RECHELPL                                                  
         LA    R0,HELPL1+RECNAMLQ+3                                             
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT             GET HELP TEXT                                
         LA    R3,HELPL1L(R3)      BUMP TO NEXT TWA LINE                        
*                                                                               
HELPREC4 LA    R2,RECTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     HELPREC2                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* HELP FOR ACTION                                                     *         
***********************************************************************         
*                                                                               
HELPACT  L     R2,AACTTAB                                                       
         USING ACTTABD,R2          R2=A(ACTION TABLE)                           
         ST    R2,BCFULL           SAVE A(TABLE)                                
         AR    R2,R0               ADD DISPLACEMENT TO NEXT ENTRY               
*                                                                               
HELPACT2 CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BE    HELPX                                                            
         TM    ACTINDS1,ACTINOH    TEST DON'T DISPLAY HELP                      
         BNZ   HELPACT6                                                         
         GOTO1 ATSTACT,ACTNUMB     TEST ACTION AUTHORISATION                    
         BNE   HELPACT6                                                         
         MVC   BCBYTE1,CSREC                                                    
         ICM   RF,15,ARECNTRY                                                   
         BZ    *+10                                                             
         MVC   BCBYTE1(L'RECNUMB),RECNUMB-RECTABD(RF)                           
         MVC   BCBYTE2(L'ACTNUMB),ACTNUMB                                       
         GOTO1 ATSTMIX,BCBYTE1     TEST COMBO AUTHORISATION                     
         BNE   HELPACT6                                                         
         L     RF,AMIXNTRY                                                      
         USING MIXTABD,RF          RF=A(RECORD TYPE/ACTION TABLE)               
         TM    MIXINDS1,MIXISEL    TEST SELECT ACTION                           
         BNZ   HELPACT6                                                         
         TM    MIXINDS1,MIXINOH    TEST DO NOT GIVE HELP                        
         BNZ   HELPACT6                                                         
         CLI   HELPL1H+(FVTLEN-FVIHDR),HELPL1L                                  
         BNE   HELPX               EXIT IF END OF TWA REACHED                   
         MVC   HELPL1(ACTNAMLQ),BCWORK                                          
         LA    R1,BCPARM                                                        
         USING GETTXTD,R1          BUILD GETTXT BLOCK                           
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,MIXHELP                                                  
         MVI   GTMAXL,MIXHELPL                                                  
         LA    R0,HELPL1+ACTNAMLQ+3                                             
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT             GET HELP TEXT                                
*                                                                               
HELPACT4 LA    R3,HELPL1L(R3)      BUMP TO NEXT SCREEN LINE                     
*                                                                               
HELPACT6 LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     HELPACT2                                                         
         DROP  R2                                                               
*                                                                               
HELPOPT  DS    0H                                                               
HELPSEL  DS    0H                                                               
         B     HELPX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EXIT WITH MESSAGE                                                   *         
***********************************************************************         
*                                                                               
HELPX    MVC   TWAHELPD,BCEFFS                                                  
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         MVC   FVMSGNO,=AL2(GI$HELP0)                                           
         CLI   0(R2),EOT           TEST END OF TABLE                            
         BE    HELPX2                                                           
         S     R2,BCFULL                                                        
         STCM  R2,3,TWAHELPD       SET DISPLACEMENT TO NEXT TABLE ENTRY         
         MVC   FVMSGNO,=AL2(GI$HELP1)                                           
*                                                                               
HELPX2   B     ROUTH                                                            
*                                                                               
GI$HELP0 EQU   100+X'FF00'                                                      
GI$HELP1 EQU   101+X'FF00'                                                      
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO ANY STANDARD SYSTEM FILE                 *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES (LOW ORDER 2 BYTES) SET FROM IO EQUATES *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSAV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)           *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSAV *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
*                                                                               
         USING IOWORKD,RC                                                       
IOEX     ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOFLAG,0            RESET I/O FLAG BYTE                          
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IOCTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         TM    IOCTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
*                                                                               
         LA    R1,IO1+IO2          ESTABLISH I/O AREA ADDRESS (IO1-IO3)         
         N     R1,IOCTRL                                                        
         BZ    IOEX02                                                           
         SRL   R1,6                R1=I/O AREA NUMBER                           
         B     IOEX04                                                           
*                                                                               
IOEX02   TM    IOCTRL+2,X'F0'      ESTABLISH I/O AREA ADDRESS (IO4-IOA)         
         BZ    IOEX2                                                            
         IC    R1,IOCTRL+2                                                      
         SRL   R1,4                                                             
*                                                                               
IOEX04   STC   R1,IODUB                                                         
         CLI   IODUB,X'0A'         ONLY 10 I/O AREAS SUPPORTED HERE             
         BH    IOEX06                                                           
         BCTR  R1,0                                                             
         MH    R1,=Y(IOAREALN)                                                  
         AH    R1,=Y(IOAREA1-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,IOAREAD          SAVE I/O AREA ADDRESS                        
*                                                                               
         LA    R1,L'IODA+L'IOWORK(R1)                                           
         STCM  R1,15,IOADDR        SET REAL I/O ADDRESS                         
         B     IOEX2                                                            
*                                                                               
IOEX06   CLI   IODUB,X'0B'         SPECIAL FOR IOREC                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOREC                                                        
         ST    R1,IOADDR           I/O AREA REAL ADDRESS                        
*                                                                               
         SH    R1,=Y(L'IODA+L'IOWORK)                                           
         STCM  R1,15,IOAREAD       I/O AREA WORK AREA                           
*                                                                               
IOEX2    LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IOEX4                                                            
         OC    IOFILE,IOFILE       CALLER MUST SUPPLY FILE NAME                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX20                                                           
*                                                                               
IOEX4    SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILNTRY         POINT TO LOCAL SYSTEM FILES                  
         LA    R0,10                                                            
         CR    R1,R0                                                            
         BNH   *+8                                                              
         L     RE,ASYSTAB          POINT TO GLOBAL SYSTEM FILES                 
*                                                                               
         USING FILTABD,RE                                                       
IOEX6    CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX6                                                            
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IOFILNM,IOFILNM     TEST NATIVE SYSTEM FILE                      
         BZ    IOEX600                                                          
         GOTO1 IOSWITCH,BCSWSYSN   SWITCH TO NATIVE SYSTEM IF REQUIRED          
         BNE   IOEXX                                                            
         B     IOEX7                                                            
*                                                                               
IOEX600  MVC   IOSWSYS(L'IOSWSYS+L'IOSWFIL),FILSYSN                             
         L     RE,AFILTAB                                                       
         SR    R1,R1                                                            
IOEX602  CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,RE),IOSWSYS     MATCH ON SYSTEM SWITCH NUMBER                
         BE    *+16                                                             
         ICM   R1,3,4(RE)                                                       
         LA    RE,5(R1,RE)                                                      
         B     IOEX602                                                          
         MVC   IOSWSYSN,1(RE)      SAVE SWITCH-TO SYSTEM NAME                   
         LA    RE,6(RE)            POINT TO FIRST FILE ENTRY                    
IOEX604  CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLC   FILNUM,IOSWFIL      MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX604                                                          
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IOFILNM,IOFILNM     TEST NATIVE FILE TO THIS SYSTEM              
         BNZ   *+6                                                              
         DC    H'0'                NO - KILL THE APPLICATION                    
         GOTO1 IOSWITCH,IOSWSYS    SWITCH TO CORRECT SYSTEM                     
         BE    IOEX7                                                            
         GOTO1 IOSW,BCSWSYSN       CAN'T SWITCH - SWITCH BACK TO NATIVE         
         MVI   IOERR,FF            SET SWITCH FAILURE ERROR BITS                
         B     IOEXX2                                                           
*                                                                               
IOEX7    L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IOEX8                                                            
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IOEX20                                                           
*                                                                               
IOEX8    CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)                                                   
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     IOEX10                                                           
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOEX8                                                            
*                                                                               
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
IOEX10   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOEX10                                                           
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
*                                                                               
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOEX16                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX18                                                           
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IOEX11                                                           
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         XC    IODAOVER,IODAOVER                                                
         B     IOEX16                                                           
*                                                                               
IOEX11   ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    IOEX14                                                           
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    0(L'IODA,R1),0(R1)                                               
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         LA    R1,L'IODA(R1)                                                    
*                                                                               
IOEX12   OC    0(L'IOWORK,R1),0(R1)                                             
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
         LA    R1,L'IOWORK(R1)                                                  
*                                                                               
IOEX14   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX16                                                           
*                                                                               
         TM    IOFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         SR    R1,R1                                                            
         IC    R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTO1 AIO                 RECURSE FOR DIRECTORY I/O                    
         BE    IOEX16              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
*                                                                               
IOEX16   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO I/O AREA ADDRESS                          
         MVC   IOFILE,IOFILNM      SET FILE NAME IN WORK AREA                   
         GOTO1 VDMGR,BCPARM,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK              
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         MVC   0(L'IODA,R1),IODA                                                
         LA    R1,L'IODA(R1)       YES - BUMP BY D/A LENGTH                     
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOEXX               EXIT TO CALLER                               
*                                                                               
IOEX18   TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX20                                                           
         MVC   IOFILE,IOFILNM      SET FILE NAME IN WORK AREA                   
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,FILIID      TEST I/S FILE HAS D/A ATTACHED               
         BZ    *+12                YES - MUST READ INTO IOAREA                  
         TM    IOFILI,FILIVL                                                    
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,BCPARM,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
         TM    IOFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEX182                                                          
         SR    R1,R1                                                            
         IC    R1,IOFILKL          YES - EXTRACT DISK ADDRESS                   
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)        POINT TO DISK ADDRESS                        
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         MVC   0(L'IODA,R1),IODA                                                
         B     IOEXX                                                            
*                                                                               
IOEX182  ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         LA    R1,L'IODA(R1)       YES - BUMP BY D/A LENGTH                     
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOEXX                                                            
*                                                                               
IOEX20   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,BCPARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEXX    TM    IOINDS1,IOISWAUT    TEST AUTO SWITCH BACK AFTER I/O              
         BZ    IOEXX2                                                           
         TM    IOFLAG,IOFSWTCH     TEST SYSTEM SWITCH OCCURRED                  
         BZ    IOEXX2                                                           
         GOTO1 IOSWITCH,BCSWSYSP   SWITCH TO PREVIOUS SYSTEM                    
*                                                                               
IOEXX2   MVI   IOQ,1               SET I/O COMPLETED OK                         
         TM    IOERR,IOERRS                                                     
         BZ    IOEXXX                                                           
         MVI   IOQ,2               SET LOGICAL I/O ERROR                        
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   IOEXXX                                                           
         MVI   IOQ,0               SET IRRECOVERABLE ERROR                      
*                                                                               
IOEXXX   CLI   IOQ,1               SET CONDITION CODE FOR CALLER                
         B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SWITCH TO A SYSTEM                                       *         
*                                                                     *         
* NTRY - R1=A(LOGICAL SYSTEM NUMBER)                                  *         
* EXIT - CC=LOW   - USER NOT AUTHORISED FOR SYSTEM                    *         
*        CC=EQUAL - SWITCH SUCCESSFUL                                 *         
*        CC=HIGH  - SYSTEM NOT AVAILABLE (ONLINE ONLY)                *         
* NOTE - IF ERROR OCCURRS IOERR IS SET TO X'FF' WHICH WILL RETURN A   *         
*        CC OF HIGH FROM I/O ROUTINE WITH FVMSGNO SET TO FVFIOER. IT  *         
*        IS THE CALLER'S RESPONSIBILITY TO DEAL WITH THIS OTHERWISE   *         
*        A RANDOM DATAMGR ERROR WILL BE REPORTED.                     *         
***********************************************************************         
*                                                                               
IOSWITCH CLC   BCSWSYSC,0(R1)      TEST SWITCHED TO CORRECT SYSTEM              
         BER   RE                                                               
IOSW     LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   IOBYTE,0(R1)        SAVE SYSTEM NUMBER                           
         CLI   0(R1),10            TEST SWITCH TO CONTROL SYSTEM                
         BE    IOSW4                                                            
         L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LA    RF,SYSSWMAX                                                      
IOSW2    CLC   SYSSWSOV,IOBYTE     MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   *+12                                                             
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         B     IOSW4                                                            
         LA    RE,SYSSWLEN(RE)     BUMP TO NEXT SWITCH TABLE ENTRY              
         BCT   RF,IOSW2                                                         
         MVI   IOBYTE,0            SET CC=LOW FOR INVALID SYSTEM                
         B     IOSWX                                                            
*                                                                               
IOSW4    CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   IOSW6                                                            
         GOTOR VPROTOFF            TURN OFF PROTECTION                          
         ICM   RF,15,BCAUTL        YES - MOVE SE NUMBER TO UTL                  
         MVC   TSYS-UTLD(,RF),0(R1)                                             
         GOTOR VPROTON             TURN PROTECTION ON                           
         B     IOSW8                                                            
*                                                                               
IOSW6    MVC   BCPARM(1),0(R1)     SWITCH TO A SYSTEM                           
         MVC   BCPARM+1(3),BCEFFS                                               
         XC    BCPARM+4(4),BCPARM+4                                             
         GOTO1 VSWITCH,BCPARM                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    IOSW8                                                            
         MVI   IOBYTE,2            SET CC=HIGH FOR CAN'T SWITCH                 
         B     IOSWX                                                            
*                                                                               
IOSW8    MVC   BCSWSYSP,BCSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   BCSWSYSC,IOBYTE     SAVE CURRENT SYSTEM NUMBER                   
         OI    IOFLAG,IOFSWTCH     SET SYSTEM SWITCH OCCURRED                   
*                                                                               
IOSW10   MVI   IOBYTE,1            SET CC=EQUAL FOR OK                          
*                                                                               
IOSWX    CLI   IOBYTE,1            SET CC FOR CALLER                            
         BE    *+8                                                              
         MVI   IOERR,FF            SET ALL ERROR BITS ON                        
         LR    RE,R0                                                            
         BR    RE                  RETURN TO CALLER                             
         DROP  RC,RE                                                            
*                                                                               
IOWORKD  DSECT                     ** IOEX S/R LOCAL W/S **                     
IODUB    DS    D                   GENERAL WORK AREA                            
IOAREAD  DS    A                   I/O AREA ADDRESS                             
IOCTRL   DS    F                   I/O COMMAND WORD                             
IOBYTE   DS    X                   I/O BYTE                                     
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILNM  DS    CL7                 FILE NAME                                    
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOSWSYS  DS    XL1                 SWITCH SYSTEM NUMBER                         
IOSWFIL  DS    XL1                 SWITCH FILE NUMBER                           
IOSWSYSN DS    CL3                 SWITCH SYSTEM NAME                           
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO EMULATE OLD I/O ROUTINES                                *         
***********************************************************************         
*                                                                               
READ     DS    0H                  ** READ ACCOUNT **                           
         GOTO1 AIO,IORD+IOACCFIL+IO1+IORDEL                                     
         B     ROUTX                                                            
*                                                                               
READL    DS    0H                  ** READ & LOCK ACCOUNT **                    
         GOTO1 AIO,IORDUP+IOACCFIL+IO1+IORDEL                                   
         B     ROUTX                                                            
*                                                                               
RDHI     DS    0H                  ** READ HIGH ACCOUNT **                      
         GOTO1 AIO,IOHI+IOACCFIL+IO1+IORDEL                                     
         B     ROUTX                                                            
*                                                                               
RDHIL    DS    0H                  ** READ HIGH & LOCK ACCOUNT **               
         GOTO1 AIO,IOHIUP+IOACCFIL+IO1+IORDEL                                   
         B     ROUTX                                                            
*                                                                               
WRITE    DS    0H                  ** WRITE ACCOUNT **                          
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         B     ROUTX                                                            
*                                                                               
ADD      DS    0H                  ** ADD ACCOUNT **                            
         GOTO1 AIO,IOADD+IOACCFIL+IO1                                           
         B     ROUTX                                                            
*                                                                               
SEQ      DS    0H                  ** READ SEQUENTIAL ACCOUNT **                
*&&US*&& GOTO1 AIO,IOSQ+IOACCFIL+IO1                                            
*&&UK*&& GOTO1 AIO,IOSQ+IOACCFIL+IO1+IORDEL                                     
         B     ROUTX                                                            
*                                                                               
SEQL     DS    0H                  ** READ SEQ & LOCK ACCOUNT **                
*&&US*&& GOTO1 AIO,IOSQUP+IOACCFIL+IO1                                          
*&&UK*&& GOTO1 AIO,IOSQUP+IOACCFIL+IO1+IORDEL                                   
         B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE CURRENT SESSION TO NEXT SESSION SAVE AREA                      *         
*                                                                     *         
* NTRY - TWASESNL=CURRENT LEVEL OF NESTING                            *         
*      - R1=A(SESSION PARAMETERS)                                     *         
* EXIT - TWASESNL=NEXT LEVEL                                          *         
***********************************************************************         
*                                                                               
NTRSES   LR    R3,R1               SAVE SESSION PARAMETERS                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,TWASESNL                                                      
         LA    RE,1(RE)            INCREMENT NEXT LEVEL                         
         CLM   RE,1,=AL1(TWASESMX)                                              
         BNH   *+6                                                              
         DC    H'0'                MAXIMUM NEST LEVEL EXCEEDED                  
         STC   RE,TWASESNL                                                      
         LA    RE,1(RE)                                                         
         SRDL  RE,1                                                             
         STC   RE,BCBYTE1          SAVE ABSOLUTE TEMPSTR PAGE NUMBER            
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   0(L'TWASESRA,RE),CSRECACT                                        
         OC    CSINITRA,CSINITRA                                                
         BZ    *+10                                                             
         MVC   0(L'TWASESRA,RE),CSINITRA                                        
*                                                                               
         XC    BCHALF,BCHALF                                                    
         LTR   RF,RF                                                            
         BNZ   NTRSES02                                                         
         L     R0,ATIA             CLEAR TEMPSTR PAGE                           
         LH    R1,=Y(TWAMAXRL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     NTRSES04                                                         
*                                                                               
NTRSES02 MVC   BCHALF,=Y(TWAMAXRL/2)                                            
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(TWAMAXRL)                                                
         GOTO1 VDMGR,BCPARM,DMREAD,TEMPSTR,(BCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NTRSES04 L     R2,ATIA             SAVE START ADDRESS INTO TIA                  
         AH    R2,BCHALF                                                        
         USING SESD,R2             R2=A(SESSION SAVE AREA)                      
         USING SELTPARM,R3         R3=A(NTRSES PARAMETERS)                      
         MVI   SESROUT,0                                                        
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    *+10                                                             
         MVC   SESROUT,SELTRTN     SET EXIT ROUTINE NUMBER                      
*                                                                               
         MVC   SESSCRN,TWASCRN     SAVE GLOBAL VALUES                           
         MVC   SESSCRF,TWASCRF                                                  
*                                                                               
         LA    R0,SESOSSV          SAVE CURRENT OVERLAY SAVE VALUES             
         LA    R1,SESOSSVL                                                      
         LA    RE,OSVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESO2SV          SAVE CURRENT OVERLAY SAVE VALUES 2           
         LA    R1,SESO2SVL                                                      
         LA    RE,TWAD                                                          
         AH    RE,=Y(OSVALS2-TWAD)                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESCSSV          SAVE CURRENT SESSION VALUES                  
         LA    R1,SESCSSVL                                                      
         LA    RE,CSVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,BCAUTL                                                        
         MVC   SESSCRSV,TSCRNE-UTLD(RF)                                         
*                                                                               
         LA    R0,SESTWSV          SAVE ENTIRE SCREEN                           
         LA    R1,SESTWSVL                                                      
         LA    RE,BASOPTH                                                       
         LA    RF,OSVALS-BASOPTH                                                
         MVCL  R0,RE                                                            
*                                                                               
         XC    CSINITRA,CSINITRA   CLEAR INITIATOR RECORD/ACTION                
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         SR    RF,RF               SAVE CURRENT SESSION LSTTAB ENTRY            
         ICM   RF,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    NTRSES06                                                         
         MH    RF,=Y(LSTTABL)                                                   
         LA    RF,BCLSTCUR-LSTTABL(RF)                                          
         MVC   0(LSTTABL,RF),CSLSTCUR                                           
*                                                                               
NTRSES06 GOTO1 VDMGR,BCPARM,DMWRITE,TEMPSTR,(BCBYTE1,0),ATIA                    
*                                                                               
         TM    CSINDSL2,CSIOVKEP   TEST KEEPING OVERLAY VALUES                  
         BO    NTRSES08                                                         
         LA    R0,OSVALS           CLEAR OSVALS                                 
         LA    R1,OSVALSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
NTRSES08 OC    BASOPT,BASOPT       TEST ANYTHING IN OPTIONS FIELD               
         BZ    *+14                                                             
         XC    BASOPT,BASOPT                                                    
         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         MVC   CSPSRECN,CSHIRECN   SET NEXT SESSION LOW TSAR RECORD#            
*                                                                               
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    NTRSESX                                                          
         MVC   CSOIND1,SELTNSI1    SET NEXT SESSION INDICATORS                  
         MVC   CSOIND2,SELTNSI2                                                 
         OC    SELTRECA,SELTRECA   TEST RECORD/ACTION PASSED                    
         BZ    NTRSESX                                                          
*                                                                               
         L     R0,AOVERWRK         CLEAR OVERWRK                                
         LH    R1,=Y(OVERWRKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ARECACT,SELTREC     SET ENTRY RECORD/ACTION NAMES                
         L     R1,AMIXNTRY         SET NEXT SESSION CSLSTCUR                    
         SR    RF,RF                                                            
         ICM   RF,1,MIXNTREC-MIXTABD(R1)                                        
         BZ    NTRSES12                                                         
         MH    RF,=Y(LSTTABL)                                                   
         LA    RF,BCLSTCUR-LSTTABL(RF)                                          
         MVC   CSLSTCUR(LSTTABL),0(RF)                                          
*                                                                               
NTRSES12 CLI   SELTNXPF,0          TEST AUTO RETURN PFKEY SET                   
         BE    *+10                                                             
         MVC   CSNEXTPF,SELTNXPF   YES - SET VALUE                              
         OI    TWAINDS1,TWAINTRS   SET NTRSES ISSUED                            
         XC    CSINDSL,CSINDSL                                                  
         MVI   CSINDSL1,CSIUSELC                                                
         XC    CSINDSG,CSINDSG                                                  
         L     RD,BCSVRD                                                        
         L     RD,8(RD)                                                         
*                                                                               
NTRSESX  B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RESTORE PREVIOUS SESSION                                            *         
*                                                                     *         
* NTRY - TWASESNL=CURRENT NESTING LEVEL                               *         
* EXIT - TWASESNL=PREVIOUS NESTING LEVEL                              *         
***********************************************************************         
*                                                                               
         USING XSWORKD,RC                                                       
XITSES   MVC   XSNXRECN,CSHIRECN                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,TWASESNL                                                      
         BCTR  RE,0                DECREMENT NEXT LEVEL                         
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                MINIMUM NEST LEVEL EXCEEDED                  
         STC   RE,TWASESNL                                                      
         LA    RE,2(RE)                                                         
         SRDL  RE,1                                                             
         STC   RE,BCBYTE1          SAVE ABSOLUTE TEMPSTR PAGE NUMBER            
         L     R2,ATIA                                                          
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         AH    R2,=Y(TWAMAXRL/2)                                                
         USING SESD,R2             R2=A(SESSION SAVE AREA)                      
*                                  READ SAVED TEMPSTR PAGE                      
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(TWAMAXRL)                                                
         GOTO1 VDMGR,BCPARM,DMREAD,TEMPSTR,(BCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF               SAVE CURRENT SESSION LSTTAB ENTRY            
         ICM   RF,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    XITSES02                                                         
         MH    RF,=Y(LSTTABL)                                                   
         LA    RF,BCLSTCUR-LSTTABL(RF)                                          
         MVC   0(LSTTABL,RF),CSLSTCUR                                           
*                                                                               
XITSES02 TM    CSINDSL2,CSIOVKEP   TEST KEEPING OVERLAY VALUES                  
         BO    XITSES03                                                         
         LA    R0,OSVALS           RESTORE OVERLAY SAVE VALUES                  
         LA    R1,SESOSSVL                                                      
         LA    RE,SESOSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,TWAD             RESTORE OVERLAY SAVE AREA 2                  
         AH    R0,=Y(OSVALS2-TWAD)                                              
         LA    R1,SESO2SVL                                                      
         LA    RE,SESO2SV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,CSVALS           RESTORE SESSION VALUES                       
         LA    R1,SESCSSVL                                                      
         LA    RE,SESCSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
XITSES03 MVC   CSNXRECN,XSNXRECN   SET NEXT SESSION HIGH RECORD                 
*                                                                               
         GOTOR VPROTOFF            TURN PROTECTION OFF                          
         L     RF,BCAUTL                                                        
         MVC   TSCRNE-UTLD(L'SESSCRSV,RF),SESSCRSV                              
         GOTOR VPROTON             TURN PROTECTION ON                           
*                                                                               
         MVC   TWASCRN,SESSCRN     RESTORE GLOBAL VALUES                        
         MVC   TWASCRF,SESSCRF                                                  
*                                                                               
         LA    R0,BASOPTH          RESTORE TWA                                  
         LA    R1,OSVALS-BASOPTH                                                
         LA    RE,SESTWSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   TWAHELPT,0                                                       
         BNE   *+12                                                             
         LA    R1,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         LA    R0,BASOLY1H         R0=A(OVERLAY SCREEN START)                   
         LA    R1,BASMSGH          TRANSMIT SCREEN & TURN OFF CURSORS           
         LA    RF,OSVALS-1                                                      
         TM    CSBIND8,TYPIXOVL    TEST EXTRA AREA FOR SCREEN                   
         BNO   *+8                                                              
         LA    RF,OSSAVE-1                                                      
         SR    RE,RE                                                            
XITSES04 CLI   FVTLEN-FVIHDR(R1),0                                              
         BE    XITSES08                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         NI    FVOIND-FVIHDR(R1),FF-FVOCUR                                      
         CR    R1,R0               TEST INTO THE OVERLAY SCREEN AREA            
         BL    XITSES06                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   XITSES05                                                         
         CLI   TWASCRN,BTS46       DON'T CHANGE INPUT STATUS                    
         BE    XITSES05            FOR TYPE 46                                  
         CLI   TWASCRN,BTS46C                                                   
         BE    XITSES05                                                         
         CLI   TWASCRN,BTS01C      OR CANADIAN TYPES 1 AND 3                    
         BE    XITSES05                                                         
         CLI   TWASCRN,BTS03C                                                   
         BE    XITSES05                                                         
         NI    FVIIND-FVIHDR(R1),FF-FVIVAL                                      
XITSES05 CLI   TWAHELPT,0                                                       
         BNE   XITSES06                                                         
         C     R0,FVADDR           TEST A(OVERLAY SCREEN FIELD) SET             
         BNH   XITSES06                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   XITSES06                                                         
         ST    R1,FVADDR           A(FIRST UNPROT OVERLAY SCREEN FIELD)         
XITSES06 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+8                                                              
         BXLE  R1,RE,XITSES04                                                   
XITSES08 MVI   1(R1),1             SET CLEAR BEFORE AND AFTER                   
         MVI   2(R1),1                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    XITSES10                                                         
         MH    RF,=Y(LSTTABL)                                                   
         LA    RF,BCLSTCUR-LSTTABL(RF)                                          
         MVC   CSLSTCUR(LSTTABL),0(RF)                                          
*                                                                               
XITSES10 MVI   BCBYTE1,0                                                        
         NI    CSINDSL2,X'FF'-CSIOVKEP                                          
         CLI   SESROUT,0           TEST RETURN POINT GIVEN                      
         BE    XITSES12                                                         
         GOTO1 ARECACT,CSREC                                                    
         OI    TWAINDS1,TWAIXITS   SET XITSES ISSUED                            
         MVC   BCBYTE1,SESROUT     RETURN ROUTINE NUMBER                        
         L     RD,BCSVRD                                                        
         L     RD,8(RD)                                                         
         B     XITSESX                                                          
*                                                                               
XITSES12 MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
XITSESX  B     ROUTE                                                            
         DROP  R2                                                               
*                                                                               
XSWORKD  DSECT                     ** XITSES S/R LOCAL W/S **                   
XSNXRECN DS    XL2                 SAVED CSHIRECN TO SET CSNXRECN               
XSWORKX  EQU   *                                                                
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SET NEW RECORD/ACTION WORDS                                         *         
*                                                                     *         
* NTRY - R1=AL1(RECORD NUMBER,ACTION NUMBER)                          *         
***********************************************************************         
*                                                                               
RECACT   GOTO1 ATSTMIX             TEST VALID RECORD/ACTION COMBO               
         BNE   ROUTH                                                            
         GOTO1 ASETSEL             SET A(SELECT TABLE)                          
         MVC   CSREC(L'CSREC+L'CSACT),0(R1)                                     
         L     RE,AMIXNTRY                                                      
         MVC   CSQRTN,MIXQRTN-MIXTABD(RE)                                       
         MVC   CSOVER,MIXOVER-MIXTABD(RE)                                       
*                                                                               
         L     RE,ARECTAB                                                       
         USING RECTABD,RE                                                       
RECACT2  CLI   RECTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RECNUMB,CSREC                                                    
         BE    *+12                                                             
         LA    RE,RECTABL(RE)                                                   
         B     RECACT2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,RECNAMEU                                                    
         LA    RF,TWAD(RF)                                                      
         MVC   BASREC,0(RF)        OUTPUT RECORD WORD AND TRANSMIT              
         OI    BASRECH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    BASRECH+(FVIIND-FVIHDR),FVIVAL                                   
*                                                                               
         L     RE,AACTTAB                                                       
         USING ACTTABD,RE                                                       
RECACT4  CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTNUMB,CSACT                                                    
         BE    *+12                                                             
         LA    RE,ACTTABL(RE)                                                   
         B     RECACT4                                                          
         SR    RF,RF                                                            
         ICM   RF,3,ACTNAMEU                                                    
         LA    RF,TWAD(RF)                                                      
         MVC   BASACT,0(RF)        OUTPUT ACTION WORD AND TRANSMIT              
         OI    BASACTH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    BASACTH+(FVIIND-FVIHDR),FVIVAL                                   
*                                                                               
RECACTX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR                                      *         
*                                                                     *         
* NTRY - R1=TSAR ACTION VALUE (TSAR WILL BE INITIALISED IF NECESSARY) *         
*     OR R1=A(TSAR ACTION VALUE,A(RECORD))                            *         
***********************************************************************         
*                                                                               
         USING TIWORKD,RC                                                       
TSARIO   ST    R1,TIPARM           SAVE ACTION OR A(PARAMETER)                  
         MVC   TITSACTN,TIPARM+3   SAVE CALLER'S ACTION                         
         USING LSTTABD,R2          R2=A(CURRENT LIST ENTRY)                     
         LA    R2,CSLSTCUR                                                      
         OC    TIPARM(3),TIPARM    TEST ACTION,A(RECORD) PASSSED                
         BZ    *+14                                                             
         MVC   TITSACTN,0(R1)                                                   
         ICM   R2,7,1(R1)                                                       
*                                                                               
         L     R3,ATSABLK                                                       
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
         TM    TSERRS,TSEEOF       EOF, FILLED UP                               
         BZ    TSARIO01            NO, CONTINUE                                 
         L     RD,BCSVRD           THIS IS FOR 2ND TIME.  SETMSG CALLS          
         LM    RE,RC,12(RD)        TSARIO AFTER TSAR EOF'S                      
         BR    RE                  EXIT TO ROOT CALL POINT                      
*                                                                               
TSARIO01 TM    BCTSINDS,BCTSIRES   TEST ALREADY RESTORED                        
         BNZ   TSARIO04                                                         
         LA    R0,LSTTABD                                                       
         ST    R0,TSAREC           SET A(RECORD)                                
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,L'LSTTRECN   SET KEY LENGTH                               
         MVC   TSRECL,=Y(LSTTABL)  SET RECORD LENGTH                            
         MVI   TSPAGN,TSPEXPN      SET NUMBER OF TEMPEST PAGES                  
*                                                                               
         MVI   TSPAGN,28                                                        
*                                                                               
         MVI   TSACTN,TSAINI       SET INITIALISE                               
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         CLI   TITSACTN,TSASAV     TEST SAVE                                    
         BE    TSARIOX                                                          
         TM    BCTSINDS,BCTSIINI   TEST TEMPEST BUFFER INITIALISED              
         BZ    TSARIO02                                                         
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVC   TSPAGL,BCTSLOWP     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,BCTSNUMP     SET NUMBER OF PAGES ALLOCATED                
*                                                                               
TSARIO02 GOTO1 VTSAR,TSARD         CALL TO INITIALISE/RESTORE                   
         BNE   TSARIOAB            ABEND                                        
         MVC   BCTSLOWP,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   BCTSNUMP,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         OI    BCTSINDS,BCTSIINI+BCTSIRES                                       
*                                                                               
TSARIO04 MVC   TSACTN,TITSACTN     SET ACTION NUMBER                            
         CLI   TSACTN,TSAINI       TEST EXPLICIT INITIALISE                     
         BE    TSARIOX                                                          
         CLI   TSACTN,TSARES       TEST EXPLICIT RESTORE                        
         BE    TSARIOX                                                          
         CLI   TSACTN,TSASAV       TEST SAVE                                    
         BNE   *+12                                                             
         NI    BCTSINDS,FF-BCTSIRES                                             
         B     TSARIO08                                                         
         MVC   TSRNUM,LSTTRECN     SET RECORD NUMBER (FOR TSAGET)               
         CLI   TSACTN,TSAADD       TEST ADD/PUT/WRITE                           
         BE    TSARIO06                                                         
         CLI   TSACTN,TSAPUT                                                    
         BE    TSARIO06                                                         
         CLI   TSACTN,TSAWRT                                                    
         BNE   TSARIO08                                                         
*                                                                               
TSARIO06 GOTO1 ASETMSK,LSTTABD     SET VALID ACTION MASK FOR RECORD             
         CLC   CSHIRECN,TSRNUM     SET HIGH RECORD NUMBER IF REQUIRED           
         BNL   *+10                                                             
         MVC   CSHIRECN,TSRNUM                                                  
         MVI   TSACTN,TSAPUT       SET TO PUT RECORD                            
         CLC   TSRNUM,BCTSHIGH     TEST NEW RECORD                              
         BNH   TSARIO08                                                         
         MVC   BCTSHIGH,TSRNUM     SET HIGH RECORD NUMBER - GLOBAL              
         MVI   TSACTN,TSAADD       SET TO ADD RECORD                            
*                                                                               
TSARIO08 GOTO1 VTSAR,TSARD                                                      
         BE    TSARIO10            NO ERRORS                                    
         TM    TSERRS,TSEEOF       EOF, FILLED UP                               
         BO    *+6                 PUT OUT ERROR MESSAGE                        
         DC    H'0'                                                             
         LA    R1,BASACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$TMIIL)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
*                                                                               
TSARIO10 CLC   BCTSHIGH,TSPRECN    CHECK HIGH NUMBERS AGREE                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSARIOX  B     ROUTE                                                            
*                                                                               
TSARIOAB LA    R1,BASACTH          ABEND IF INITIALISE/RESTORE FAILS            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$ISUTS)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
TIWORKD  DSECT                     ** TSARIO LOCAL W/S **                       
TIPARM   DS    A                   CALLING PARAMETER                            
TITSACTN DS    XL1                 SAVED TSAR ACTION NUMBER                     
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SET VALID ACTION MASK FOR A RECORD                                  *         
* NTRY - R1=A(CURRENT RECORD ENTRY)                                   *         
***********************************************************************         
*                                                                               
         USING SMWORKD,RC                                                       
SETMSK   LR    R2,R1                                                            
         USING LSTTABD,R2                                                       
         MVC   LSTTMASK,BCEFFS     PRESET ALL ACTIONS VALID                     
         SR    R1,R1                                                            
         ICM   R1,1,LSTTRTYP                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     SETMSK02            BATCH RECORD                                 
         B     SETMSK40            ITEM RECORD                                  
         B     SETMSK60            POSTING RECORD                               
                                                                                
SETMSK02 SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BCHALF,LSTBDELI                                                  
         SH    R0,BCHALF           TEST ANY ITEMS ADDED                         
         BNZ   *+10                                                             
         NC    LSTTMASK,=AL2(LMBATM02)                                          
                                                                                
         CLM   R0,3,CSBMAXIU       TEST TOO MANY ITEMS FOR UPDATE               
         BNH   *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATUPD))                                   
                                                                                
         TM    LSTTSTAT,TBAHSUPD   TEST BATCH UPDATED                           
         BO    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEREV))                                   
                                                                                
         TM    LSTTSTAT,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(LMBATDSP+LMITELST)                                 
                                                                                
         TM    CSBIND2,TYPINOU     TEST UPDATE ALLOWED FOR BATCH TYPE           
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATUPD))                                   
                                                                                
         GOTO1 ATSTACS,=AL1(RECITE,ACTINP)                                      
         BE    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATRCL))                                   
                                                                                
         TM    CSBIND4,TYPIBACO    TEST BATCH/COPY NOT ALLOWED                  
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITECOP))                                   
                                                                                
         TM    CSBIND4,TYPIBARV    TEST BATCH/REVERSE NOT ALLOWED               
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEREV))                                   
                                                                                
         TM    CSBIND4,TYPIBGIC    TEST BAT/GEN & ITE/CHA NOT ALLOWED           
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEGEN))                                   
                                                                                
         TM    LSTBIND2,LSTBDISP   TEST BATCH DISPLAY ONLY                      
         BO    *+12                                                             
         TM    CSBIND8,TYPIDISP    TEST TYPE DISPLAY ONLY                       
         BZ    SETMSK04                                                         
         NC    LSTTMASK,=AL2(LMBATDSP+LMITELST+LMBATPRT)                        
         B     SETMSK05                                                         
                                                                                
SETMSK04 TM    LSTBINDS,LSTBBCBG   TEST BATCH CREATED BY ACBG                   
         BO    *+12                                                             
         TM    CSBIND2,TYPICBG     TEST TYPE CREATED BY ACBG                    
         BZ    SETMSK05                                                         
         NC    LSTTMASK,=AL2(LMBATDSP+LMITELST+LMBATPRT+LMBATGLU)               
                                                                                
SETMSK05 TM    CSBIND5,TYPIASIR    TEST AUTO SELECT ALL FOR REVERSE             
         BZ    SETMSK06                                                         
         TM    LSTBHDS1,BHDSNREV   TEST REVERSAL NOT ALLOWED                    
         BNZ   SETMSK06                                                         
         OC    LSTTMASK,=AL2(LMITEREV+LMBATCLO+LMBATUPD)                        
         TM    LSTBHDS1,BHDSREVS   IF REVERSAL BATCH ALLOW DELETE               
         BZ    *+10                                                             
         OC    LSTTMASK,=AL2(LMBATDEL+LMBATAPR+LMBATUAP+LMBATSAV)               
                                                                                
SETMSK06 TM    LSTBHDS1,BHDSACRV   TEST ACCRUAL REVERSAL                        
         BO    *+12                                                             
         TM    CSBIND1,TYPIACRV    TEST AUTOMATIC ACCRUAL REVERSAL              
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(LMBATDSP+LMITELST+LMBATPRT)                        
                                                                                
         TM    LSTBIND2,LSTBISIN   TEST SPLIT INVOICE ITEM                      
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEGEN))  GENERATE NOT ALLOWED             
                                                                                
         TM    LSTTSTAT,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(LMBATDSP+LMITELST+LMITECRG+LMBATPRT)               
*&&US                                                                           
         TM    LSTTSTAT,TBAHSUPD   TEST BATCH UPDATED                           
         BNZ   *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEREV+LMBATGLU))                          
         TM    LSTBHDS2,BHDSGENL   TEST UPDATED TO G/L                          
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATGLU))                                   
*                                                                               
         OC    BCCPYGLM,BCCPYGLM                                                
         BZ    SETMSK07                                                         
*        CLC   BCCPYGLM,LSTBMOSP                                                
*        BH    SETMSK07                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMBATGLU))                                   
SETMSK07 DS    0H                                                               
*&&                                                                             
         CLC   LSTBEFDT,LSTBADDT   TEST REGULAR BATCH                           
         BNE   SETMSK08                                                         
         TM    BCCPYST5,CPYSBAPR   TEST APPROVING REGULAR BATCHES               
         BNZ   *+14                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATAPR+LMBATUAP))                          
         B     SETMSK10                                                         
         TM    LSTTSTAT,TBAHSAPR   TEST BATCH IS ALREADY APPROVED               
         BZ    *+14                                                             
         NC    LSTTMASK,=AL2(LMBATM03+LMBATPRT)                                 
         B     SETMSK10                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMBATUAP))                                   
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,=AL1(ACTAPR)                                             
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BE    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATUPD))                                   
         B     SETMSK10                                                         
                                                                                
SETMSK08 TM    BCCPYST5,CPYSBAPE   TEST APPROVING EFFECTIVE BATCHES             
         BNZ   *+14                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATAPR+LMBATUAP))                          
         B     SETMSK10                                                         
         TM    LSTTSTAT,TBAHSAPR   TEST BATCH IS ALREADY APPROVED               
         BZ    *+14                                                             
         NC    LSTTMASK,=AL2(LMBATM03+LMBATPRT)                                 
         B     SETMSK10                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMBATUAP))                                   
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,=AL1(ACTAPR)                                             
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BE    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATUPD))                                   
                                                                                
SETMSK10 TM    LSTTSTAT,TBAHSEND                                                
         BNZ   *+14                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATAPR+LMBATUAP))                          
         B     *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATCLO+LMBATSAV+LMBATCHA))                 
                                                                                
         TM    LSTTSTAT,TBAHSSAV                                                
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATSAV))                                   
                                                                                
         TM    LSTTSTAT,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    SETMSK12                                                         
         NC    LSTTMASK,=AL2(LMBATM04)                                          
         CLC   LSTBADDT,BCTODAYC   RECALL/SAVE ONLY IF CREATED TODAY            
         BE    SETMSK12                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMBATRCL+LMBATSAV))                          
                                                                                
SETMSK12 TM    LSTTSTAT,TBAHSIIP                                                
         BNZ   *+14                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATSAV))                                   
         B     SETMSK14                                                         
         MVC   SMMASK,=AL2(LMBATDSP+LMITELST+LMBATPRT)                          
         ICM   R0,3,LSTBIBNO                                                    
         BNZ   *+8                                                              
         ICM   R0,3,LSTBBCHR                                                    
         CLM   R0,3,CUPASS         TEST BATCH OWNED BY CONNECTED USER           
         BNE   *+10                                                             
         OC    SMMASK,=AL2(LMBATM01)                                            
         NC    LSTTMASK,SMMASK                                                  
                                                                                
SETMSK14 TM    LSTBINDS,LSTBINPT   TEST INPUT SESSION IN PROGRESS               
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMBATRCL))                                   
         TM    CSBIND1,TYPICUMU    TEST TYPE ACCUMULATES TOTAL DR/CR            
         BZ    SETMSK16                                                         
         OC    LSTBTDRS,LSTBTDRS   TEST TOTAL DEBITS (& CREDITS) SET            
         BZ    SETMSK16                                                         
         CP    LSTBTDRS,LSTBTCRS   TEST BATCH IN BALANCE                        
         BE    SETMSK16                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMBATAPR+LMBATUPD+LMBATCLO))                 
                                                                                
SETMSK16 LA    R3,SETTAB           R3=A(ACTION/MASK TABLE)                      
         LA    R0,SETTABN          R0=NUMBER OF ENTRIES IN TABLE                
                                                                                
SETMSK18 MVC   BCWORK(L'LSTTMASK),LSTTMASK                                      
         NC    BCWORK(L'LSTTMASK),1(R3)                                         
         BZ    SETMSK20                                                         
         SR    RF,RF               TEST ACTION VALID FOR BATCH TYPE             
         IC    RF,LSTBBTYP                                                      
         LA    RF,64(RF)           ADD ON BASE NUMBER FOR RECORD TYPE           
         GOTO1 VSECRET,BCPARM,('SECPRACT',ASECBLK),((RF),(R3))                  
         BE    SETMSK20                                                         
         XC    BCWORK(L'LSTTMASK),BCEFFS                                        
         NC    LSTTMASK,BCWORK     TURN OFF LIST MASK ACTION BIT                
                                                                                
SETMSK20 AHI   R3,L'SETTAB         BUMP TO NEXT ACTION/MASK ENTRY               
         BCT   R0,SETMSK18         DO FOR NUMBER OF ENTRIES                     
         B     SETMSKX                                                          
                                                                                
SETMSK40 NC    LSTTMASK,=AL2(FFFF-(LMITEINP))                                   
         TM    CSBIND4,TYPIBGIC    TEST BAT/GEN & ITE/CHA NOT ALLOWED           
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITECHA))                                   
         TM    LSTIIND1,LSTI1SIN   TEST ITEM IS A SPLIT INVOICE                 
         BO    *+12                                                             
         CLI   LSTINASK,0          TEST ANY POSTINGS ON RECORD                  
         BE    *+12                                                             
         TM    LSTTSTAT,TBAESDEL+TBAESLDE                                       
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMPSTLST))                                   
         TM    LSTTSTAT,TBAESDEL+TBAESLDE                                       
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEM01))                                   
         TM    LSTTSTAT,TBAESORD                                                
         BZ    SETMSK42                                                         
         CLI   CSBTYP,60           TYPE 60 ACCEPTED                             
         BE    SETMSK42                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMITEM02))                                   
                                                                                
SETMSK42 TM    LSTIIND1,LSTI1SEL                                                
         BZ    *+14                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMISESEL))                                   
         B     *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMISEDES+LMISECHA))                          
         TM    CSBIND2,TYPICBG     TEST TYPE CREATED BY ACBG                    
         BNZ   SETMSK44                                                         
         TM    LSTIIND1,LSTI1AUT   TEST 'AUTO GENERATED' ITEM                   
         BNZ   SETMSK44                                                         
         TM    BCBATCUR+(LSTBINDS-LSTTABD),LSTBBCBG                             
         BZ    SETMSK46                                                         
                                                                                
SETMSK44 NC    LSTTMASK,=AL2(FFFF-(LMITEDEL+LMITECHA+LMITEDSP))                 
         B     *+10                                                             
                                                                                
SETMSK46 NC    LSTTMASK,=AL2(FFFF-(LMPSTDSP+LMPSTANL))                          
         TM    BCBATCUR+(LSTBINDS-LSTTABD),LSTBACRV                             
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEDEL+LMITECHA))                          
         TM    BCBATCUR+(LSTTSTAT-LSTTABD),TBAHSM01                             
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITEDEL+LMITECHA))                          
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT+LSTBIMSI                    
         BNZ   SETMSK48                                                         
         TM    CSBIND3,TYPIMITE+TYPIMSCR                                        
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITECHA))                                   
                                                                                
SETMSK48 TM    BCBATCUR+(LSTTSTAT-LSTTABD),TBAHSIIP                             
         BZ    SETMSK50                                                         
         ICM   R0,3,BCBATCUR+(LSTBIBNO-LSTTABD)                                 
         BNZ   *+8                                                              
         ICM   R0,3,BCBATCUR+(LSTBBCHR-LSTTABD)                                 
         CLM   R0,3,CUPASS         TEST BATCH OWNED BY CONNECTED USER           
         BE    SETMSK50                                                         
         NC    LSTTMASK,=AL2(FFFF-(LMITECHA+LMITEDEL))                          
                                                                                
SETMSK50 TM    BCBATCUR+(LSTBHDS1-LSTTABD),BHDSREVS                             
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMITECHA))                                   
         B     SETMSKX                                                          
                                                                                
SETMSK60 TM    LSTTSTAT,TRNSDELT   TEST TRANSACTION DELETED/NOT FOUND           
         BZ    *+10                                                             
         NC    LSTTMASK,=AL2(FFFF-(LMPSTDSP+LMPSTANL))                          
         B     SETMSKX                                                          
*                                                                               
SETMSKX  B     ROUTE                                                            
         DROP  R2,RC                                                            
*                                                                               
SETTAB   DS    0XL3                ** TABLE OF ACTIONS AND MASKS **             
         DC    AL1(ACTOPN),AL2(LMBATOPN)                                        
         DC    AL1(ACTDSP),AL2(LMBATDSP)                                        
         DC    AL1(ACTUPD),AL2(LMBATUPD)                                        
         DC    AL1(ACTCHA),AL2(LMBATCHA)                                        
         DC    AL1(ACTCLO),AL2(LMBATCLO)                                        
         DC    AL1(ACTREC),AL2(LMBATRCL)                                        
         DC    AL1(ACTSAV),AL2(LMBATSAV)                                        
         DC    AL1(ACTDEL),AL2(LMBATDEL)                                        
         DC    AL1(ACTAPR),AL2(LMBATAPR)                                        
         DC    AL1(ACTUAP),AL2(LMBATUAP)                                        
         DC    AL1(ACTCOP),AL2(LMITECOP)                                        
         DC    AL1(ACTREV),AL2(LMITEREV)                                        
         DC    AL1(ACTGEN),AL2(LMITEGEN)                                        
         DC    AL1(ACTGLU),AL2(LMBATGLU)                                        
SETTABN  EQU   (*-SETTAB)/L'SETTAB                                              
         DS    0H                                                               
*                                                                               
LMBATM01 EQU   LMBATRCL+LMBATSAV+LMBATCLO+LMBATUPD+LMBATDEL+LMBATCHA            
LMBATM02 EQU   LMBATDSP+LMBATSAV+LMBATRCL+LMBATDEL+LMBATCHA                     
LMBATM03 EQU   LMBATDSP+LMITELST+LMBATUAP+LMBATUPD+LMITECRG                     
LMBATM04 EQU   LMBATDSP+LMITELST+LMBATRCL+LMBATSAV+LMBATPRT                     
LMITEM01 EQU   LMITEDEL+LMITECHA+LMISESEL+LMISEDES+LMISECHA                     
LMITEM02 EQU   LMITECHA+LMISESEL+LMISEDES+LMISECHA                              
TBAHSM01 EQU   TBAHSDEL+TBAHSUPD+TBAHSEND+TBAHSIAD+TBAHSAPR                     
LMITECRG EQU   LMITECOP+LMITEREV+LMITEGEN+LMBATGLU                              
*                                                                               
SMWORKD  DSECT                     ** SETMSK S/R LOCAL W/S **                   
SMMASK   DS    XL(L'LSTTMASK)                                                   
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT RECORD ACTIVITY ELEMENTS INTO RECORD                            *         
*                                                                     *         
* NTRY - P1 BYTE 0 = RACTADD TO SET ADD ELEMENT ONLY                  *         
*                  = RACTCHA TO SET CHANGE ELEMENT ONLY               *         
*                  = RACTADD+RACTCHA TO SET BOTH ELEMENTS             *         
*              1-3 = A(RECORD)                                                  
***********************************************************************         
*                                                                               
         USING PRWORKD,RC                                                       
PUTRAC   MVC   PRTYPE,0(R1)                                                     
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         USING ACTRECD,R2          R2=A(RECORD)                                 
*                                                                               
         LA    R3,PRRACEL          SET UP ACTIVITY ELEMENT                      
         USING RACELD,R3                                                        
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CUPASS                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,BCTODAYP                                                 
         MVC   RACTIME,ASTIME                                                   
*                                                                               
         GOTO1 PRAC,RACTADD                                                     
         GOTO1 PRAC,RACTCHA                                                     
*                                                                               
PUTRACX  B     ROUTE                                                            
*                                                                               
PRAC     STC   R1,RACTYPE          TEST ELEMENT REQUIRED                        
         NC    RACTYPE,PRTYPE                                                   
         BZR   RE                                                               
*                                                                               
         LR    R0,RE               SEE IF ELEMENT ALREADY IN RECORD             
         LA    R1,ACTRFST                                                       
         XR    RF,RF                                                            
PRAC02   CLI   0(R1),0                                                          
         BE    PRAC10                                                           
         CLI   0(R1),RACELQ                                                     
         BNE   PRAC08                                                           
         CLC   RACTYPE,RACTYPE-RACELD(R1)                                       
         BNE   PRAC08                                                           
         MVC   0(RACLNQ,R1),RACELD COPY NEW ACTIVITY ELEMENT                    
         B     PRACX                                                            
PRAC08   IC    RF,1(R1)                                                         
         BXH   R1,RF,PRAC02                                                     
*                                                                               
PRAC10   GOTO1 VHELLO,BCPARM,(C'P',ACCMST),ACTRECD,RACELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRACX    LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R2,R3,RC                                                         
*                                                                               
PRWORKD  DSECT                     ** PUTRAC S/R LOCAL W/S **                   
PRTYPE   DS    XL1                                                              
PRRACEL  DS    XL(RACLNQ)          ACTIVITY ELEMENT                             
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET RECORD ACTIVITY ELEMENTS FROM RECORD                            *         
*                                                                     *         
* NTRY - P1 = A(RECORD)                                               *         
*        P2 = A(AREA FOR ADD ELEMENT) OR 0                            *         
*        P3 = A(AREA FOR CHANGE ELEMENT) OR 0                         *         
***********************************************************************         
*                                                                               
GETRAC   LM    R2,R4,0(R1)                                                      
         USING ACTRECD,R2          R2=A(RECORD)                                 
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         XC    0(RACLNQ,R3),0(R3)  CLEAR AREA FOR ADD ELEMENT                   
         LTR   R4,R4                                                            
         BZ    *+10                                                             
         XC    0(RACLNQ,R4),0(R4)  CLEAR AREA FOR CHANGE ELEMENT                
*                                                                               
         LA    R1,ACTRFST                                                       
         USING RACELD,R1           R1=A(ACTIVITY ELEMENT)                       
         XR    RF,RF                                                            
GRAC02   CLI   RACEL,0             TEST EOR                                     
         BE    GETRACX                                                          
         CLI   RACEL,RACELQ        MATCH ON ACTIVITY ELEMENT                    
         BNE   GRAC08                                                           
*                                                                               
         CLI   RACTYPE,RACTADD     TEST ADD ELEMENT                             
         BNE   GRAC04                                                           
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   0(RACLNQ,R3),RACELD                                              
         B     GRAC08                                                           
*                                                                               
GRAC04   CLI   RACTYPE,RACTCHA     TEST CHANGE ELEMENT                          
         BNE   GRAC08                                                           
         LTR   R4,R4                                                            
         BZ    *+10                                                             
         MVC   0(RACLNQ,R4),RACELD                                              
         B     GETRACX                                                          
*                                                                               
GRAC08   IC    RF,RACLN            BUMP R1 TO NEXT ELEMENT                      
         BXH   R1,RF,GRAC02                                                     
*                                                                               
GETRACX  B     ROUTE                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* TEST TAX TYPE FOR SE POSTINGS                                       *         
*                                                                     *         
* NTRY - P1/B0   = INDICATOR AS FOLLOWS:-                             *         
*                  X'80'=FIELD IS REQUIRED IF ON ELSE OPTIONAL        *         
*                  X'40'=DEFAULT ACCOUNT IS SE LEDGER (CAN BE OVERR-  *         
*                        IDDEN WITH *UL) IF ON ELSE CHECK FOR SE OR   *         
*                        *SE AT THE BEGINNING OF THE FIELD            *         
*                                                                     *         
*        P1/B1-3 = A(INPUT FIELD HEADER) OR 0 IF FVAL CALLED          *         
* EXIT - BCBYTE1 CONTAINS X'00' IF TAX N/A OR TAX TYPE (C'1'-C'3')    *         
***********************************************************************         
*                                                                               
TSTTAX   DS    0H                                                               
*&&US*&& DC    H'0'                SHOULD NOT BE CALLED IN USA                  
*&&UK                                                                           
         MVI   BCBYTE1,0           SET RETURN VALUE (NO TAX)                    
         LR    R2,R1                                                            
         XR    R1,R1                                                            
         ICM   R1,7,1(R2)                                                       
         BZ    TSTTAX01            R1=A(INPUT FIELD HEADER)                     
         TM    0(R2),X'80'         TEST IF FIELD IS REQUIRED                    
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
TSTTAX01 TM    BCCPYST8,CPYSTTAX                                                
         BZ    ROUTE                                                            
         TM    0(R2),X'40'         TEST DEFAULT IS AN SE ACCOUNT                
         BZ    TSTTAX02                                                         
         CLI   FVIFLD,C'*'         IF OVERRIDE THEN DON'T LOOK                  
         BE    ROUTE                                                            
         B     TSTTAX04                                                         
*                                                                               
TSTTAX02 LA    RE,FVIFLD                                                        
         CLI   FVIFLD,C'*'                                                      
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLC   =C'SE',0(RE)        TEST IF AN SE ACCOUNT                        
         BNE   ROUTE                                                            
*                                                                               
TSTTAX04 MVI   BCBYTE1,C'1'        SET DEFAULT TAX TYPE                         
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,FVIFLD-2(RE)                                                  
         CLC   0(1,RE),BCCOMMA     LOOK FOR FIELD DELIMITER                     
         BNE   ROUTE                                                            
         MVC   BCWORK(1),1(RE)                                                  
         XC    0(2,RE),0(RE)       CLEAR END OF INPUT FIELD                     
         SR    RE,RE                                                            
         IC    RE,FVILEN           AND DECREMENT INPUT LENGTHS                  
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         STCM  RE,1,FVILEN                                                      
         IC    RE,FVXLEN                                                        
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         STC   RE,FVXLEN                                                        
         CLI   BCWORK,C'1'         TEST FOR VALID TAX TYPE                      
         BL    TSTTAXN                                                          
         CLI   BCWORK,C'3'                                                      
         BH    TSTTAXN                                                          
         MVC   BCBYTE1,BCWORK                                                   
         B     ROUTE                                                            
*                                                                               
TSTTAXN  MVC   FVMSGNO,=AL2(AE$INVTX)                                           
         B     ROUTH                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK BATCH MONTH FORMAT ENTERED AGAINST BATCH PROFILE              *         
*                                                                     *         
* NTRY - BCWORK CONTAINS PERVAL BLOCK                                 *         
***********************************************************************         
*                                                                               
CHKBMO   LA    R1,BCWORK                                                        
         USING PERVALD,R1                                                       
         CLI   BCBP27,C'Y'         TEST YEAR IS REQUIRED                        
         BNE   CHKBMO02                                                         
         TM    PVALASSM,PVALASY    TEST YEAR ASSUMED                            
         BZ    CHKBMO02                                                         
         MVC   FVMSGNO,=AL2(AE$SYIPE)                                           
         B     ROUTH                                                            
*                                                                               
CHKBMO02 DS    0H                  FURTHER TESTS GO HERE                        
         B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT OUT EXCHANGE RATE(S)                                *         
*                                                                     *         
* NTRY: P1 BYTE 0 = L'OUTPUT                                          *         
*             1-3 = A(OUTPUT)                                         *         
*       P2        = A(1ST EXCHANGE RATE - PWOS, PL5)                  *         
*       P3        = A(2ND EXCHANGE RATE) OR 0                         *         
***********************************************************************         
*                                                                               
         USING ERWORKD,RC                                                       
EDTRAT   MVC   ERPARMS,0(R1)                                                    
         MVC   ERWORK,BCSPACES                                                  
         LA    R2,ERWORK           EXIT OUT 1ST EXCHANGE RATE                   
         L     R3,ERPARAT1                                                      
         BAS   RE,EDTOUT                                                        
         ICM   R3,15,ERPARAT2      TEST FOR 2ND EXCHANGE RATE                   
         BZ    EDTRATX                                                          
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,EDTOUT                                                        
*                                                                               
EDTRATX  IC    RF,ERPLOUT          COPY TO OUTPUT AREA                          
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
         ICM   RE,7,ERPAOUT                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ERWORK                                                   
         B     ROUTX                                                            
*                                                                               
EDTOUT   ST    RE,ERSAVERE         EDIT OUT SINGLE EXCHANGE RATE                
         ZAP   ERPL8,BCPZERO                                                    
         MVO   ERPL8,0(5,R3)                                                    
         CURED (P8,ERPL8),(EDTOUTLQ,(R2)),5,ALIGN=LEFT,DMCB=ERDMCB              
         LA    R2,EDTOUTLQ-1(R2)                                                
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
EDTOUT02 CLI   0(R2),C'0'                                                       
         BH    EDTOUTX                                                          
         BL    *+12                                                             
         MVI   0(R2),C' '                                                       
         BCT   R2,EDTOUT02                                                      
         MVI   1(R2),C'0'                                                       
         LA    R2,1(R2)                                                         
EDTOUTX  LA    R2,1(R2)                                                         
         L     RE,ERSAVERE                                                      
         BR    RE                                                               
*                                                                               
         DROP  RC                                                               
*                                                                               
ERWORKD  DSECT                     ** EDTRAT S/R LOCAL W/S **                   
ERPARMS  DS    0XL12               INPUT PARAMETERS                             
ERPLOUT  DS    XL1                 L'OUPUT                                      
ERPAOUT  DS    AL3                 A(OUTPUT)                                    
ERPARAT1 DS    A                   A(EXCHANGE RATE 1)                           
ERPARAT2 DS    A                   A(EXCHANGE RATE 2)                           
         ORG   ERPARMS+L'ERPARMS                                                
ERPARM   DS    6A                                                               
ERDMCB   DS    6A                                                               
ERSAVERE DS    A                                                                
ERPL8    DS    PL8                                                              
ERWORK   DS    CL80                                                             
EDTOUTLQ EQU   16                  LENGTH OF EDIT RATE OUTPUT AREA              
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A SET OF TRANSACTIONS FOR A GROUP INVOICE.  ON    *         
* ENTRY IOKEY CONTAINS KEY OF GROUP INVOICE PASSIVE.  IF GINPISN IS   *         
* SET TO X'FFFF' THEN ALL TRANSACTIONS AND PASSIVES FOR THE GROUP ARE *         
* DELETED, OTHERWISE ONLY THOSE FOR THE SPECIFIC SEQUENCE NUMBER ARE. *         
* ONLY OLD CREDITOR/VAT TXS/PASSIVES ARE DELETED IF GINPISN ISN'T SET.*         
***********************************************************************         
*                                                                               
         USING DGWORKD,RC                                                       
DELGIN   LA    R2,IOKEY                                                         
         USING GINPASD,R2                                                       
         MVC   DGKEY,GINPKEY       SAVE PASSIVE KEY                             
         OC    GINPISN,GINPISN                                                  
         BNZ   *+12                                                             
         OI    DGFLAG,DGFCVO       OLD CREDITOR/VAT ONLY                        
         B     DELGIN01                                                         
         CLI   GINPISN,FF                                                       
         BNE   *+14                                                             
         OI    DGFLAG,DGFALL       ALL IN GROUP                                 
         XC    GINPISN,GINPISN                                                  
DELGIN01 XC    GINPPTYP(GINPEND-(GINPPTYP-GINPASD)),GINPPTYP                    
         GOTO1 AIO,IOHIUP+IOACCDIR                                              
*                                                                               
DELGIN02 BE    *+6                                                              
         DC    H'0'                                                             
DELGIN03 CLC   GINPASD(GINPISN-GINPKEY),DGKEY                                   
         BNE   DELGINX                                                          
         TM    DGFLAG,DGFALL       TEST ALL ENTRIES TO BE DELETED               
         BO    DELGIN04                                                         
         TM    DGFLAG,DGFCVO       TEST DELETING OLD CREDITOR/VAT ONLY          
         BZ    *+18                                                             
         OC    GINPISN,GINPISN                                                  
         BNZ   DELGINX                                                          
         B     DELGIN04                                                         
         CLC   GINPISN,DGKEY+(GINPISN-GINPKEY)                                  
         BNE   DELGINX                                                          
DELGIN04 MVC   DGSKEY,IOKEY        SAVE THIS KEY                                
         TM    DGFLAG,DGFCVO       DON'T DELETE LAST CREDITOR/VAT               
         BZ    DELGIN06                                                         
         GOTO1 AIO,IOSQUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GINPASD(GINPULA-GINPKEY),DGSKEY                                  
         BNE   DELGIN03                                                         
         XC    IOADDR,IOADDR       RE-READ TO ESTABLISH SEQUENCE                
         MVC   IOKEY(L'DGSKEY),DGSKEY                                           
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
DELGIN06 OI    GINPKSTA,TRNSDELT   SET TO DELETE PASSIVE POINTER                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,DGIO                                                          
         ST    R0,IOADDR                                                        
         GOTO1 AIO,IOGETRUP+IOACCMST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DGIO+(TRNRSTA-TRNRECD),TRNSDELT                                  
         MVC   BCINVREF,BCSPACES                                                
         MVC   BCINVDTE,DGIO+TRNKDATE-TRNRECD                                   
         MVC   BCINVREF(L'TRNKREF),DGIO+TRNKREF-TRNRECD                         
         GOTO1 AIO,IOPUT+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'TRNKEY),DGIO                                             
         XC    IOADDR,IOADDR                                                    
         USING TRNRECD,R2                                                       
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TRNKSTA,TRNSDELT                                                 
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CSBIND8,TYPIDCSA    TEST DR/CR SUBSIDIARILY ACCUMULATED          
         BZ    DELGIN07                                                         
         LA    RE,CSLSTCUR                                                      
         USING LSTTABD,RE                                                       
         LA    R2,DGIO                                                          
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+14                                                             
         SP    LSTBTDRS,TRNAMNT                                                 
         B     *+10                                                             
         SP    LSTBTCRS,TRNAMNT                                                 
         DROP  RE,RF                                                            
         LA    R2,IOKEY                                                         
DELGIN07 MVC   TRNKWORK(TRNKEND-(TRNKWORK-TRNRECD)),BCSPACES                    
         GOTO1 AIO,IORD+IOACCDIR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,DGIO                                                          
         ST    R0,IOADDR                                                        
         MVC   IODAOVER,IODA                                                    
         GOTO1 AIO,IOGETRUP+IOACCMST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,DGIO+(ACTRFST-ACTRECD)                                        
         SR    R0,R0                                                            
         USING ASTELD,R1                                                        
DELGIN08 IC    R0,ASTLN                                                         
         AR    R1,R0                                                            
         CLI   ASTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ASTEL,ASTELQ                                                     
         BNE   DELGIN08                                                         
         SR    R0,R0                                                            
         ICM   R0,7,ASTDRAFT                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R0,0                                                             
         STCM  R0,7,ASTDRAFT                                                    
         GOTO1 AIO,IOPUT+IOACCMST                                               
DELGIN10 XC    IOADDR,IOADDR                                                    
         MVC   IOKEY(L'DGSKEY),DGSKEY                                           
         GOTO1 AIO,IOHIUP+IOACCDIR                                              
         B     DELGIN02                                                         
*                                                                               
DELGINX  B     ROUTE                                                            
*                                                                               
DGWORKD  DSECT                     ** DELGIN S/R LOCAL W/S **                   
DGFLAG   DS    X                                                                
DGFALL   EQU   X'80'               DELETE ALL RECORDS FOR GROUP                 
DGFCVO   EQU   X'40'               DELETE CREDITOR/VAT RECORDS ONLY             
DGKEY    DS    XL(L'GINPKEY)       SAVED INPUT KEY                              
DGSKEY   DS    XL(L'GINPKEY)       SAVED LAST KEY READ                          
DGIO     DS    XL2048                                                           
BAT6D    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE NEXT AVAILABLE GROUP INVOICE NUMBER              *         
* EXIT - CSGIN SET IF CC EQUAL, ERROR MESSAGE SET IF CC NOT EQUAL     *         
***********************************************************************         
*                                                                               
GETGIN   XC    CSGIN,CSGIN                                                      
         LA    R3,IOKEY                READ LEDGER RECORD                       
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYSUP),BCCPYSUP                                     
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BNE   GGIN12                                                           
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO3                                    
         BNE   GGIN12                                                           
         L     R3,AIO3                                                          
         LA    R2,ACTRFST                                                       
*                                                                               
         XR    R0,R0                                                            
GGIN02   CLI   0(R2),0                                                          
         BE    GGIN08                                                           
         CLI   0(R2),FFTELQ                                                     
         BE    GGIN06                                                           
GGIN04   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GGIN02                                                           
*                                                                               
         USING FFTELD,R2                                                        
GGIN06   CLI   FFTTYPE,FFTTHGIN                                                 
         BNE   GGIN04                                                           
         ICM   R1,15,FFTDATA       HIGHEST ALLOCATION GROUP NO.                 
         C     R1,=X'7FFFFFFF'                                                  
         BNL   GGIN12                                                           
         LA    R1,1(R1)            INCREMENT                                    
         STCM  R1,15,FFTDATA                                                    
         STCM  R1,15,CSGIN                                                      
         B     GGIN10                                                           
         DROP  R2,R3                                                            
*                                                                               
GGIN08   XC    BOELEM,BOELEM       ADD FREE FORM TEXT ELEMENT                   
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTHGIN                                                 
         LH    R0,=H'1'                                                         
         STCM  R0,15,FFTDATA                                                    
         STCM  R0,15,CSGIN                                                      
         LA    RF,L'CSGIN                                                       
         STC   RF,FFTDLEN                                                       
         LA    RF,FFTLN1Q+1(RF)                                                 
         STC   RF,FFTLN                                                         
         GOTO1 VHELLO,BCPARM,(C'P',=C'ACCMST'),AIO3,BOELEM,0                    
         CLI   12(R1),0                                                         
         BNE   GGIN12                                                           
         POP   USING                                                            
*                                                                               
GGIN10   GOTO1 AIO,IOPUT+IOACCMST+IO3  WRITE BACK UPDATED LEDGER RECORD         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ROUTE                                                            
*                                                                               
GGIN12   MVC   FVMSGNO,=AL2(AE$UEGIN)  UNABLE TO ESTABLISH NUMBER               
         B     ROUTX                                                            
         EJECT                                                                  
BAT6D    CSECT                                                                  
***********************************************************************         
* BUILD AUDIT ENTRY FOR DELETED/ADDED BATCH ITEM                      *         
***********************************************************************         
*                                                                               
         USING STCELD,R5                                                        
         USING UOWORKD,RC                                                       
ORDAUD   LA    R5,BOELEM                                                        
         L     R2,0(R1)            R2=A(BATCH RECORD)                           
         MVC   UOACTN,0(R1)        ACTION REQUIRED, 1=DELETE, 2=ADD             
                                                                                
         XC    UOWCCNT,UOWCCNT                                                  
         XC    UORDNUM,UORDNUM                                                  
                                                                                
         USING TBARECD,R2                                                       
         XC    STCEL(L'BOELEM),STCEL                                            
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIORD2                                                  
         MVI   STCOTYP,STCOMATQ                                                 
         MVI   STCLN,STCOLN5Q                                                   
         MVC   STCOUSR,CUUSER                                                   
         MVC   STCOPID,CUPASS                                                   
         MVC   STCODTE,BCTODAYP                                                 
         MVI   STCOAPPL,STCOPSTM                                                
         MVC   STCOBREF,TBAKBREF                                                
         MVC   STCOBMY,TBAKBMOS   CONVERT YYMM -> YM EBCDIC                     
         OI    STCOBMY,X'F0'                                                    
         LLC   R1,STCOBMY+1                                                     
         LHI   RF,X'F0'                                                         
         TM    STCOBMY+1,X'10'                                                  
         JNO   *+8                                                              
         LHI   RF,X'B1'                                                         
         AR    R1,RF                                                            
         STC   R1,STCOBMY+1                                                     
                                                                                
         MVC   STCOIREF,BCINVREF                                                
         MVC   STCOIDTE,BCINVDTE                                                
         XC    BODUB1,BODUB1                                                    
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,BODUB1+4                                                   
         ZAP   STCOTIM,BODUB1                                                   
*&&US*&& AP    STCOTIM,=P'60000'   ADJUST TO REAL (EST) TIME IN US              
                                                                                
ORDAUD08 LA    R3,TBARFST                                                       
         USING BIOELD,R3                                                        
         LA    R4,UOWCTAB                                                       
WC       USING UOWCTAB,R4                                                       
ORDAUD10 CLI   BIOEL,0                                                          
         JE    ORDAUD18                                                         
         CLI   BIOEL,BIOELQ                                                     
         JE    ORDAUD14                                                         
ORDAUD12 LLC   RF,BIOLN                                                         
         AR    R3,RF                                                            
         J     ORDAUD10                                                         
                                                                                
ORDAUD14 OC    UORDNUM,UORDNUM     HAVE WE READ ANY BIOELS                      
         JNZ   ORDAUD16            YES                                          
         MVC   UORDNUM,BIOONUM     NO - FIRST TIME EXTRACT ORDER NUMBER         
         CLI   UOACTN,UODELETE        AND STATUS OF MATCHING                    
         JNE   *+8                                                              
         MVI   STCOMIND,STCOIUMA                                                
         CLI   UOACTN,UOADD                                                     
         JNE   ORDAUD16                                                         
*&&UK                                                                           
         CLI   CSACT,ACTREV                                                     
         JE    ORDAUD15                                                         
*&&                                                                             
         MVI   STCOMIND,STCOIFMA                                                
         CLI   BIOSTAT,BIOSPTQ                                                  
         JNE   *+8                                                              
         MVI   STCOMIND,STCOIPMA                                                
         J     ORDAUD16                                                         
                                                                                
ORDAUD15 MVI   STCOMIND,STCOIFMR                                                
         CLI   BIOSTAT,BIOSPTQ                                                  
         JNE   *+8                                                              
         MVI   STCOMIND,STCOIPMR                                                
                                                                                
ORDAUD16 MVC   WC.UOWCMTCH,BIOWORK                                              
         ZAP   WC.UOWCAMT,BCPZERO                                               
         CLI   UOACTN,UODELETE                                                  
         JE    *+14                                                             
         AP    WC.UOWCAMT,BIOAMNT                                               
         J     *+10                                                             
         SP    WC.UOWCAMT,BIOAMNT                                               
         LA    R4,UOWCTABL(R4)                                                  
         LLC   RF,UOWCCNT                                                       
         AHI   RF,1                                                             
         STC   RF,UOWCCNT                                                       
         J     ORDAUD12                                                         
                                                                                
ORDAUD18 OC    UOWCCNT,UOWCCNT     ANY ORDERS ON BATCH ITEM                     
         JZ    ORDAUDX             NO - EXIT                                    
         LA    R2,IOKEY            BUILD ORDER RECORD KEY                       
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,UORDNUM                                                  
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVC   UOSTAT,ORDRSTAT     STORE OLD ORDER STATUS                       
         MVC   UOSTA2,ORDRSTA2                                                  
ORDAUD20 LA    R3,ORDRFST                                                       
         USING OAMELD,R3                                                        
         LA    R4,UOWCTAB                                                       
ORDAUD22 CLI   OAMEL,0                                                          
         JE    ORDAUD28                                                         
         CLI   OAMEL,OAMELQ                                                     
         JE    ORDAUD26                                                         
ORDAUD24 LLC   RF,OAMLN                                                         
         AR    R3,RF                                                            
         J     ORDAUD22                                                         
*                                                                               
ORDAUD26 MVC   WC.UOWCORIG,OAMWORK                                              
         LA    R4,UOWCTABL(R4)                                                  
         J     ORDAUD24                                                         
*                                                                               
ORDAUD28 LA    R4,UOWCTAB                                                       
         LA    R3,STCOMTNT                                                      
         LLC   RF,UOWCCNT                                                       
AUD      USING STCOMTNT,R3                                                      
ORDAUD30 CP    WC.UOWCAMT,BCPZERO                                               
         JNE   ORDAUD32                                                         
*&&UK                                                                           
         CP    WC.UOWCFCAM,BCPZERO                                              
*&&                                                                             
         JE    ORDAUD34                                                         
ORDAUD32 MVC   AUD.STCOOWC,WC.UOWCORIG                                          
         MVC   AUD.STCOMWC,BCSPACES                                             
         CLC   AUD.STCOOWC,BCSPACES                                             
         JE    *+10                                                             
         MVC   AUD.STCOMWC,WC.UOWCMTCH                                          
         ZAP   AUD.STCOMWCA,WC.UOWCAMT                                          
*&&UK*&& ZAP   AUD.STCOMWCF,WC.UOWCFCAM                                         
         LA    R3,L'STCOMTNT(R3)                                                
         LLC   RE,STCLN                                                         
         AHI   RE,L'STCOMTNT                                                    
         STC   RE,STCLN                                                         
         LLC   RE,STCOMWC#                                                      
         AHI   RE,1                                                             
         STC   RE,STCOMWC#                                                      
ORDAUD34 LA    R4,UOWCTABL(R4)                                                  
         JCT   RF,ORDAUD30                                                      
         DROP  WC,AUD                                                           
                                                                                
         USING AUDRECD,R2                                                       
         LA    R2,IOKEY            BUILD ORDER AUDIT RECORD                     
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUABIN                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,UORDNUM                                                 
         GOTO1 AIO,IORD+IOACCDIR+IO3                                            
         JNE   ORDAUDX                                                          
         MVC   UOSVKEY,IOKEY       SAVE OFF KEY AND SEQUENCE                    
                                                                                
ORDAUD46 GOTO1 AIO,IOSEQ+IOACCDIR+IO3 FIND LAST AUDIT RECORD                    
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   UOSVKEY(AUDKSEQ-AUDRECD),IOKEY                                   
         JNE   ORDAUD48                                                         
         MVC   UOSVKEY,IOKEY                                                    
         J     ORDAUD46                                                         
*                                                                               
ORDAUD48 MVC   IOKEY,UOSVKEY                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,AUDRLEN        GET CURRENT RECORD LENGTH                    
         JNZ   *+8                                                              
         LHI   R0,AUDRFST+1-AUDRECD                                             
         LLC   R1,STCLN            R1=L'ELEMENT TO BE ADDED                     
         AR    R0,R1               UPDATE RECORD LENGTH                         
                                                                                
         CHI   R0,2000                                                          
         JH    ORDAUD50            NO ROOM ADD A NEW AUDIT RECORD               
*                                                                               
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AUDRECD,BOELEM,(RF)                  
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         JE    ORDAUDX                                                          
         DC    H'0'                                                             
*                                                                               
ORDAUD50 L     R2,AIO3             CLEAR AIO AREA AND ADD NEW AUDIT             
         LR    R0,R2                                                            
         LHI   R1,2048                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    AUDKEY,AUDKEY                                                    
         MVC   AUDKEY,UOSVKEY                                                   
         MVC   AUDRSTAT,UOSTAT                                                  
         MVC   AUDRSTA2,UOSTA2                                                  
         LLC   RF,AUDKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,AUDKSEQ                                                       
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AUDRECD,BOELEM,(RF)                  
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOADDREC+IOACCMST+IO3                                        
         JE    ORDAUDX                                                          
         DC    H'0'                                                             
ORDAUDX  B     ROUTE                                                            
                                                                                
UOWORKD  DSECT                     ** ORDAUD LOCAL W/S **                       
UOSVKEY  DS    XL42                ORDER AUDIT SAVE KEY                         
UOSTAT   DS    XL1                 ORDER STATUS BYTE                            
UOSTA2   DS    XL1                 ORDER STATUS BYTE 2                          
UOWCCNT  DS    XL1                 WORKCODE COUNT                               
UOACTN   DS    XL1                 ACTION STATUS                                
UODELETE EQU   1                   DELETE ITEM                                  
UOADD    EQU   2                   ADD ITEM                                     
UORDNUM  DS    CL6                 ORDER NUMBER                                 
UOELEM   DS    XL255               ELEMENT                                      
*                                                                               
UOWCTAB  DS    0X                  ORDER WC TABLE                               
UOWCORIG DS    CL2                 ORIGINAL WORKCODE                            
UOWCMTCH DS    CL2                 MATCHED WORKCODE                             
UOWCAMT  DS    PL6                 AMOUNT                                       
UOWCFCAM DS    PL6                 FOREIGN CURRENCY AMOUNT                      
UOWCTABL EQU   *-UOWCTAB                                                        
UOWCMAX  EQU   6                                                                
         DS   (UOWCMAX-1)XL(UOWCTABL)                                           
         EJECT                                                                  
BAT6D    CSECT                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
DMREAD   DC    C'DMREAD  '                                                      
DMWRITE  DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
ACCMST   DC    C'ACCMST  '                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*                                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
*                                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*                                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
*                                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*                                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*                                                                               
HELPD    DSECT                                                                  
HELPH1H  DS    XL8                                                              
HELPH1   DS    CL79                HELP HEAD LINE 1                             
HELPH2H  DS    XL8                                                              
HELPH2   DS    CL79                HELP HEAD LINE 2                             
HELPL1H  DS    XL8                                                              
HELPL1   DS    CL79                HELP DATA LINE 1                             
HELPL1L  EQU   *-HELPL1H                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042ACGEN6D   05/28/13'                                      
         END                                                                    

*          DATA SET PPBUY00    AT LEVEL 069 AS OF 10/21/19                      
*PHASE T41100A                                                                  
*INCLUDE PPWRKDAY                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY00 - BUY PROGRAM BASE'                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FUNCTION LIMIT ACCESS CONTROLS                                                
*                                                                               
* T411FFD+12  X'01'  = DISPLAY ONLY                                             
*             X'02'  = WESTERN BUYER (FOR TEARSHEET ACTIONS)                    
*             X'04'  = WESTERN BILLER (FOR TEARSHEET ACTIONS)                   
*             X'08'  = TEARSHEET DISPLAY ONLY                                   
*             X'10'  = SAME AS X'01' BUT ALLOW INVOICE STAT FLD CHG             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 06/29/15 SUPPORT MEDIA B (MOBILE), V (NATL VIDEO), W (LOCAL VID)         
*                                                                               
* KWAN 02/24/14 SUPPORT MEDIA L (SOCIAL)                                        
*                                                                               
* KWAN 03/06/12 REMOVED SJ FROM FILOPT ROUTINE TO ALLOW NORMAL OPT VAL          
*                                                                               
* SMYE  01/10   ALLOW FOR RETENTION AND DISPLAY OF "S" COST INDICATOR           
*                 IN "FREE" BUYS                                                
*                                                                               
* KWAN 02/06/09 INSERTION DATE FIX FOR WMMMDD-## FORMATS (9 CHARACTERS)         
*                                                                               
* BOBY 09/00/06 ADDITIONAL CHARGES FOR PBU                                      
*                                                                               
* BOBY 07/00/06 CUSTOM COLUMNS FOR PBU                                          
*                                                                               
* KWAN 02/21/06 STEWARDSHIP INSERTION                                           
*                                                                               
* KWAN 10/04/05 NEGATIVE NET PREMIUM FIX                                        
*                                                                               
* KWAN 04/27/05 DROP CENTS TO FIT LEADING DIGIT FOR COLOR PREMIUMS              
*                                                                               
* KWAN 04/19/05 DROP CENTS FOR LARGE RATE TO DISPLAY MINUS SIGN                 
*                                                                               
* KWAN 01/18/05 NO OVERRIDES TO PBU ON-SALE, SPA CLO & MAT CLO DATES            
*                                                                               
* SMYE  06/04   STORE PPWRKDAY ADDRESS IN VPPWKDAY (PPBUYWRK1)                  
*                                                                               
* SMYE  02/04   ADD CODE FOR CUSTOM COLUMNS                                     
*                                                                               
* KWAN 02/23/04 HEX CONTROL FOR INVOICE STATUS FLD CHANGES                      
*                                                                               
* SMYE  04/03   ADD CODE FOR GLOBBER FROM CONTRACT PROGRAM                      
*                                                                               
* KWAN 04/01/03 BEST FOOD DAY & WEEK OF INSERTION TYPE FIXES FOR AB             
*                                                                               
* KWAN 03/27/03 FIX $MAD UPLD ERROR, DISALLOW BMMM,WMMM INS DATE FMT            
*                                                                               
* KWAN 10/15/01 CODES FOR ADBUYER                                               
*                                                                               
* KWAN 10/10/01 FIX ADDITIONAL CHARGES BUG (CHA/DEL MAIN BUY)                   
*                                                                               
* KWAN 09/26/01 CHANGES TO CALLS FOR T41116 (ADDITIONAL CHARGES)                
*                                                                               
* KWAN 08/16/01 NO TRAFFIC BUYS, PDBSTAT BIT IS X'20' (IN EDTINS)               
*                                                                               
* KWAN 08/14/01 CEASE DUMPS FOR UNKNOWN ERROR (GIVE IT GENERIC #)               
*                                                                               
* KWAN 06/11/01 PUT ADDITIONAL CHARGES CODE BACK                                
*                                                                               
* KWAN 04/20/01 FIX BUG IN BUMPING FLIEDS (FBUMPFDS, NOT FBUMPFD)               
*                                                                               
* KWAN 03/06/01 FIX FILSCR AND FILSCR2 BASE=* PROBLEM                           
*                                                                               
* KWAN 03/05/01 ACTIVATE NO OPED CODES FOR ADDITIONAL CHARGES                   
*                                                                               
* KWAN 03/05/01 NO OP ADDITIONAL CHARGES (FIND ON EC WORD AND MYOVRLY)          
*                                                                               
* KWAN 02/14/01 ALLOW TRANSACTION CODES FOR ADDITIONAL CHARGES                  
*                                                                               
* KWAN 02/08/01 EXPAND WRKREC TO 4001 BYTES FROM 3001 BYTES                     
*                                                                               
* SMYE 11/00    CHANGES FOR CLIENTS FROZEN BY DATE                              
*                                                                               
* BPLA 04/00    CHANGE IN FMTPR TO DISPLAY LARGE PREMIUMS                       
*               THOSE OVER 10,000 WERE GETTING TRUNCATED                        
*               IF WITH A COLOR                                                 
*                                                                               
* BPLA 03/00    CORRECT NMOD WORK AREA WAS 5 BYTES SHORT                        
*               WAS 2725 (21800 BYTES) 21805 NEEDED                             
*               CORRECTED TO 2730 (21840 BYTES) HAS SPARE                       
*                                                                               
* KWAN 12/99    EXPANDED REC AND CONIO SIZES, PROGM NEED 2725 DUBS              
*                                                                               
* BPLA 12/98    SPECIAL TRANSACTION CODE OF RL TO RETURN                        
*               FIRST PUB OF PUBLIST BY UNVALIDATING THE PUB                    
*                                                                               
* BPLA 12/98    CHANGES FOR INTERACTIVE LIST BUYING                             
*               (NEEDS PPBUY01L, PPBUY05L, PPBUY10L)                            
*                                                                               
* BPLA 08/98    CHANGES TO ALLOW FOR MAGAZINE LIST BUYING                       
*               (NEEDS PPBUY01L, PPBUY05L, PPBUY11L)                            
*                                                                               
* BPLA 04/98    CHANGES FOR FROZEN CLIENTS                                      
*                                                                               
* BPLA 11/97    USE CORE-RESIDENT GETINS                                        
*                                                                               
* BPLA 10/97    IF BUYING ON SFH ESTIMATE (SVESPROF+29 X'01')                   
*               NOTE-PPBUY01 SAVES PESTSTAT IN SVESPROF+29                      
*               IN PBDSTAT SET ON X'04' - SFH BUY                               
*               AND X'08' (HELD- NO I/O OR CONTRACT)                            
*                                                                               
* BPLA 07/97    IF I ENCOUNTER A DELETE OBJECT WITH NO                          
*               LINE NUMBER IN A PBU UPLOAD FILE                                
*               JUST RETURN RECORD NOT FOUND ERROR                              
*                                                                               
* BPLA 05/97    CHANGES FOR PREMIUMS INPUT AT NET                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PBWORKX-GENOLD,*T41100*,RR=R9,CLEAR=YES                          
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DS    F                                                                
*                                                                               
         USING GENOLD,RC                                                        
         USING T411FFD,RA                                                       
         LA    R8,T41100+4095                                                   
         LA    R8,1(R8)                                                         
         USING T41100+4096,R8      NOTE: R8 IS SECOND BASE REGISTER             
*                                                                               
         LR    R9,R1               SAVE R1                                      
         BRAS  RE,INITL            CLEAR WORK AREA                              
         LR    R1,R9               RESTORE R1                                   
*                                                                               
         MVC   ACOMFACS,16(R1)                                                  
         MVC   ATIOB,0(R1)         A(TIOB)                                      
         L     R1,8(R1)            GET A(FACLIST)                               
         MVC   VUSCAN,44(R1)                                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)    GET GLOBBER ADDR               
         MVC   VMINIO,CMINIO-COMFACSD(RF)        AND MINIO ADDR                 
         MVC   VPERVAL,CPERVAL-COMFACSD(RF)      AND PERVAL ADDR                
*                                                                               
         MVC   FULL,=X'D9000A71'                                                
         BRAS  RE,GETCORES                                                      
         MVC   VEDITOR,DMCB        STORE EDITOR ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AAB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   VGETINS,DMCB        STORE GETINS ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000A0D'                                                
         BRAS  RE,GETCORES                                                      
         MVC   SQUASHER,DMCB       STORE SQUASHER ADDRESS                       
*                                                                               
         MVC   FULL,=X'D9000AB8'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBVAL,DMCB        STORE PUBVAL ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB9'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         MVC   LENTWA,=H'18432'                                                 
*                                                                               
         LH    RF,=Y(PUBIO-GENOLD)                                              
         AR    RF,RC                                                            
         ST    RF,APUBIO                                                        
         LH    RF,=Y(CONIO-GENOLD)                                              
         AR    RF,RC                                                            
         ST    RF,ACONIO                                                        
         LH    RF,=Y(JOBIO-GENOLD)                                              
         AR    RF,RC                                                            
         ST    RF,AJOBIO                                                        
         LH    RF,=Y(COMWRK-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,ACOMWRK                                                       
         LH    RF,=Y(WRKREC-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,AWRKREC                                                       
         LH    RF,=Y(MINBLK-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,AMINBLK                                                       
         LH    RF,=Y(MINRECTB-GENOLD)                                           
         AR    RF,RC                                                            
         ST    RF,AMINRECT                                                      
         LH    RF,=Y(MINBUFFS-GENOLD)                                           
         AR    RF,RC                                                            
         ST    RF,AMINBUFF                                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,BTODAY)                                    
         ST    RB,BASERB                                                        
         ST    R8,BASER8                                                        
*                                                                               
         ST    RD,BASERD                                                        
*                                                                               
         LA    R0,NXTTR                                                         
         ST    R0,VNXTTR                                                        
*                                                                               
         L     R0,=V(PPWRKDAY)                                                  
         A     R0,RELO                                                          
         ST    R0,VPPWKDAY                                                      
*                                                                               
         L     R0,=A(EDTINS)                                                    
         A     R0,RELO                                                          
         ST    R0,VEDTINS                                                       
*                                                                               
         LA    R0,FMTINS                                                        
         ST    R0,VFMTINS                                                       
         LA    R0,FMTTR                                                         
         ST    R0,VFMTTR                                                        
*                                                                               
         L     R0,=A(FMTRTN)       FORMAT NEWSPAPER RATE                        
         A     R0,RELO                                                          
         ST    R0,VFMTRTN                                                       
*                                                                               
         LA    R0,FMTPR                                                         
         ST    R0,VFMTPR                                                        
*                                                                               
         CLI   SVSCRN,X'EC'        ADDTNL CHRGS RECALL/CHG PREVIOUSLY?          
         BE    *+12                                                             
         CLI   SVSCRN,X'EF'        CUST COLUMNS RECALL/CHG PREVIOUSLY?          
         BNE   BUYINITX                                                         
*                                                                               
         LA    R2,BUYCONH                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NEED TO RESTORE THIS TITLE                   
         XC    8(6,R2),8(R2)                                                    
         MVC   8(5,R2),=C'MATRL'                                                
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
BUYINITX DS    0H                                                               
*                                                                               
         CLI   SVBROWSE,C'B'       BROWSE IN PROGRESS ?                         
         BE    BUYGLBXX            YES                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         BRAS  RE,CKDDLINK         CK FOR DDLINK CALLS (ADBUYER)                
         BE    EXXMOD                                                           
*                                                                               
*        CHECK IF TRANSFER TO PFM                                               
*                                                                               
         CLC   BUYNM(3),=C'TST'    FOR TST ONLY OPTION TO SWITCH                
         BNE   BUYGLC03                                                         
         CLC   BUYTR1(2),=C'RM'    FOR RM ONLY                                  
         BNE   BUYGLC03                                                         
         TM    BUYNMH+4,X'80'      IF NEW ENTERED THEN SKIP                     
         BO    BUYGLC03                                                         
*                                                                               
         L     RF,ATIOB            POINT TO INPUT AREA                          
         USING TIOBD,RF            ESTABLISH AREA                               
         CLI   TIOBAID,9           LOOK FOR PFKEY 09 HIT                        
         BNE   BUYGLC03                                                         
         DROP  RF                                                               
         L     RF,=A(VGOPFM)                                                    
         A     RF,RELO                                                          
         GOTOR (RF),DMCB           GO TO PFM                                    
         B     EXXMOD                                                           
*                                                                               
BUYGLC03 DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK GLOBBER TO SEE IF THIS IS CALL FROM CONTRACT                            
* IF SO GO FILL IN FIELDS ON SCREEN WITH THOSE FROM GLOBBER                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    BUYGLBXX            TO NORMAL PROCESSING                         
*                                                                               
* SKIP IF STILL PROCESSING CALL FROM CONTRACT PROGRAM                           
*                                                                               
         CLI   CONSW,C'Y'          IF NOT DOING CONTRACT CALL                   
         BNE   BUYGLC10            CK GLOBBER AREA                              
*                                                                               
         L     RF,ATIOB            POINT TO INPUT AREA                          
         USING TIOBD,RF            ESTABLISH AREA                               
         CLI   TIOBAID,12          LOOK FOR PFKEY 12 HIT                        
         BE    BUYGLC05                                                         
         CLI   TIOBAID,24                                                       
         BNE   BUYGLCX             SKIP IF NOT PF12(24)                         
*                                                                               
BUYGLC05 DS    0H                                                               
         MVI   CONSW,C' '          CLEAR CONTRACT CALL SWITCH                   
         MVC   BUYID,=CL17'=SW '   SWAP BACK TO CONTRACT                        
         B     EXXMOD                                                           
         DROP  RF                                                               
*                                                                               
BUYGLC10 DS    0H                                                               
*                                                                               
* CHECK GLOBBER  - SEE IF CALLED FROM $CON                                      
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',X,24,GLVXCTL                              
         CLI   DMCB+8,GLEGNF                                                    
         BE    BUYGLBXX            NOT FOUND - NOT GLOBBER CALL                 
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BE SURE CALL WAS FROM PRINT $CON TO PRINT $BUY                                
*                                                                               
         CLC   X(12),=C'PRICONPRIBUY'                                           
         BNE   BUYGLCX             NOT CONTRACT CALL                            
*                                                                               
* DELETE 'OLD' TRANSFER ELEM                                                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FILL IN SCREEN FROM GLOBBER                                                   
*                                                                               
         BRAS  RE,FILSCRC                                                       
*                                                                               
         B     BUYGLBXX           "NORMAL" PROCESSING                           
*                                                                               
BUYGLCX  DS    0H                                                               
         EJECT                                                                  
*                                                                               
* CHECK GLOBBER  - SEE IF CALLED FROM $MAD                                      
*                                                                               
BUYGLB   DS    0H                                                               
*                                                                               
         XC    SAVUHDR,SAVUHDR     INIT SAVEAREA                                
*                                                                               
         OC    VGLOBBER,VGLOBBER   SKIP IF NO GLOBBER ADDRESS                   
         BZ    BUYGLBXX                                                         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',X,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,GLEGNF       SKIP IF NOT A GLOBBER CALL                   
         BE    BUYGLBXX                                                         
*                                                                               
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BE SURE CALL WAS FROM CONTROL $MAD TO PRINT $BUY                              
*                                                                               
         CLC   X(12),=C'CONMADPRIBUY'                                           
         BNE   BUYGLBXX            IF NOT-IGNORE                                
*                                                                               
* DELETE 'OLD' TRANSFER ELEM                                                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* MUST CALL IN BASE SCREEN                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D90411FF'                                           
         GOTO1 VCALLOV,DMCB,64(RA)                                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* PBU DATA STORED IN TEMP STORAGE IN TWA'S                                      
*                                                                               
* SET FOR 2ND TWA - THIS WILL REALLY START WITH TWA 3                           
*                                                                               
         MVC   CURRTWA,=F'2'                                                    
*                                                                               
         L     R5,VTIA                                                          
         MVC   0(2,R5),=X'FFFF'    SO GETTMP WILL SKIP THE FIRST TWA            
*                                                                               
         XC    DNEXTTMP,DNEXTTMP                                                
         XC    ATHISTMP,ATHISTMP                                                
*                                                                               
         BRAS  RE,VGETTMP          READ IN FIRST PBU RECORD                     
         BNE   BUYGLBX                NONE                                      
*                                                                               
         L     R5,ATHISTMP                                                      
         USING PHDRD,R5            ESTABLISH PBU DATA                           
*                                                                               
         CLC   PHDRTYPE,=C'HDR*'   MUST START WITH HEADER RECORD                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVUHDR(PHDRLENQ+2),PHDRLEN  SAVE HEADER RECORD                  
*                                                                               
         DROP  R5                                                               
*                                                                               
* RETURN HERE TO PROCESS NEXT INSERTION IN TWA BUFFER                           
*                                                                               
BUYGLB5  BRAS  RE,FILHEAD          CLR SCREEN AND FILL HEADER INFO              
*                                  (EXCEPT PUB AND BUYER)                       
         MVI   ERRAREA,0           RESET TO 0                                   
*                                                                               
         MVI   LASTACT,0                                                        
*                                                                               
BUYGLB10 BRAS  RE,VGETTMP          GET FIRST/NEXT INSERTION FOR UPLOAD          
         BNE   BUYGLBX             DONE                                         
*                                                                               
         L     R5,ATHISTMP         ESTABLISH PBU RECORD                         
         USING PDELD,R5                                                         
*                                                                               
         CLC   PDELTYPE,=C'DEL*'   SEE IF DELETION                              
         BE    BUYGLB25                                                         
*                                                                               
         CLC   PDELTYPE,=C'EOP*'   SEE IF END OF PUB                            
         BE    BUYGLB10            SKIP                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R5,2(R5)            BUMP PAST LENGTH                             
*                                                                               
         USING PINSD,R5            ESTABLISH AS INSERTION RECORD                
         CLC   PINSTYPE,=C'INS*'                                                
         BE    *+6                                                              
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
* REMINDER EDR IS DEFUNCT                                                       
*                                                                               
         CLC   SAVUHDR+PHDRUTYP-PHDRD(2),=C'E2'  EDR STYLE 2                    
         BNE   BUYGLB20                                                         
*                                                                               
* PUT MEDIA ONTO SCREEN, FOR EDR STYLE 2 MEDIA IS IN INS OBJ                    
*                                                                               
         FOUT  BUYMDH,PINSMED,1                                                 
*                                                                               
* PUT PUB INTO SCREEN, FOR EDR STYLE 2 PRINTPAK PUB IS IN PINSPUBP              
*                                                                               
         CLI   PINSMED,X'FF'       MEDIA INVALID FROM $MAD?                     
         BNE   BUYGLB12                                                         
         MVI   PINSERF,17          YES - FLAG MEDIA FIELD                       
         MVC   PINSERNO,=AL2(INVERR)                                            
         B     BUYGLB5             SKIP TO NEXT INSERTION                       
*                                                                               
BUYGLB12 DS    0H                                                               
         CLC   PINSPUBP,=6X'FF'    CHECK FOR INVALID PRINTPAK PUB               
         BNE   BUYGLB15            FROM $MAD                                    
         MVI   PINSERF,1           SET ERROR TO PUB FIELD                       
         MVC   PINSERNO,=AL2(PBERR)                                             
         B     BUYGLB5             SKIP TO NEXT INSERTION                       
*                                                                               
BUYGLB15 XC    X(24),X                                                          
         GOTO1 APUBEDIT,DMCB,PINSPUBP,(C'S',X)                                  
BUYGLB17 LA    R2,BUYPBH                                                        
         LA    R4,X                                                             
         BRAS  RE,SETIN                                                         
         B     BUYGLBXX                                                         
*                                                                               
*        FILL IN PUB FROM INS* RECORD                                           
*                                                                               
BUYGLB20 DS    0H                                                               
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(19),WORK                                                  
*                                                                               
         MVC   WORK(L'PINSPUB),PINSPUB                                          
         MVC   WORK+L'PINSPUB+1(L'PINSZONE),PINSZONE                            
         MVC   WORK+L'PINSPUB+L'PINSZONE+2(L'PINSEDTN),PINSEDTN                 
         LA    R2,L'PINSPUB+L'PINSZONE+L'PINSEDTN+2                             
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(C',',(R2))                                   
*                                                                               
         L     R2,DMCB+4           L'SQUASHED STRING                            
         MVC   BUYPB,WORK          FILL PUB                                     
         STC   R2,BUYPBH+5                                                      
         MVI   BUYPBH+4,0                                                       
         FOUT  BUYPBH              RE-TRANSMIT PUB                              
*                                                                               
         B     BUYGLBXX                                                         
         DROP  R5                                                               
*                                                                               
*        PROCESS A DELETION FROM PBU                                            
*                                                                               
BUYGLB25 DS    0H                                                               
         USING PDELD,R5            ESTABLISH PBU DEL* RECORD                    
         XC    X(24),X                                                          
*                                                                               
* IF NO LINE NUMBER RECORD WAS ADDED, JUST SEND NOT FOUND ERROR                 
*                                                                               
         CLI   PDELLINE,0                                                       
         BE    BUYGLB28                                                         
*                                                                               
         CLC   SAVUHDR+PHDRUTYP-PHDRD(2),=C'E2'  SEE IF EDR STYLE 2             
         BNE   BUYGLB35                                                         
*                                                                               
* FOR EDR STYLE 2 I MUST MEDIA IN PDELMED                                       
*                                                                               
         FOUT  BUYMDH,PDELMED,1                                                 
*                                                                               
* FOR EDR UPLOAD PDELPUB IS PWOS EDR NUMBER                                     
* IF PDELPUB+5 IS X'FF' OTHERWISE IT IS A PRINTPAK PUB                          
*                                                                               
         CLI   PDELPUB+5,X'FF'     SEE IF EDR PUB NUMBER                        
         BNE   BUYGLB35                                                         
*                                                                               
* READ PRT DIRECTORY FOR AGY PUB CODE                                           
*                                                                               
         XC    KEY,KEY                                                          
         UNPK  X(9),PDELPUB(5)                                                  
         MVC   KEY+4(8),X                                                       
*                                                                               
         MVC   KEY(2),AGYALPHA                                                  
         MVI   KEY+2,C'Z'                                                       
         MVI   KEY+3,X'E3'                                                      
         MVC   KEY+15(1),PDELMED   NOTE THAT CLIENT IS NOT ACTIVE YET           
         GOTOR HIGH                                                             
         CLC   KEY(15),KEYSAVE     SEE IF I FOUND IT                            
         BE    BUYGLB30            IF NOT FOUND THEN EXIT                       
*                                                                               
BUYGLB28 DS    0H                                                               
         LA    R3,NFNDERR          NOT FOUND ERROR                              
         STC   R3,PDELERNO+1                                                    
         B     BUYGLB5             GO TRY NEXT INSERTION                        
*                                                                               
BUYGLB30 XC    X(24),X                                                          
         GOTO1 APUBEDIT,DMCB,KEY+16,(C'S',X)                                    
         B     BUYGLB40                                                         
*                                                                               
*        PUT PUB FROM DEL* RECORD ON SCREEN                                     
*                                                                               
BUYGLB35 GOTO1 APUBEDIT,DMCB,PDELPUB,(C'S',X)                                   
*                                                                               
BUYGLB40 LA    R2,BUYPBH                                                        
         LA    R4,X                                                             
         BRAS  RE,SETIN                                                         
         B     BUYGLBXX                                                         
*                                                                               
BUYGLBX  DS    0H                  FINISHED - RETURN TO $MAD                    
*                                                                               
         XC    X(24),X                                                          
         LA    R1,X                                                             
*                                                                               
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'    FROM PRINT BUY                               
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'CON'    TO CONTROL $MAD                              
         MVC   GLVXTOPR,=C'MAD'                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1RETG        RETURN WITH GLOBALS            
         DROP  R1                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',X,24,GLVXCTL                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ERRAREA,0                                                        
*                                                                               
         SR    RF,RF                                                            
         LA    R2,BUYMSGH          FIRST FIELD ON SCREEN                        
         IC    RF,BUYMSGH          LENGTH OF FIRST FIELD                        
         LA    R2,0(RF,R2)         POINT TO SERVICE REQUEST FIELD               
*                                                                               
         B     EXXMOD                                                           
         DROP  R5                                                               
*                                                                               
BUYGLBXX DS    0H                                                               
*                                                                               
         MVI   SVBROWSE,C' '       CLEAR BROWSE INDICATOR                       
*                                                                               
         MVI   BUYMSGH+7,60        SET LEN TO MAX                               
         XC    BUYMSG,BUYMSG       CLEAR LAST MESSAGE                           
         FOUT  BUYMSGH                                                          
         OI    BUYCL+2,C' '        ENSURE NO BINARY ZERO                        
         OI    BUYPR+2,C' '                                                     
*                                                                               
* TEST IF ALL HEADLINE FIELDS VALIDATED                                         
*                                                                               
         TM    BUYMDH+4,X'20'      SKIP IF MEDIA NOT VALIDATED                  
         BZ    BUY2                                                             
*                                                                               
         LA    R2,BUYNMH           REQUESTOR VALID IF PRESENT                   
         BRAS  RE,ANY                                                           
*                                                                               
         CLI   BUYNM,C'*'                                                       
         BE    BUY1                                                             
*                                                                               
         CLI   5(R2),4                                                          
         BNE   BUY1                                                             
*                                                                               
         LA    R3,2                INITIALS INVALID                             
         BE    ERROR               CAN'T HAVE 4 CHARACTERS                      
*                                                                               
BUY1     DS     0H                                                              
*                                                                               
         CLC   BUYTR1(2),=C'RF'    SPECIAL TRANSACTION CODE                     
*                                  RECALL FIRST PUB IN PUBLIST                  
         BNE   BUY1A                                                            
*                                                                               
         NI    BUYPBH+4,X'DF'      UNVALIDATING THE PUB SHOULD DO IT            
         MVC   BUYTR1(2),=C'R '    CHANGE TO RECALL                             
         MVI   BUYTR1H+5,1         FORCE LENGTH                                 
         FOUT  BUYTR1H                                                          
*                                                                               
* NOTE:  IF THE PUB WAS NOT A PUBLIST RF WILL BECOME A NORMAL RECALL            
*                                                                               
BUY1A    TM    BUYCLH+4,X'20'                                                   
         BZ    BUY2                                                             
         TM    BUYPRH+4,X'20'                                                   
         BZ    BUY2                                                             
         TM    BUYESH+4,X'20'                                                   
         BZ    BUY2                                                             
         TM    BUYPBH+4,X'20'                                                   
         BZ    BUY2                                                             
         B     BUY10                                                            
*                                                                               
* CALL HEADLINE EDIT                                                            
*                                                                               
BUY2     DS    0H                                                               
         MVI   DMCB,1              OVERLAY 1                                    
         BRAS  RE,GETOVLY                                                       
         CLI   ERRAREA,0           TEST ERRORS IN HEADLINES                     
         BE    BUY3                NO                                           
*                                                                               
         CLI   MADSW,C'Y'          SEE IF FROM $MAD                             
         BNE   BUY2X                                                            
*                                                                               
MADHERR  DS    0H                  $MAD HEADLINE ERRORS                         
*                                                                               
         L     R5,ATHISTMP                                                      
         USING PDELD,R5                                                         
*                                                                               
         CLC   PDELTYPE,=C'DEL*'                                                
         BNE   MADHE10                                                          
*                                                                               
         CLI   ERRAREA,C'M'        MEANS COULDN'T FIND THE BUY                  
         BNE   MADHE5                                                           
         LA    R3,NFNDERR                                                       
         STC   R3,PDELERNO+1                                                    
         B     BUYGLB5             SKIP TO NEXT INSERTION                       
*                                                                               
MADHE5   L     R4,ERRAREA                                                       
         CLC   8(2,R4),=C'EP'                                                   
         BE    *+16                                                             
         MVI   PDELERNO+0,0        UNKNOWN ERROR                                
         MVI   PDELERNO+1,2        FORCE IT TO INVALID ERROR                    
         B     BUYGLB5                                                          
*                                                                               
         PACK  DUB,11(4,R4)                                                     
         CVB   R0,DUB                                                           
         STH   R0,PDELERNO                                                      
         B     BUYGLB5             GO TRY NEXT INSERTION                        
         DROP  R5                                                               
*                                                                               
         USING PINSD,R5                                                         
MADHE10  LA    R5,2(R5)            BUMP PAST LENGTH                             
         CLC   PINSTYPE,=C'INS*'                                                
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         CLI   ERRAREA,C'M'        MEANS COULDN'T FIND THE BUY                  
         BNE   MADHE15                                                          
         LA    R3,NFNDERR                                                       
         STC   R3,PINSERNO+1                                                    
         MVI   PINSERF,2           HDR ERR, SET FLD NUMBER TO DATE              
         B     BUYGLB5             GO TRY NEXT INSERTION                        
*                                                                               
MADHE15  L     R4,ERRAREA                                                       
         CLC   8(2,R4),=C'EP'                                                   
         BE    *+16                                                             
         MVI   PINSERNO+0,0        UNKNOWN ERROR                                
         MVI   PINSERNO+1,2        FORCE IT TO INVALID ERROR                    
         B     MADHE15H                                                         
*                                                                               
         PACK  DUB,11(4,R4)                                                     
         CVB   R0,DUB                                                           
         STH   R0,PINSERNO                                                      
MADHE15H MVI   PINSERF,1           HDR ERR, SET FLD NUMBER TO PUB               
         B     BUYGLB5             GO TRY NEXT INSERTION                        
*                                                                               
BUY2X    NI    BUYPBH+4,X'DF'      YES - FORCE A RE-EDIT                        
         B     EXXMOD                                                           
         DROP  R5                                                               
*                                                                               
BUY3     DS    0H                                                               
         XC    LASTPBEL,LASTPBEL   CLEAR RX POINTER                             
         XC    LASTCELD,LASTCELD   CLEAR RA DATE                                
         MVI   CHGELCNT,0          INITIALIZE CHG ELEM COUNTER                  
         XC    INVELDTE,INVELDTE   CLEAR RY DATE                                
         MVI   INVELCNT,0          INITIALIZE INV ELEM COUNTER                  
*                                                                               
         LA    R2,BUYTR1H                                                       
         CLI   5(R2),0             TEST TR CODE GIVEN                           
         BNE   BUY4                                                             
         FOUT  BUYMSGH,=C'ENTER TRANSACTION DATA'                               
*                                                                               
         CLI   MADSW,C'Y'          SEE IF FROM $MAD UPLOAD                      
         BNE   DONE                                                             
*                                                                               
         BRAS  RE,FILSCR           FILL REST OF SCREEN                          
*                                                                               
         CLI   BYTE4,0             CHECK FOR ERROR                              
         BE    BUY4                   NONE                                      
         B     BUYGLB5             SKIP TO NEXT INSERTION                       
*                                                                               
BUY4     DS    0H                                                               
         EJECT                                                                  
BUY10    CLI   SVSCRN,X'FE'        CHK FOR WSJ SCREEN                           
         BE    BUY10X              YES - DON'T TOUCH SVINS                      
*                                                                               
* REMOVE ALL ENTRIES FROM SVINS TABLE FOR                                       
* FIELDS NOT TRANSMITTED THIS TIME                                              
*                                                                               
BUY10C   L     R4,LASTFLD                                                       
         SR    R4,RA                                                            
         LA    R1,SVINS                                                         
         LA    R0,L'SVINS/6                                                     
*                                                                               
         CH    R4,0(R1)                                                         
         BNL   *+10                                                             
         XC    0(6,R1),0(R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-18                                                          
*                                                                               
* CLEAR PROTECTED DISPLAY LINES AS REQUIRED                                     
*                                                                               
BUY10X   LA    R4,BUYTR1H                                                       
*                                                                               
         CLC   8(2,R4),=C'RX'                                                   
         BE    *+10                                                             
         XC    LASTPBEL,LASTPBEL   CLEAR RX POINTER                             
*                                                                               
         CLC   8(2,R4),=C'RA'                                                   
         BE    *+14                                                             
         XC    LASTCELD,LASTCELD   CLEAR RA DATE                                
         MVI   CHGELCNT,0          AND CHG ELEM COUNTER                         
*                                                                               
         CLC   8(2,R4),=C'RY'                                                   
         BE    *+14                                                             
         XC    INVELDTE,INVELDTE   CLEAR RY DATE                                
         MVI   INVELCNT,0          AND INV ELEM COUNTER                         
*                                                                               
         SR    R5,R5                                                            
BUY11A   IC    R5,0(R4)                                                         
         AHI   R5,-9                                                            
         CLI   0(R4),86                                                         
         BNE   BUY11B                                                           
         EX    R5,BUYCLROC                                                      
         BZ    BUY11B                                                           
         EX    R5,BUYCLRXC                                                      
         FOUT  (R4)                                                             
BUY11B   LA    R4,9(R4,R5)                                                      
         CLI   0(R4),0                                                          
         BNE   BUY11A                                                           
         B     BUY12                                                            
BUYCLROC OC    8(0,R4),8(R4)                                                    
BUYCLRXC XC    8(0,R4),8(R4)                                                    
*                                                                               
BUY12    XC    TRADDR,TRADDR                                                    
*                                                                               
* CALCULATE OVERLAY NUMBER                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
*                                                                               
         MVI   DMCB,X'20'          CUSTCOL OVERLAY                              
*                                                                               
         CLC   BUYTR1(2),=C'RU'    TEST CUSTOM COLUMN RECALL                    
         BE    *+14                                                             
         CLC   BUYTR1(2),=C'CU'    CUSTOM COLUMN CHANGE?                        
         BNE   BUY12C                                                           
*                                                                               
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
*                                                                               
         CLI   SVSCRN,X'EF'        SEE IF LAST WAS CUST COLM RECALL/CHG         
         BNE   BUY2                MUST RECALL HEADLINE EDIT                    
*                                                                               
         B     BUY12OK                                                          
*                                                                               
BUY12C   DS    0H                                                               
         MVI   DMCB,X'07'                                                       
         CLC   BUYTR1(2),=C'RT'    TEST TEARSHEET RECALL                        
         BNE   *+16                                                             
         CLI   SVSCRN,X'F4'        SEE IF LAST WAS TEARSHEET RECALL/CHG         
         BNE   BUY2                MUST RECALL HEADLINE EDIT                    
         B     BUY12OK                                                          
*                                                                               
         MVI   DMCB,X'16'                                                       
         CLC   BUYTR1(2),=C'RC'    ADDITIONAL CHARGE RECALL?                    
         BE    *+14                                                             
         CLC   BUYTR1(2),=C'CC'    ADDITIONAL CHARGE CHANGE?                    
         BNE   BUY12H                                                           
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
         CLI   SVSCRN,X'EC'        ADDTNL CHRGS RECALL/CHG PREVIOUSLY?          
         BNE   BUY2                MUST RECALL HEADLINE EDIT                    
         B     BUY12OK                                                          
*                                                                               
BUY12H   MVI   DMCB,5              SET RECALL OVERLAY                           
         CLI   BUYTR1,C'R'         TEST RECALL                                  
         BNE   BUY121                                                           
         CLI   SVSCRN,X'F4'        TEARSHEET RECALL/CHG PREVIOUSLY?             
         BE    BUY2                MUST RECALL HEADLINE EDIT                    
         CLI   SVSCRN,X'EC'        ADDTNL CHRGS RECALL/CHG PREVIOUSLY?          
         BE    BUY2                MUST RECALL HEADLINE EDIT                    
         CLI   SVSCRN,X'EF'        CUSTOM COL'S RECALL/CHG PREVIOUSLY?          
         BE    BUY2                MUST RECALL HEADLINE EDIT                    
*                                                                               
BUY12OK  BRAS  RE,GETOVLY                                                       
*                                                                               
         CLI   MADSW,C'Y'          SEE IF DOING $MAD UPLOAD                     
         BE    CKMADERR                                                         
*                                                                               
         CLI   ERRAREA,0                                                        
         BE    BUY16                                                            
         B     EXXMOD                                                           
*                                                                               
BUY121   DS    0H                                                               
         CLI   SVESPROF,C'1'       LOCKED OUT                                   
         BE    BUY12A                                                           
         CLI   SVESPROF,C'2'       PERM LOCKOUT                                 
         BNE   BUY12B                                                           
*                                                                               
BUY12A   LA    R3,LOCKERR          NO BUYS/CHA/DEL ON LOCKEDOUT ESTS            
         LA    R2,BUYTR1H                                                       
         B     ERROR                                                            
*                                                                               
BUY12B   DS    0H                                                               
         CLI   T411FFD+1,C'*'      DDS TERMINAL - SKIP LIMIT CHECK              
         BE    BUY12E                                                           
*                                                                               
         CLI   MADSW,C'Y'          SEE IF UPLOADING FROM $MAD                   
         BE    BUY12E              YES - IGNORE LIMIT ACCESS                    
*                                                                               
         CLI   SVSCRN,X'F4'        TEARSHEET EDIT? (T41115)                     
         BE    BUY12E                                                           
*                                                                               
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         BO    NAFTFERR                                                         
*                                                                               
         MVI   BYTE4,X'01'         FOR BUY CHA DEL                              
         NC    BYTE4(1),T411FFD+12 CHK FOR ACTION LIMIT ACCESS                  
         BZ    BUY12E                                                           
*                                                                               
NAFTFERR LA    R3,FACCERR          NOT AUTHORIZED FOR THIS FUNCTION             
         LA    R2,BUYTR1H                                                       
         B     ERROR                                                            
*                                                                               
BUY12E   DS    0H                                                               
*                                                                               
         MVI   DMCB,X'20'          CUSTOM COLUMNS EDIT OVERLAY                  
*                                                                               
         CLI   SVSCRN,X'EF'        CUSTOM COLUMNS SCREEN                        
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'16'          ADDITIONAL CHARGES EDIT OVERLAY              
         CLI   SVSCRN,X'EC'        ADDITIONAL CHARGES SCREEN                    
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'15'          TEARSHEET OVERLAY - EDIT                     
         CLI   SVSCRN,X'F4'        TEARSHEET SCREEN                             
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'14'                                                       
         CLI   SVSCRN,X'FE'        NEWS - WSJ SCREEN                            
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'12'          NEWS - OVERLAY 12                            
         CLI   BUYMD,C'N'                                                       
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'11'          MAGS - OVERLAY 11                            
         CLI   BUYMD,C'M'                                                       
         BE    BUY14                                                            
*                                                                               
         CLI   BUYMD,C'S'                                                       
         BE    BUY14                                                            
*                                                                               
         CLI   BUYMD,C'T'                                                       
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'13'          OUTDOOR  - OVERLAY 13                        
         CLI   BUYMD,C'O'                                                       
         BE    BUY14                                                            
*                                                                               
         MVI   DMCB,X'10'          INTERACTIVE - OVERLAY 10                     
         CLI   BUYMD,C'I'                                                       
         BE    BUY14                                                            
         CLI   BUYMD,C'L'          SOCIAL - SAME OVERLAY AS INTERACTIVE         
         BE    BUY14                                                            
         CLI   BUYMD,C'B'          MOBILE - SAME AS INTERATIVE                  
         BE    BUY14                                                            
         CLI   BUYMD,C'V'          NVIDEO - SAME AS INTERATIVE                  
         BE    BUY14                                                            
         CLI   BUYMD,C'W'          LVIDEO - SAME AS INTERATIVE                  
         BE    BUY14                                                            
         CLI   BUYMD,C'D'          DIGITAL AUDIO - MAP TO INTERACTIVE           
         BE    BUY14                                                            
*                                                                               
         DC    H'0'                UNKNOWN MEDIA, SOMETHING VERY WRONG          
*                                                                               
BUY14    DS    0H                                                               
         BRAS  RE,GETOVLY          LOAD AND GO TO OVERLAY                       
*                                                                               
         CLI   MADSW,C'Y'          SEE IF FROM $MAD UPLOAD                      
         BNE   BUY14N                                                           
*                                                                               
CKMADERR DS    0H                  $MAD INSERTION DATA ERRORS                   
*                                                                               
         CLI   ERRAREA,C'D'        ACTION COMPLETED                             
         BE    CKMADE20                                                         
         CLI   ERRAREA,0           OR NO ERROR                                  
         BE    CKMADE20                                                         
*                                                                               
         L     R5,ATHISTMP                                                      
         USING PDELD,R5                                                         
         CLC   PDELTYPE,=C'DEL*'                                                
         BNE   CKMADE4                                                          
         CLI   ERRAREA,C'M'        MEANS COULDN'T FIND THE BUY                  
         BNE   CKMADE2                                                          
         LA    R3,NFNDERR                                                       
         STC   R3,PDELERNO+1                                                    
         B     BUYGLB5             GO TRY NEXT INSERTION                        
*                                                                               
CKMADE2  L     R4,ERRAREA                                                       
         CLC   8(2,R4),=C'EP'                                                   
         BE    *+16                                                             
         MVI   PDELERNO+0,0        UNKNOWN ERROR                                
         MVI   PDELERNO+1,2        FORCE IT TO INVALID ERROR                    
         B     BUYGLB5                                                          
*                                                                               
         PACK  DUB,11(4,R4)                                                     
         CVB   R0,DUB                                                           
         STH   R0,PDELERNO                                                      
         B     BUYGLB5             GO TRY NEXT INSERTION                        
*                                                                               
         DROP  R5                                                               
*                                                                               
CKMADE4  DS    0H                                                               
         LA    R5,2(R5)            BUMP PAST LENGTH                             
*                                                                               
         USING PINSD,R5                                                         
         CLC   PINSTYPE,=C'INS*'                                                
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         CLI   ERRAREA,C'M'        MEANS COULDN'T FIND THE BUY                  
         BNE   CKMADE5                                                          
         LA    R3,NFNDERR                                                       
         STC   R3,PINSERNO+1                                                    
         MVI   PINSERF,2           HDR ERR, SET FLD NUMBER TO DATE              
         B     MADERRX                                                          
*                                                                               
CKMADE5  L     R4,ERRAREA          EXTRACT ERROR NUMBER FROM MESSAGE            
         CLC   8(2,R4),=C'EP'                                                   
         BE    *+16                                                             
         MVI   X+0,0               UNKNOWN ERROR                                
         MVI   X+1,2               FORCE IT TO INVALID ERROR                    
         B     CKMADE6                                                          
*                                                                               
         PACK  DUB,11(4,R4)                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,X                                                           
*                                                                               
CKMADE6  DS    0H                                                               
         MVI   PINSERF,1           HDR ERR, SET FLD NUMBER TO PUB               
         TM    BUYPBH+6,OI1C       SEE IF CURSOR ON PUB                         
         BNZ   MADIERR             GO TRY NEXT INSERTION                        
*                                                                               
         MVI   PINSERF,0                                                        
         LA    R2,BUYTR1H          TRANSACTION FIELD                            
*                                                                               
         CLI   SAVUHDR+PHDRUTYP-PHDRD,C'E'       SEE IF EDR UPLOAD              
         BNE   CKMADE8                                                          
         CLI   SAVUHDR+PHDRRATE-PHDRD,C'L'       SEE IF I'M TO USE              
         BNE   CKMADE8                           EDR RATE ONLY                  
*                                                                               
* IF NO CONTRACT OR PUB                                                         
*                                                                               
         TM    6(R2),OI1C          SEE IF CURSOR ON TRANSACTION                 
         BZ    CKMADE8                                                          
         CLC   X(2),=H'130'        SEE IF RATE NOT FOUND ERROR                  
         BNE   CKMADE8                                                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP PAST DATE                               
         BRAS  RE,BUMPFLD          BUMP PAST JOB                                
         BRAS  RE,BUMPFLD          BUMP PAST SPACE                              
         BRAS  RE,BUMPFLD          BUMP TO RATE                                 
         LA    R4,PINSCOST                                                      
*                                                                               
         CLI   BUYMD,C'N'          SEE IF NON-NEWSPAPERS                        
         BNE   CKMADE7             THEN USE PINSCOST                            
*                                                                               
         LA    R4,PINSRATE                                                      
         CLI   PINSCOST,C' '       SEE IF I HAVE A "TOTAL" COST                 
         BE    CKMADE7             NO THEN USE UNIT RATE                        
         MVI   WORK,C' '                                                        
         MVC   WORK+1(12),WORK                                                  
         MVI   WORK,C'T'                                                        
         MVC   WORK+1(9),PINSCOST                                               
         LA    R4,WORK                                                          
*                                                                               
CKMADE7  BRAS  RE,SETIN                                                         
         XC    BUYMSG,BUYMSG                                                    
         MVI   BUYMSGH+7,60                                                     
         FOUT  BUYMSGH                                                          
         MVI   ERRAREA,0                                                        
         NI    BUYTR1H+6,X'BF'     UNSET CUROSR - TRANSACTION                   
         NI    BUYDT1H+6,X'BF'     UNSET CUROSR - DATE                          
*                                                                               
         CLI   BUYTR1,C'B'         SEE IF BUYING                                
         BE    BUY4                                                             
         MVC   BUYTR1(2),=C'C '                                                 
         FOUT  BUYTR1H                                                          
         MVI   BUYTR1H+5,1         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'C'                                                     
         B     BUY12               FOR CHANGES                                  
*                                                                               
CKMADE8  DS    0H                                                               
         MVI   PINSERF,2           INSERTION DATE ERROR                         
         TM    BUYTR1H+6,OI1C      SEE IF CURSOR ON TRANSACTION CODE            
         BNZ   MADIERR             GO TRY NEXT INSERTION                        
         TM    BUYDT1H+6,OI1C      OR DATE                                      
         BNZ   MADIERR             GO TRY NEXT INSERTION                        
         MVI   PINSERF,3           JOB                                          
         TM    BUYAD1H+6,OI1C      SEE IF CURSOR ON JOB                         
         BNZ   MADIERR             GO TRY NEXT INSERTION                        
         LA    R2,BUYAD1H          THIS IS THE LAST FIELD                       
*                                  COMMON TO ALL MEDIAS                         
         BRAS  RE,BUMPFLD                                                       
         CLI   BUYMD,C'N'          SEE IF NEWSPAPERS                            
         BE    MADNERR                                                          
         CLI   BUYMD,C'O'          SEE IF OUTDOOR                               
         BE    MADOERR                                                          
MADMERR  DS    0H                  MAGAZINE ERRORS                              
         MVI   PINSERF,4           SPACE                                        
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
MADME5   MVI   PINSERF,8           COST                                         
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
MADE10   BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,12          CLOSING DATE                                 
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,13          ON-SALE                                      
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,14          MATERIALS CLOSING                            
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         MVI   PINSERF,0           NOT IN THIS RECORD                           
         B     MADOPTE             CHK ERROR IN OPTIONAL DATA                   
*                                                                               
MADNERR  DS    0H                  NEWSPAPER ERRORS                             
         MVI   PINSERF,4           SPACE                                        
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,8                                                        
         CLI   PINSCOST,C' '       SEE IF COST GIVEN                            
         BNE   *+8                                                              
         MVI   PINSERF,9           SET FOR RATE                                 
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,10                                                       
         CLI   PINSPDSC,C' '                                                    
         BNE   *+8                                                              
         MVI   PINSERF,11                                                       
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,14          MATERIALS CLOSING                            
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         MVI   PINSERF,0           NOT IN THIS RECORD                           
         B     MADOPTE             CHK FOR ERROR IN OPTIOAL DATA                
*                                                                               
MADOERR  DS    0H                  OUTDOOR ERRORS                               
         MVI   PINSERF,5           SHOWING                                      
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,6           REGULAR PANELS                               
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,7           ILLUMINATED PANELS                           
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,8           COST                                         
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,12          CLOSING DATE                                 
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         BRAS  RE,BUMPFLD                                                       
         MVI   PINSERF,14          MATERIALS CLOSING                            
         TM    6(R2),OI1C          CHECK CURSOR                                 
         BNZ   MADIERR                                                          
         MVI   PINSERF,0           NOT IN THIS RECORD                           
         B     MADOPTE             CHK FOR ERROR IN OPTIONAL DATA               
*                                                                               
MADIERR  MVC   PINSERNO(2),X       SET ERROR CODE INTO INSERTION REC            
         B     BUYGLB5             GO PROCESS NEXT INSERTION                    
         DROP  R5                                                               
*                                                                               
MADOPTE  DS    0H                  SEE IF OPTIONAL DATA LINE CAUSED ERR         
         LHI   RF,2                                                             
         BRAS  RE,BUMPFLDS         SETS R2 TO FIRST OPTIONAL DATA               
         ST    R5,SAVER5           SAVE ADDR OF INSERTION                       
         ST    R2,SAVER2           SAVE ADDR OF FIRST OPT DATA LINE             
*                                                                               
         LA    R6,5                FOR BCT                                      
MADOE5   ZICM  R1,0(R5),(3)                                                     
         AR    R5,R1                                                            
*                                                                               
         USING POPTD,R5                                                         
*                                                                               
         CLC   POPTTYPE,=C'EIN*'   SEE IF AT END OF INS                         
         BE    MADOEX                                                           
         CLC   POPTTYPE,=C'ZZZ*'   SEE IF ZZZ DATA                              
         BE    MADOEZ                                                           
         CLC   POPTTYPE,=C'CCL*'   SEE IF CUSTOM COLUMN DATA                    
         BE    MADOCCL                                                          
         CLC   POPTTYPE,=C'ACH*'   SEE IF ADDITIONAL CHARGE DATA                
         BE    MADOACH                                                          
         CLC   POPTTYPE,=C'OPT*'                                                
         BE    MADOOPT                                                          
*                                                                               
         DC    H'0'                BAD DATA                                     
*                                                                               
MADOCCL  DS    0H                  CUSTOM COLUMN RECORD                         
*                                                                               
MADOACH  DS    0H                  ADDITIONAL CHARGE RECORD                     
*                                                                               
MADOOPT  DS    0H                  OPTIONAL DATA RECORD                         
*                                                                               
         TM    6(R2),OI1C          SEE IF CURSOR SET TO THIS FIELD              
         BZ    MADOE10                                                          
         MVC   POPTERNO,X          STORE SAVED ERROR CODE                       
         B     MADERRX                                                          
*                                                                               
MADOE10  BRAS  RE,BUMPFLD          BUMP TO NEXT OPTIONAL DATA LINE              
         BCT   R6,MADOE5                                                        
*                                                                               
MADOEZ   DS    0H                  CHK FOR ERROR IN ZZZ DATA                    
         L     R5,SAVER5           RESTORE R5 TO BEGINNING                      
         L     R2,SAVER2           RESTORE R2 TO 1ST OPTIONAL DATA LINE         
*                                                                               
MADEOZ5  ZICM  R1,0(R5),2                                                       
         AR    R5,R1                                                            
*                                                                               
         CLC   POPTTYPE,=C'EIN*'   SEE IF AT END OF INS                         
         BE    MADOEX                                                           
         CLC   POPTTYPE,=C'CCL*'                                                
         BE    MADEOZ5                                                          
         CLC   POPTTYPE,=C'ACH*'                                                
         BE    MADEOZ5                                                          
         CLC   POPTTYPE,=C'OPT*'                                                
         BE    MADEOZ5                                                          
         CLC   POPTTYPE,=C'ZZZ*'   SEE IF ZZZ DATA                              
         BE    *+6                                                              
         DC    H'0'                BAD DATA                                     
*                                                                               
         CLC   BUYPR,=C'ZZZ'       SEE IF PRODUCT IS ZZZ                        
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         USING PZZZD,R5                                                         
*                                                                               
         LHI   RF,6                                                             
         BRAS  RE,BUMPFLDS                                                      
*                                                                               
         TM    6(R2),OI1C          SEE IF CURSOR SET HERE                       
         BZ    MADOEX              NO, SET ERROR IN INSERTION RECORD            
*                                                                               
         MVC   PZZZERNO,X          STORE SAVED ERROR CODE                       
         B     MADERRX                                                          
         DROP  R5                                                               
*                                                                               
MADOEX   DS    0H                                                               
*                                                                               
* WILL GET HERE IF ERROR WAS NOT CAUSED BY AN UPLOAD FIELD                      
* E.G. MISSING ALLOCATION DATA FOR A ZZZ BUY                                    
*                                                                               
         L     R5,SAVER5           RESET TO INSERTION RECORD                    
         USING PINSD,R5                                                         
         MVI   PINSERF,1           SET FIELD ERROR TO PUB                       
         MVC   PINSERNO(2),X       ERROR CODE                                   
         DROP  R5                                                               
*                                                                               
MADERRX  B     BUYGLB5             GO TRY NEXT INSERTION                        
*                                                                               
*        NO ERRORS                                                              
*                                                                               
CKMADE20 DS    0H                  HERE IF NO ERRORS                            
*                                                                               
         L     R5,ATHISTMP                                                      
         USING PDELD,R5                                                         
*                                                                               
         CLC   PDELTYPE,=C'DEL*'                                                
         BNE   CKMADE24                                                         
*                                                                               
         CLI   LASTACT,C'D'        SEE IF LAST ACT WAS DELETE                   
         BE    CKMADEX             FINISHED - GO DO NEXT INSERTION              
*                                                                               
         OC    PDELERNO,PDELERNO   SEE IF ANY ERROR                             
         BZ    CKMADE22                                                         
*                                                                               
         B     CKMADEX             GO DO NEXT INSERTION                         
*                                                                               
CKMADE22 DS    0H                                                               
*                                                                               
         BRAS  RE,SETSCDEL         SET SCREEN FOR DELETE                        
*                                                                               
         MVC   BUYTR1(2),=C'DL'                                                 
         FOUT  BUYTR1H                                                          
         MVI   BUYTR1H+5,2         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'D'                                                     
         B     BUY12               GO PROCESS DELETION                          
         DROP  R5                                                               
*                                                                               
CKMADE24 DS    0H                                                               
*                                                                               
         LA    R5,2(R5)            MUST BUMP PAST LENGTH                        
*                                                                               
         USING PINSD,R5                                                         
*                                                                               
         CLC   PINSTYPE,=C'INS*'                                                
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         CLI   PINSACTN,C'A'       SEE IF NEW BUY                               
         BNE   CKMADE30                                                         
*                                                                               
         CLI   PINSLINE,0          SHOULD BE ZERO                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PINSLINE,PBUYKLIN   SET LINE NUMBER IN TEMPSTR                   
*                                                                               
         B     CKMADE40                                                         
*                                                                               
         B     CKMADEX             MAY NEED TO KEEP                             
*                                                                               
CKMADE30 CLI   LASTACT,C'C'        SEE IF LAST ACTION WAS CHANGE                
         BE    CKMADE40            YES THEN DONE WITH THIS INSERTION            
*                                                                               
         MVC   BUYTR1(2),=C'C '                                                 
         FOUT  BUYTR1H                                                          
         MVI   BUYTR1H+5,1         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'C'                                                     
*                                                                               
         BRAS  RE,FILSCR2          GO FILL REST OF SCR                          
*                                                                               
         B     BUY12               THEN PROCESS                                 
*                                                                               
CKMADE40 DS    0H                                                               
*                                                                               
*        CHECK FOR CUSTOM COLUMN DATA                                           
*                                                                               
         SR    RF,RF                                                            
*                                                                               
CKMADE41 DS    0H                                                               
*                                                                               
         ICM   RF,3,0(R5)          BUMP TO NEXT PBU RECORD                      
         AR    R5,RF                                                            
         USING PCCLD,R5            ESTABLISH AS CUSTOM COLUMN RECORD            
*                                                                               
         CLC   PCCLTYPE,=C'EIN*'   DONE IF END OF INSERTION                     
         BE    CKMADE45                                                         
*                                                                               
         CLC   PCCLTYPE,=C'CCL*'   CUSTOM COLUMN FOUND                          
         BE    CKMADE42                                                         
*                                                                               
         B     CKMADE41                                                         
*                                                                               
CKMADE42 DS    0H                                                               
*                                                                               
         MVC   BUYTR1(2),=C'RU'    RECALL CUSTOM COLUMN DATA                    
*                                                                               
         FOUT  BUYTR1H                                                          
*                                                                               
         MVI   BUYTR1H+5,2         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'R'                                                     
*                                                                               
*        RECALL CUSTOM COLUMN SCREEN                                            
*                                                                               
         MVI   BYTE,X'EF'          X'EF' IS CUSTCOL SCREEN                      
         BRAS  RE,MYOVRLY          GET CCOL SCREEN AND PGM                      
*                                                                               
         BRAS  RE,FILSCR3          FILL IN CUSTOM COLUMN DATA                   
*                                                                               
         MVC   BUYTR1(2),=C'CU'    CHANGE CUSTOM COLUMN DATA                    
*                                                                               
         FOUT  BUYTR1H                                                          
*                                                                               
         MVI   BUYTR1H+5,2         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'C'                                                     
*                                                                               
         XC    DOUBLE,DOUBLE       CLEAR SAVED DATE                             
         MVI   ERRAREA,0           INIT MESSAGE NUMBER                          
         MVI   BYTE,X'EF'          X'EF' IS CUSTCOL SCREEN                      
         BRAS  RE,MYOVRLY          GET CCOL SCREEN AND PGM                      
*                                                                               
*        CHECK FOR ERRORS                                                       
*                                                                               
         CLI   ERRAREA,C'D'        ACTION COMPLETED                             
         BE    CKMADE45                                                         
         CLI   ERRAREA,C'K'        ACTION COMPLETED                             
         BE    CKMADE45                                                         
         CLI   ERRAREA,0           OR NO ERROR                                  
         BE    CKMADE45                                                         
*                                                                               
         L     R4,ERRAREA                                                       
*                                                                               
         CLC   8(2,R4),=C'EP'                                                   
         BE    *+16                                                             
         MVI   PCCLERNO+0,0        UNKNOWN ERROR                                
         MVI   PCCLERNO+1,2        FORCE IT TO INVALID ERROR                    
         B     CKMADE45                                                         
*                                                                               
         PACK  DUB,11(4,R4)                                                     
         CVB   RF,DUB                                                           
         STH   RF,PCCLERNO                                                      
*                                                                               
CKMADE45 DS    0H                                                               
*                                                                               
*        CHECK FOR ADDITIONAL CHARGES UPLOAD                                    
*                                                                               
         L     R5,ATHISTMP         START OF PBU RECORD                          
         LA    R5,2(R5)            BUMP PAST TOTAL RECORD LENGTH                
*                                                                               
         SR    RF,RF                                                            
*                                                                               
CKMADE51 DS    0H                                                               
*                                                                               
         ICM   RF,3,0(R5)          BUMP TO NEXT PBU RECORD                      
         AR    R5,RF                                                            
         USING PACHD,R5            ESTABLISH AS ADDITIOAL CHARGE RECORD         
*                                                                               
         CLC   PACHTYPE,=C'EIN*'   DONE IF END OF INSERTION                     
         BE    CKMADE55                                                         
*                                                                               
         CLC   PACHTYPE,=C'ACH*'   ADDITIONAL CHARGE FOUND                      
         BE    CKMADE52                                                         
*                                                                               
         B     CKMADE51                                                         
*                                                                               
CKMADE52 DS    0H                                                               
*                                                                               
         MVC   BUYTR1(2),=C'RC'    RECALL ADDITIONAL CHARGE DATA                
*                                                                               
         FOUT  BUYTR1H                                                          
*                                                                               
         MVI   BUYTR1H+5,1         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'R'                                                     
*                                                                               
*        RECALL ADDITIONAL CHARGE SCREEN                                        
*                                                                               
         MVI   BYTE,X'EC'          X'EC' IS ADDITIONAL CHARGE SCREEN            
         BRAS  RE,MYOVRLY          GET ADDITIONAL CHARGE SCREEN AND PGM         
*                                                                               
         BRAS  RE,FILSCR4          FILL IN ADDITIONAL CHARGE DATA               
*                                                                               
         MVC   BUYTR1(2),=C'CC'    CHANGE ADDITIONAL CHARGE DATA                
*                                                                               
         FOUT  BUYTR1H                                                          
*                                                                               
         MVI   BUYTR1H+5,1         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
         MVI   LASTACT,C'C'                                                     
*                                                                               
         XC    DOUBLE,DOUBLE       CLEAR SAVED DATE                             
         MVI   ERRAREA,0           INIT MESSAGE NUMBER                          
         MVI   BYTE,X'EC'          X'EF' IS CUSTCOL SCREEN                      
         BRAS  RE,MYOVRLY          GET ADDITIONAL CHARGE SCREEN AND PGM         
*                                                                               
*        CHECK FOR ERRORS                                                       
*                                                                               
         CLI   ERRAREA,C'D'        ACTION COMPLETED                             
         BE    CKMADE55                                                         
         CLI   ERRAREA,C'K'        ACTION COMPLETED                             
         BE    CKMADE55                                                         
         CLI   ERRAREA,0           OR NO ERROR                                  
         BE    CKMADE55                                                         
*                                                                               
         L     R4,ERRAREA                                                       
*                                                                               
         CLC   8(2,R4),=C'EP'                                                   
         BE    *+16                                                             
         MVI   PACHERNO+0,0        UNKNOWN ERROR                                
         MVI   PACHERNO+1,2        FORCE IT TO INVALID ERROR                    
         B     CKMADE55                                                         
*                                                                               
         SR    R0,R0               INIT LINE COUNTER                            
*                                                                               
         CLI   HALF2,0             IF NO LINE NUMBER GIVEN                      
         BNE   *+10                                                             
         MVC   HALF2,=X'0101'         DEFAULT TO FIRST ACH                      
*                                                                               
         L     R5,ATHISTMP         START OF PBU RECORD                          
         LA    R5,2(R5)            BUMP PAST TOTAL RECORD LENGTH                
*                                                                               
         USING PACHD,R5            ESTABLISH PBU RECORD AS ADD CHG              
*                                                                               
CKACHELP DS    0H                                                               
*                                                                               
         CLC   =C'EIN*',PACHTYPE   DONE AT END OF INSERTION                     
         BE    CKACHEDN                                                         
*                                                                               
         CLC   =C'ACH*',PACHTYPE   SKIP IF NOT ADDITIONAL CHARGE RECORD         
         BNE   CKACHECN                                                         
*                                                                               
         AHI   R0,1                BUMP LINE COUNTER                            
*                                                                               
         CLM   R0,1,HALF2          DONE IF CORRECT LINE FOUND                   
         BE    CKACHEFD                                                         
*                                                                               
CKACHECN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PACHLEN        GET RECORD LENGTH                            
         LA    R5,0(RF,R5)         BUMP TO NEXT PBU FIELD                       
         B     CKACHELP                                                         
*                                                                               
CKACHEFD DS    0H                  FOUND RECORD WITH ERROR                      
*                                                                               
         PACK  DUB,11(4,R4)                                                     
         CVB   RF,DUB                                                           
         STH   RF,PACHERNO                                                      
*                                                                               
         MVC   PACHERF,HALF2+1     SET ERROR FIELD NUMBER                       
*                                                                               
CKACHEDN DS    0H                                                               
*                                                                               
CKMADE55 DS    0H                                                               
*                                                                               
         CLI   SVSCRN,X'EC'        IF WE HAD ADDITIONAL CHARGES                 
         BE    *+8                                                              
         CLI   SVSCRN,X'EF'        OR CUSTOM COLUMNS                            
         BNE   CKMADEX                                                          
*                                                                               
* MUST RECALL IN BASE SCREEN                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D90411FF'                                           
         GOTO1 VCALLOV,DMCB,64(RA)                                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SVSCRN,0            CLEAR SAVED SCREEN NUMBER                    
         LA    RF,REC              RESET I/O AREA POINTER                       
         ST    RF,AREC                                                          
*                                                                               
CKMADEX  B     BUYGLB5             GO DO NEXT INSERTION                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
BUY14N   CLI   ERRAREA,C'D'                                                     
         BE    BUY14RZ                                                          
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
BUY14RZ  DS    0H                                                               
         CLI   BUYTR1,C'R'                                                      
         BE    BUY16                                                            
         CLI   BUYPB,C'L'          TEST LIST BUY                                
         BE    BUY16               YES                                          
         CLI   WARN,0                                                           
         BNE   BUY15                                                            
         CLI   TRCODE,C'R'                                                      
         BNE   BUY16                                                            
BUY15    DS    0H                                                               
         LA    R2,BUYTR1H                                                       
         LH    RE,SVNTRNS          GET FIELDS/BUY LINE                          
         BCTR  RE,R0                                                            
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,6                                                           
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
BUY16    DS    0H                                                               
         LA    R2,BUYPBH                                                        
         FOUT  BUYMSGH,=C' ** ACTION COMPLETED **'                              
         B     DONE                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
EXT      XIT                                                                    
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FILL IN FIELDS ON SCREEN FROM GLOBBER AREA                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILHEAD  NTR1                                                                   
*                                                                               
* CLEAR ALL UNPROTECTED FIELDS ON SCREEN                                        
*                                                                               
         LA    R2,BUYMDH           POINT TO FIRST FIELD ON SCREEN               
         SR    RF,RF                                                            
*                                                                               
FCLCLRLP DS    0H                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    FCLCLRCN                                                         
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    FCLCLRDN            DONE IF SCREEN END REACHED                   
*                                                                               
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         SKIP IF NOT EXTENDED HEADER                  
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         NI    6(R2),X'BF'         UNSET CURSOR                                 
*                                                                               
FCLCLRCN DS    0H                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
         B     FCLCLRLP                                                         
*                                                                               
FCLCLRDN DS    0H                                                               
         MVI   MADSW,C'Y'          INDICATE CALLED BY $MAD                      
*                                                                               
         L     RF,VGLOBBER         POINT TO GLOBBER                             
*                                                                               
* FILL IN HEADLINE FIELDS                                                       
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',BUYMDH,,GLVPRMD    MEDIA                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,BUYCLH,,GLVPRCLT           CLIENT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,BUYPRH,,GLVPRPRD           PRODUCT                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,BUYESH,,GLVPREST           ESTIMATE                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FCLCLX   FOUT  BUYMDH              FORCE RE-TRANSMISSION OF MEDIA               
         FOUT  BUYCLH              FORCE RE-TRANSMISSION OF CLIENT              
         FOUT  BUYPRH              FORCE RE-TRANSMISSION OF PRODUCT             
         FOUT  BUYESH              FORCE RE-TRANSMISSION OF ESTIMATE            
*                                                                               
         LA    R5,SAVUHDR          BUYER FROM SAVED UPLOAD HEADER               
         USING PHDRD,R5                                                         
         LA    R2,BUYNMH                                                        
         LA    R4,PHDRBUYR                                                      
         BRAS  RE,SETIN                                                         
         MVI   11(R2),0            MUST ADJUST BUYER SINCE FLD IS 4             
         ZIC   R5,5(R2)            BUT DATA IS 3 CHARACTERS LONG                
         BCTR  R5,0                                                             
         STC   R5,5(R2)                                                         
*                                                                               
FILHEADX J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* THIS ROUTINE FINDS THE NEXT DATA CHUNK FROM THE TEMPORARY FILE                
* BUFFER.  THE TEMPORARY FILE USES TEMPSTR TO STORE ITS RECORDS, AND            
* VTIA AS ITS BUFFER.  THE ROUTINE MOVES THROUGH THE BUFFER TO                  
* READ EACH ELEMENT, AND EACH TIME THE END IS REACHED, IT READS IN THE          
* NEXT TEMPSTR RECORD.  IF THE ROUTINE IS SUCCESSFUL, IT RETURNS 'YES',         
* ELSE IT RETURNS 'NO'.                                                         
*                                                                               
* RETURNS:     DNEXTTMP WILL BE SET TO ADDRESS OF NEXT DATA                     
*              ATHISTMP HAS ADDRESS OF THIS DATA                                
*                                      0 - END OF TEMP FILE                     
*       FOR FIRST TIME IN SET CURRTWA TO 2                                      
*       AND SET 'XFFFF' INTO VTIA                                               
*       THIS WILL CAUSE GETTMP TO BUMP CURRTWA AND START WITH 3                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VGETTMP  NTR1                                                                   
         L     R4,DNEXTTMP         R4 = A(NEXT DATA IN TMP BUFFER)              
*                                                                               
         LA    R1,2(R4)            IF LENGTH FITS IN TMP BUFFER                 
         CH    R1,LENTWA                                                        
         BNH   GT10                THEN GET THE LENGTH AND DATA                 
*                                                                               
GT5      L     RF,CURRTWA          ELSE BUMP TO NEXT TEMPSTR NUMBER             
         LA    RF,1(RF)                                                         
         ST    RF,CURRTWA                                                       
*                                                                               
         CLC   CURRTWA,=F'5'       IF TEMPSTR RECORD 5                          
         BE    GT5                 THEN SKIP TO NEXT                            
*                                                                               
         CLC   CURRTWA,=F'11'      IF OVERFLOW                                  
         BNL   GTOVER              THEN ERROR                                   
*                                                                               
* FILL TMPBUF W/ NEXT TEMPSTR REC                                               
*                                                                               
         GOTO1 VREADTWA,CMDMCB,VTIA,CURRTWA                                     
         BNE   GTNO                                                             
*                                                                               
         SR    R4,R4               RESET DNEXTTMP                               
         ST    R4,DNEXTTMP                                                      
*                                                                               
GT10     A     R4,VTIA             GET LENGTH FROM TEMP BUF                     
*                                                                               
         CLC   =X'FFFF',0(R4)                                                   
         BNE   GT20                END OF BLOCK                                 
*                                                                               
         CLC   CURRTWA,=F'2'       MEANS FIRST TIME IN                          
         BE    GT5                                                              
*                                                                               
* MUST WRITE BACK LAST TWA                                                      
*                                                                               
         GOTO1 VWRTTWA,CMDMCB,VTIA,CURRTWA                                      
         B     GT5                 GO GET NEXT BUFFER                           
*                                                                               
GT20     ZICM  R3,0(R4),(3)                                                     
         BNZ   *+12                IF LENGTH IS ZERO                            
         MVI   EOTFLAG,C'Y'        THEN SET END OF TEMP FLAG                    
         B     GTNO                AND RETURN 'NO'                              
*                                                                               
         LA    R4,2(R4)            R4 = A(DATA STORED IN TMP BUFFER)            
         LR    R5,R3               R5 = LENGTH OF DATA                          
*                                                                               
         L     R1,DNEXTTMP         IF DATA FITS IN TIA                          
         LA    R1,2(R1,R3)                                                      
         CH    R1,LENTWA                                                        
         BNH   GT30                THEN COPY DATA TO ADDRESS GIVEN              
         DC    H'0'                MUST FIT                                     
*                                                                               
GT30     DS    0H                                                               
         ST    R4,ATHISTMP         SAVE ADDRESS OF THIS OBJECT                  
*                                                                               
         L     R1,ATHISTMP         BACK-UP FOR LENGTH                           
         AHI   R1,-2                                                            
         ST    R1,ATHISTMP                                                      
*                                                                               
         AR    R4,R5               SAVE D(NEXT ELEMENT IN TMP BUFFER)           
         S     R4,VTIA                                                          
         ST    R4,DNEXTTMP                                                      
*                                                                               
         B     GTYES               RETURN 'YES'                                 
*                                                                               
GTOVER   DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
GTNO     CLI   EOTFLAG,C'Y'                                                     
         BNE   NO                                                               
*                                                                               
* MUST WRITE BACK LAST PAGE                                                     
*                                                                               
         GOTO1 VWRTTWA,CMDMCB,VTIA,CURRTWA                                      
         B     NO                                                               
*                                                                               
GTYES    DS    0H                                                               
         B     YES                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE TWA RECORD NUMBER SPECIFIED BY PARAMETER 2.                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VREADTWA NTR1                                                                   
*                                                                               
* R2 = A(BLOCK OF MEMORY), R3 = TWA RECORD NUMBER                               
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
         SLL   R3,32-8             SHIFT TWA NUM TO R3 HIGH ORDER BYTE          
         L     RF,VTWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
* SET DATAMGR PARM 6 TO READ THE NEW LARGE TEMPSTR RECORD SIZE                  
*                                                                               
         MVC   CMDMCB+20(2),=C'L='                                              
         MVC   CMDMCB+22(2),LENTWA                                              
*                                                                               
* CALL DATAMGR TO READ TEMPSTR RECORD                                           
*                                                                               
         GOTO1 VDATAMGR,CMDMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R3),(R2)             
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                SERIOUS BUG - NEED CORE DUMP                 
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* WRITE FROM THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 INTO THE TWA           
* RECORD NUMBER SPECIFIED BY PARAMETER 2.                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VWRTTWA  NTR1                                                                   
*                                                                               
* R2 = A(BLOCK OF MEMORY), R3 = TWA RECORD NUMBER                               
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
         SLL   R3,32-8             SHIFT TWA NUM TO R3 HIGH ORDER BYTE          
         L     RF,VTWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
* CALL DATAMGR TO WRITE TEMPSTR RECORD                                          
*                                                                               
         GOTO1 VDATAMGR,CMDMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R3),(R2),0            
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                SERIOUS BUG - NEED CORE DUMP                 
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO FIND NEXT NON-NULL TR FIELD                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTR    NTR   BASERB                                                           
*                                                                               
         L     R8,BASER8                                                        
*                                                                               
         L     R2,TRADDR           GET LAST TR FIELD ADDRESS                    
         LTR   R2,R2                                                            
         BNZ   NXTTR7                                                           
*                                                                               
         LA    R2,BUYTR1H          POINT TO FIRST TR FIELD                      
         LA    RE,SVLST            SET LIST POINTER                             
         ST    RE,LSTPTR                                                        
NXTTR2   LA    R3,INVERR                                                        
         CLI   0(R2),0             TEST LAST FIELD                              
         BZ    NXTTRLST                                                         
         CLI   5(R2),0             TEST NO TR FIELD THIS LINE                   
         BNE   NXTTR6                                                           
         C     R2,LASTFLD          COMPARE TO A(LAST FIELD ENTERED)             
         BNH   ERROR                                                            
         B     NXTTRLST                                                         
*                                                                               
NXTTR6   CLI   8(R2),C'*'          TEST NOP                                     
         BNE   NXTTR8                                                           
NXTTR7   LH    RE,SVNTRNS          SKIP TO NEXT TR FIELD                        
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
         L     RE,LSTPTR                                                        
         LA    RE,6(RE)                                                         
         ST    RE,LSTPTR                                                        
         CLI   0(RE),X'FF'                                                      
         BZ    NXTTRLST                                                         
         B     NXTTR2                                                           
*                                                                               
NXTTR8   LA    R3,INVERR                                                        
         CLI   5(R2),1                                                          
         BNE   NXTTR10                                                          
         CLI   8(R2),C'B'                                                       
         BE    NXTTRX                                                           
         CLI   8(R2),C'C'                                                       
         BE    NXTTRX                                                           
         B     ERROR                                                            
NXTTR10  CLC   =C'DL',8(R2)                                                     
         BE    NXTTRX                                                           
         CLC   =C'CT',8(R2)        TEARSHEET  CHANGE?   (T41115)                
         BE    NXTTRX1                                                          
*                                                                               
         CLC   =C'CU',8(R2)        CUSTOM COLS  CHANGE?   (T41120)              
         BNE   NXTTR11                                                          
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
         MVI   BYTE,X'EF'          X'EF' IS CUSTOM COLUMN SCREEN                
         B     NXTTR13                                                          
*                                                                               
NXTTR11  CLC   =C'CC',8(R2)        ADDTNL CHRGS CHANGE? (T41116)                
         BNE   NXTTR15                                                          
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
         MVI   BYTE,X'EC'          X'EC' IS ADDTNL CHG SCREEN                   
NXTTR13  BRAS  RE,MYOVRLY          GET ADD'L CHG OR CCOL SCREEN AND PGM         
         B     NXTTRX20                                                         
*                                                                               
NXTTR15  DS    0H                  FOR FUTURE USES                              
*                                                                               
         B     ERROR                                                            
*                                                                               
NXTTRX   DS    0H                                                               
         CLI   T411FFD+1,C'*'      DDS TERMINAL - SKIP                          
         BE    NXTTRX0                                                          
*                                                                               
         CLI   MADSW,C'Y'          SEE IF UPLOADING FROM $MAD                   
         BE    NXTTRX0             YES - IGNORE LIMIT ACCESS                    
*                                                                               
         MVI   BYTE4,X'01'                                                      
         NC    BYTE4,T411FFD+12    CHK FOR ACTION LIMIT ACCESS                  
         BZ    NXTTRX0                                                          
*                                  B,C,DL NOT ALLOWED                           
         LA    R3,FACCERR                                                       
         B     ERROR                                                            
*                                                                               
NXTTRX0  DS    0H                                                               
         CLI   8(R2),C'B'          SEE IF BUYING                                
         BNE   NXTTRX1                                                          
         TM    SVCLPROF+30,X'02'   SEE IF CLIENT IS FROZEN                      
         BNO   NXTTRX1                                                          
         TM    SVCLPROF+30,X'10'   SEE IF FROZEN WITH DATE                      
         BO    NXTTRX1             YES - CHECK AT EDTINS4Z                      
         LA    R3,FRZERR                                                        
         B     ERROR                                                            
*                                                                               
* NOTE: PCLTSTAT WAS SAVED IN SVCLPROF+30 IN PPBUY01                            
*                                                                               
NXTTRX1  DS    0H                                                               
NXTTRX2  ST    R2,TRADDR           PASS TR FIELD ADDRESS                        
*                                                                               
         CLI   SVSCRN,X'FE'        SEE IF WSJ LIST BUYING                       
         BE    NXTTRX3                                                          
         CLI   SVSCRN,X'F8'        SEE IF LIST BUYING                           
         BE    NXTTRX3                                                          
         CLI   SVSCRN,X'F9'        SEE IF LIST BUYING - ZZZ                     
         BE    NXTTRX3                                                          
*                                                                               
         CLI   SVSCRN,X'E8'        SEE IF MAG LIST BUYING                       
         BE    NXTTRX3                                                          
         CLI   SVSCRN,X'E9'        SEE IF MAG LIST BUYING - ZZZ                 
         BE    NXTTRX3                                                          
         CLI   SVSCRN,X'EA'        SEE IF INTERACTIVE LIST BUYING               
         BE    NXTTRX3                                                          
         CLI   SVSCRN,X'EB'        SEE IF INT LIST BUYING - ZZZ                 
         BE    NXTTRX3                                                          
         B     NXTTRX20                                                         
*                                                                               
* PUB ADDRESS NEEDED NOW FOR SPACE EDIT FD CALCULATION                          
*                                                                               
NXTTRX3  L     RE,LSTPTR                                                        
         MVC   BPUB,0(RE)                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(1),BUYMD                                                     
         MVC   KEY+1(6),BPUB                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BRAS  RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    NXTTRX10                                                         
         CLI   SVAGPROF+16,C'0'    TEST DEFAULT TO SRDS                         
         BE    NXTTRX5             NO                                           
         MVC   KEYSAVE+7(2),=C'ZZ' TEST FOUND DEFAULT                           
         CLC   KEYSAVE(25),KEY                                                  
         BE    NXTTRX10                                                         
*                                                                               
NXTTRX5  DS    0H                                                               
         MVC   KEY,KEYSAVE         READ DEFAULT                                 
         BRAS  RE,READPUB                                                       
NXTTRX10 MVC   SVPUBDA,KEY+27      SAVE DISK ADDRESS                            
*                                                                               
NXTTRX20 MVI   BFREQ,0             SET TO REQUIRE MONTH DAY                     
         B     NXTTRXX                                                          
NXTTRLST XC    TRADDR,TRADDR                                                    
NXTTRXX  J     EXT                                                              
         EJECT                                                                  
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         JNE   *-18                                                             
NEXTELX  LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO FORMAT INSERTION DATE/SUBLINE, R1 POINTS TO A(REC)              
*                                                                               
FMTINS   NTR   BASERB                                                           
         L     R8,BASER8                                                        
*                                                                               
         L     R2,TRADDR                                                        
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         L     R6,0(R1)            GET REC ADDRESS                              
*                                                                               
         XC    WORK(20),WORK                                                    
         LA    R4,WORK                                                          
         CLI   PBDBFD-NEWREC(R6),0                                              
         BE    FMTINS2                                                          
         MVC   0(1,R4),PBDBFD-NEWREC(R6)                                        
         TM    SVESPROF+29,X'40'                                                
         BZ    *+8                                                              
         MVI   0(R4),C'S'          INDICATE STEWARDSHIP INSERTION               
*                                                                               
         LA    R4,1(R4)                                                         
FMTINS2  GOTO1 VDATCON,DMCB,(3,16(R6)),(7,0(R4))                                
         LA    R1,5(R4)            SET NEXT OUTPUT ADDRESS                      
         LA    R7,PBDFREQ-NEWREC(R6)                                            
         CLI   0(R7),C'M'                                                       
         BNE   *+14                                                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         XC    0(2,R1),0(R1)                                                    
         LA    R0,WORK                                                          
         SR    R0,R1                                                            
         LPR   R0,R0                                                            
         STC   R0,5(R2)            SET AS INPUT LENGTH FOR LIST BUYS            
         CLI   24(R6),1            TEST SUBLINE TO DISPLAY                      
         BE    FMTINSX                                                          
         MVI   0(R1),C'-'                                                       
         IC    R0,24(R6)                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    FMTINS5                                                          
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  2(1,R1),DUB+7(1)                                                 
         OI    2(R1),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R7,DUB                                                           
         LA    R7,ALPHTAB(R7)                                                   
         MVC   1(1,R1),0(R7)                                                    
         B     FMTINS9                                                          
*                                                                               
FMTINS5  OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R1),DUB                                                      
         CLI   1(R1),C'0'                                                       
         BNE   *+10                                                             
         MVC   1(2,R1),2(R1)                                                    
*                                                                               
FMTINS9  MVC   SVINSDT,16(R6)      SAVE INSERTION DATE                          
*                                                                               
FMTINSX  DS    0H                                                               
         MVC   8(8,R2),WORK                                                     
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
         J     EXT                                                              
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO FORMAT TR FIELD CODE, R1 POINTS TO A(REC)                       
*                                                                               
FMTTR    NTR   BASERB                                                           
         L     R8,BASER8                                                        
*                                                                               
         L     R2,TRADDR                                                        
         MVI   BPSW,0                                                           
         MVC   8(2,R2),=C'* '                                                   
         FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R3,0(R1)            GET RECORD ADDRESS                           
         LA    R4,33(R3)                                                        
FMTTR2   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FMTTR8                                                           
         CLI   0(R4),X'25'         PAY ELEM                                     
         BNE   FMTTR4                                                           
         OC    2(3,R4),2(R4)                                                    
         BZ    FMTTR2                                                           
         OI    BPSW,X'40'          PAID                                         
         B     FMTTR6                                                           
FMTTR4   CLI   0(R4),X'26'         TEST BILL ELEM                               
         BNE   FMTTR5                                                           
         OC    5(3,R4),5(R4)       TEST BILLED                                  
         BZ    FMTTR2                                                           
         OI    BPSW,X'80'                                                       
         B     FMTTR6                                                           
*                                                                               
FMTTR5   CLI   0(R4),X'28'         OPEN BILLING                                 
         BNE   FMTTR2                                                           
         OC    5(3,R4),5(R4)       TEST BILLED                                  
         BZ    FMTTR2                                                           
         OI    BPSW,X'80'                                                       
         B     FMTTR6                                                           
*                                                                               
FMTTR6   MVI   9(R2),C'*'          SET TR FIELD TO ** IF PAID/BILLED            
*                                                                               
FMTTR8   DS    0H                                                               
         TM    27(R3),X'80'        TEST DELETE                                  
         BZ    *+8                                                              
         MVI   8(R2),C'D'                                                       
*                                                                               
FMTTRX   J     EXT                                                              
         EJECT                                                                  
*                                                                               
FMTPR    NTR   BASERB                                                           
         L     R8,BASER8                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R6,8(R2)                                                         
         XC    0(11,R6),0(R6)                                                   
         CLI   PBDCL,0                                                          
         BE    FMTPR2                                                           
         MVC   0(1,R6),PBDCL                                                    
         MVI   1(R6),C'C'                                                       
         MVI   2(R6),C'/'                                                       
         LA    R6,3(R6)                                                         
         B     FMTPR3                                                           
*                                                                               
FMTPR2   DS    0H                                                               
         CP    PBDPRCOS,=P'0'                                                   
         BE    FMTPR4                                                           
FMTPR3   DS    0H                  SET R1 TO PREMIUM COST                       
         ZAP   DUB,PBDPRCOS                                                     
         CVB   R1,DUB                                                           
         CLI   PBDPCTYP,C'N'       NET INPUT SO DISPLAY AS NET                  
         BNE   FMTPR3A                                                          
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
*                                                                               
FMTPR3A  DS    0H                                                               
         CLI   PBDPRIN,C' '        TEST DEFAULT IND                             
         BNH   *+14                                                             
         MVC   0(1,R6),PBDPRIN                                                  
         LA    R6,1(R6)                                                         
         CLI   PBDPCTYP,C' '       TEST PREMIUM COST TYPE                       
         BNH   *+14                                                             
         MVC   0(1,R6),PBDPCTYP                                                 
         LA    R6,1(R6)                                                         
         CLI   PBDCL,0             CHK FOR COLOR                                
         BE    FMTPR3C                                                          
*                                                                               
* NOTE BOTH OF THE INDICATORS BELOW SHOULD NOT                                  
* BE PRESENT AT THE SAME TIME                                                   
* THIS SHOULD HAVE BEEN CAUGHT IN PPBUY12 - EDTPREM                             
* -IF THEY ARE SCREEN WILL BE CLOBBERED                                         
*                                                                               
         CLI   PBDPRIN,C' '        CHK FOR PR RATE IND                          
         BH    FMTPR2A             IF SPACE I CAN DISPLAY 8 DIGITS              
         CLI   PBDPCTYP,C' '       CHK FOR PR COST TYPE                         
         BNH   FMTPR3B             IF SPACE I CAN DISPLAY 8 DIGITS              
*                                                                               
FMTPR2A  DS    0H                                                               
         C     R1,=F'-99999'       CHECK FOR NEGATIVE > -999.99                 
         BL    FMTPR2B                                                          
         C     R1,=F'1000000'      SEE IF OVER 10,000.00                        
         BL    FMTPR2H                                                          
*                                                                               
FMTPR2B  CVD   R1,DUB              SEE IF PENNY CAN BE DROPPED                  
         C     R1,=F'-10000000'    CK FOR -100,000.00                           
         BNH   FMTPR2F                                                          
         C     R1,=F'10000000'     CK FOR 100,000.00                            
         BL    FMTPR2G                                                          
*                                                                               
FMTPR2F  DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)          NO DECIMAL                                   
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R6)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR2G  DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(6)          ONE DECIMAL                                  
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R6)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR2H  CLI   PBDCL,0                                                          
         BE    FMTPR2M                                                          
         CLI   8+3(R2),C'0'                                                     
         BH    FMTPR2M                                                          
         C     R1,=F'999999'       GREATER THAN 9,999.99?                       
         BNH   FMTPR2M                                                          
         CVD   R1,DUB                                                           
         DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(6)                                                       
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R6)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR2M  EDITR (R1),(7,0(R6)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR3B  DS    0H                                                               
         C     R1,=F'-999999'      SEE IF NEGATIVE > -9,999.99                  
         BL    FMTPR3B2                                                         
         C     R1,=F'10000000'     SEE IF OVER 100,000.00                       
         BL    FMTPR3B5                                                         
*                                                                               
FMTPR3B2 DS    0H                  SEE IF PENNY CAN BE DROPPED                  
         CVD   R1,DUB                                                           
         C     R1,=F'-10000000'    CK FOR -100,000.00                           
         BNH   FMTPR3B3                                                         
         C     R1,=F'100000000'    CK FOR 1,000,000.00                          
         BL    FMTPR3B4                                                         
*                                                                               
FMTPR3B3 DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)          NO DECIMAL                                   
         CVB   R1,DUB                                                           
         EDITR (R1),(8,0(R6)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR3B4 DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(6)          ONE DECIMAL                                  
         CVB   R1,DUB                                                           
         EDITR (R1),(8,0(R6)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR3B5 EDITR (R1),(8,0(R6)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTPR4                                                           
*                                                                               
FMTPR3C  EDITR (R1),(10,0(R6)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,      +        
               IZERO=Y                                                          
*                                                                               
FMTPR4   DS    0H                                                               
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PUT DATA (R4) INTO FIELDS (R2 POINTS TO HDR), CHK FOR NUMERIC INPUT           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETIN    DS    0H                                                               
         LR    RF,R4               SAVE ADDRESS OF DATA                         
         ZIC   R1,0(R2)            LENGTH OF HEADER + FIELD                     
         AHI   R1,-9               R1 = LENGTH OF FIELD + ONE BYTE              
         AR    R4,R1               SETS ME TO END OF MAXIMUM INTO DATA          
         LA    R1,1(R1)            R1 SHOULD NOW BE LENGTH OF SCR FLD           
SETIN5   CLI   0(R4),C' '          SCAN BACKWARDS FOR NON SPACE                 
         BH    SETIN20                                                          
         BCTR  R4,0                                                             
         BCT   R1,SETIN5                                                        
         B     SETINX              NO INPUT                                     
*                                                                               
SETIN20  DS    0H                                                               
         STC   R1,5(R2)            SET INPUT LENGTH INTO HEADER                 
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
         FOUT  (R2)                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RF)       MOVE DATA                                    
*                                                                               
         LA    R1,1(R1)            MUST READJUST R1                             
*                                                                               
* NOTE: R4 NOW POINTS TO FIRST NON-BLANK CHAR WHEN SCANNING BACKWARDS           
*                                                                               
SETIN25  MVI   BYTE,0                                                           
         CLI   0(R4),C'0'          SEE IF NON-NUMERIC                           
         BL    SETIN30                                                          
         MVI   BYTE,X'08'                                                       
         BCTR  R4,0                                                             
         BCT   R1,SETIN25                                                       
         B     SETIN30                                                          
*                                                                               
* NOTE: BYTE WILL NOW BE SET TO X'08' ONLY IF ALL CHARS WERE NUMERIC            
*                                                                               
SETIN30  OC    4(1,R2),BYTE        MAY SET ON NUMERIC                           
*                                                                               
SETINX   BR    RE                  RETURN                                       
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITL    DS    0H                  SET UP TO CLEAR WORK SPACE                   
         LR    R4,RC                                                            
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BRAS  RE,CLEARWRK                                                      
         LM    R2,R5,0(R1)                                                      
         ST    R5,VTIA             USED FOR TEMPSTORE BUFFER                    
*                                                                               
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,REC                                                           
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
*                                                                               
         BR    RE                                                               
*                                                                               
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R5,250                                                           
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         AHI   R5,-250                                                          
         B     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUMPFLDS ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS                                                      
         BR    RE                                                               
*                                                                               
BUMPFLD  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ANY      CLI   5(R2),0                                                          
         JNE   ANY2                                                             
         LA    R3,1                                                             
         J     ERROR                                                            
*                                                                               
ANY2     TM    4(R2),X'10'         IS IT VALID NUMERIC                          
         BCR   8,RE                IF APPLICABLE                                
         LA    R3,3                                                             
         J     ERROR                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
DONE     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         CLI   MADSW,C'Y'          $MAD UPLOAD?                                 
         JE    CKMADERR                                                         
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ALPHTAB  DS    0H                                                               
         DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         DROP  RB,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        LOAD AND CALL OVERLAY FOR SCREEN                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETOVLY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVTRCODE,BUYTR1     SAVE TRANSACTION CODE                        
*                                                                               
         XC    DMCB+1(3),DMCB+1                                                 
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         CLC   =C'CU',SVTRCODE     IF CUSTCOL CHANGE                            
         BNE   GETOVLY0                                                         
*                                                                               
         TM    CHGIND5,PCHFXRTQ+PCHGTRKQ  SKIP IF NO STDCOL CHANGES             
         BZ    GETOVLY1                                                         
*                                                                               
*        UPDATE CHANGE ELEMENT FOR CHANGES                                      
*                                                                               
         XC    DMCB+1(3),DMCB+1                                                 
         MVI   DMCB,X'03'          LOAD OVERLAY 3                               
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
*                                                                               
         GOTOR (RF),DMCB,(RC),(RA),CHGELEMQ UPDATE CHGELEM                      
*                                                                               
         TM    REC+(PBDSTAT2-PBUYREC),X'20'  SKIP IF NOT ADDED BY IDESK         
         BNO   GETOVLY1                                                         
*                                                                               
         TM    CHGIND5,PCHGTRKQ    SKIP IF NOTHING TO TRACK                     
         BNO   GETOVLY1                                                         
*                                                                               
         GOTOR (RF),DMCB,(RC),(RA),TRKCC_Q   TRACK CUSTOM COLUMNS               
*                                                                               
         B     GETOVLYA                                                         
*                                                                               
GETOVLY0 DS    0H                                                               
*                                                                               
         XC    DMCB+1(3),DMCB+1                                                 
         MVI   DMCB,X'03'          LOAD OVERLAY 3                               
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
*                                                                               
GETOVLYA DS    0H                                                               
*                                                                               
         GOTOR (RF),DMCB,(RC),(RA),SENDMQQ   SEND MQ MESSAGE                    
*                                                                               
GETOVLY1 DS    0H                                                               
*                                                                               
         B     GETOVLYX                                                         
*                                                                               
GETOVLYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R1 = INPUT DATE TO BE USED FOR PBU'S DATES OVERRIDE BY PROFILE                
* R2 = FLD HEADER TO BE POPULATED                                               
* R4 = INPUT DATE IN FORMAT OF YYYYMMDD                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETDATE  NTR1  BASE=*,LABEL=*      SET DATE FIELD                               
*                                                                               
         USING PINSD,R5                                                         
*                                                                               
         CLI   PINSACTN,C'C'       CHANGE UPLOAD?                               
         BNE   SETD20                                                           
         CLI   BYPROF+11,C'Y'      OVERRIDE OS,SC,MC DATES?                     
         BNE   SETD20                                                           
         CLI   REC+33,X'20'                                                     
         BE    *+6                                                              
         DC    H'0'                BUY RECORD MUST PRESENT!                     
         LR    R7,R1                                                            
         OC    0(3,R7),0(R7)                                                    
         BZ    SETD_X                                                           
         GOTO1 VDATCON,DMCB,(3,0(R7)),(7,8(R2))                                 
         B     SETD40                                                           
*                                                                               
SETD20   CLI   0(R4),X'FF'         SEE IF THEY SENT BAD DATE                    
         BNE   SETD30                                                           
         MVC   8(3,R2),=C'XXX'                                                  
         MVI   5(R2),3                                                          
         B     SETD80                                                           
*                                                                               
SETD30   CLI   0(R4),C' '          SEE IF I HAVE A CLOSING DATE                 
         BE    SETD_X                                                           
         LA    R7,2(R4)            PAST YY, POINT TO YYMMDD                     
*                                                                               
         GOTO1 VDATCON,DMCB,(0,0(R7)),(7,8(R2))                                 
*                                                                               
SETD40   MVI   5(R2),5             SET INPUT LENGTH                             
SETD80   OI    1(R2),X'01'         SET ON MODIFIED                              
         FOUT  (R2)                                                             
*                                                                               
SETD_X   J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
OSSCMCDT NTR1  BASE=*,LABEL=*      PROCESS OS,SC,MC DATES                       
*                                                                               
         USING PINSD,R5                                                         
*                                                                               
         LA    R2,BUYTR1H                                                       
*                                                                               
         CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    OSSCMC10                                                         
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    OSSCMC10                                                         
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    OSSCMC10                                                         
         CLI   BUYMD,C'V'          NATIONAL VIDEO (NVIDEO)?                     
         JE    OSSCMC10                                                         
         CLI   BUYMD,C'W'          LOCAL VIDEO (LVIDEO)?                        
         JE    OSSCMC10                                                         
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    OSSCMC10                                                         
*                                                                               
         J     OSSCMC20                                                         
*                                                                               
OSSCMC10 LHI   RF,5                POINT TO SPACE CLOSING DATE FLD              
OSSCMC12 BRAS  RE,FBUMPFD                                                       
         BCT   RF,OSSCMC12                                                      
         LA    R4,PINSCLOS                                                      
         LA    R1,REC+(PBDCDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
         BRAS  RE,FBUMPFD          POINT TO MAT CLOSING DATE FLD                
         LA    R4,PINSMATC                                                      
         LA    R1,REC+(PBDMDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
         B     OSSCMC_X                                                         
*                                                                               
OSSCMC20 CLI   BUYMD,C'M'                                                       
         BE    OSSCMC22                                                         
         CLI   BUYMD,C'S'                                                       
         BE    OSSCMC22                                                         
         CLI   BUYMD,C'T'                                                       
         BNE   OSSCMC40                                                         
OSSCMC22 LHI   RF,5                POINT TO SPACE CLOSING DATE FLD              
OSSCMC24 BRAS  RE,FBUMPFD                                                       
         BCT   RF,OSSCMC24                                                      
         LA    R4,PINSCLOS                                                      
         LA    R1,REC+(PBDCDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
         BRAS  RE,FBUMPFD          POINT TO ON-SALE DATE FLD                    
         LA    R4,PINSSALE                                                      
         LA    R1,REC+(PBDSDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
         BRAS  RE,FBUMPFD          POINT TO MAT CLOSING DATE FLD                
         LA    R4,PINSMATC                                                      
         LA    R1,REC+(PBDMDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
         B     OSSCMC_X                                                         
*                                                                               
OSSCMC40 CLI   BUYMD,C'N'                                                       
         BNE   OSSCMC60                                                         
         LHI   RF,6                POINT TO MAT CLOSING DATE FLD                
OSSCMC44 BRAS  RE,FBUMPFD                                                       
         BCT   RF,OSSCMC44                                                      
         LA    R4,PINSMATC                                                      
         LA    R1,REC+(PBDMDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
         B     OSSCMC_X                                                         
*                                                                               
OSSCMC60 CLI   BUYMD,C'O'                                                       
         BNE   OSSCMC80                                                         
         LHI   RF,7                POINT TO SPACE CLOSING DATE FLD              
         B     OSSCMC12            REST IS SAME AS MED I                        
*                                                                               
OSSCMC80 DC    H'0'                MEDIA IS NOT DEFINED                         
*                                                                               
OSSCMC_X J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCORES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),FULL      CORE-RESIDENT PHASE TO BE CALLED             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                RETURN ADDRESS IN DMCB                       
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKDDLINK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',X,GLVXLENQ,GLVXCTL                        
         CLI   DMCB+8,GLEGNF                                                    
         BE    CKDDLKER                                                         
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   X(12),=C'PRILINPRIBUY'                                           
         BNE   CKDDLKER                                                         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'17'                                                       
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         XC    DMCB+1(3),DMCB+1                                                 
         MVI   DMCB,X'03'          LOAD OVERLAY 3                               
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
*                                                                               
         GOTOR (RF),DMCB,(RC),(RA),SENDMQQ   SEND MQ MESSAGE                    
*                                                                               
         B     *+8                 USE PATCH 07000700 TO TRAP BUY PRG           
         B     CKDDLKX             WILL NOT GO BACK TO DDLINK!                  
*                                                                               
* RETURN TO CALLER VIA GLOBBER XCTL                                             
*                                                                               
         LA    R1,X                                                             
         USING GLVXFRSY,R1                                                      
         MVI   GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         DROP  R1                                                               
         GOTOR VGLOBBER,DMCB,=C'PUTD',X,GLVXLENQ,GLVXCTL                        
         CLI   DMCB+8,0                                                         
         JE    CKDDLKX                                                          
         DC    H'0'                GLOBBER ERROR ENCOUNTERED                    
*                                                                               
CKDDLKX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKDDLKER LTR   RB,RB               NOT EQUAL (NO CALLS FROM DDLINK)             
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   CALL ADDITIONAL CHARGES OR CUSTOM COLUMNS SCREENS AND PROGRAMS    *         
*NTRY    BYTE=X'EC'   ADDITIONAL CHARGES CALL                         *         
*             X'EF'   CUSTOM COLUMNS CALL                             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MYOVRLY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVTRCODE,BUYTR1     SAVE TRANSACTION CODE                        
*                                                                               
         CLI   BYTE,X'EC'          ADDTNL CHRGS BEING CALLED?                   
         BNE   MYOVR05             MUST BE CUSTOM COLUMN CALL                   
*                                                                               
         CLI   SVSCRN,X'EC'        ADDTNL CHRGS LOWER SCREEN?                   
         BE    MYOVR30                                                          
*                                                                               
MYOVR05  DS    0H                                                               
*                                                                               
         CLI   BYTE,X'EF'          CUSTOM COLUMNS BEING CALLED?                 
         BNE   MYOVR10             MUST BE ADDTNL CHRGS CALL                    
*                                                                               
         CLI   SVSCRN,X'EF'        CUSTOM COLUMNS LOWER SCREEN?                 
         BE    MYOVR30                                                          
*                                                                               
MYOVR10  XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90411'                                             
         MVC   DMCB+7(L'BYTE),BYTE                                              
*                                                                               
*        CALCULATE LENGTH OF INSERTION DATE -                                   
*              LENGTH FOR LINE NUMBER GETS DROPPED SOMEWHERE                    
*                                                                               
         LA    RF,BUYDT1+L'BUYDT1-1 END OF FIELD                                
         LA    R0,L'BUYDT1         FIELD MAX LENGTH                             
*                                                                               
         CLI   0(RF),C' '          FIND LAST NON-SPACE                          
         BH    *+12                                                             
         SHI   RF,1                BACKUP A POSITION                            
         BCT   R0,*-12                                                          
*                                                                               
         STC   R0,BUYDT1H+5        SET INPUT LENGTH                             
*                                                                               
         MVC   BYTE4,BUYDT1H+5     SAVE DATE INPUT LENGTH (IF ANY)              
         MVC   DOUBLE,BUYDT1       SAVE DATE (IF ANY)                           
*                                                                               
         GOTO1 VCALLOV,DMCB,BUYHDH    GET LOWER SCREEN                          
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVSCRN,BYTE         SET SCREEN ID                                
*                                                                               
         CLI   MADSW,C'Y'          IF DOING PBU UPLOAD                          
         BNE   MYOVR30                                                          
*                                                                               
*        SET TRANSACTION CODE AND DATE                                          
*                                                                               
         MVC   BUYTR1(2),SVTRCODE  RESTORE TRANSACTION CODE                     
         FOUT  BUYTR1H                                                          
         MVI   BUYTR1H+5,2         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET TO MODIFIED                              
*                                                                               
         MVC   BUYDT1,DOUBLE       RESTORE TRANSACTION DATE                     
         FOUT  BUYDT1H             FORCE RE-DISPLAY                             
         MVC   BUYDT1H+5(1),BYTE4  DATE INPUT LENGTH                            
         OI    BUYDT1H+1,X'01'     SET TO MODIFIED                              
         OI    BUYDT1H+4,X'20'     SET AS PREVIOUSLY VALIDATED                  
*                                                                               
MYOVR30  DS    0H             CALL ADDTNL CHRGS OR CUSTOM COL'S PROGRAM         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'16'                                                       
*                                                                               
         CLI   SVSCRN,X'EC'        LOWER SCR ADDTNL CHRGS ?                     
         BE    *+8                 YES                                          
         MVI   DMCB,X'20'          MUST BE CUSTOM COLUMNS                       
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL,0(R1)                                                       
*                                                                               
         GOTO1 FULL,DMCB,(RC),(RA)                                              
*                                                                               
         CLC   SVTRCODE,=C'CU'     SKIP IF NOT CUSTCOL CHANGE                   
         BNE   MYOVRX                                                           
*                                                                               
         TM    CHGIND5,PCHFXRTQ+PCHGTRKQ  SKIP IF NO CHANGES                    
         BZ    MYOVRX                                                           
*                                                                               
*        UPDATE CHANGE ELEMENT FOR CHANGES                                      
*                                                                               
         XC    DMCB+1(3),DMCB+1                                                 
         MVI   DMCB,X'03'          LOAD OVERLAY 3                               
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
*                                                                               
         GOTOR (RF),DMCB,(RC),(RA),CHGELEMQ UPDATE CHGELEM                      
*                                                                               
MYOVRX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILSCR   NTR1  BASE=*,LABEL=*      FILL SCR WITH INSERTION DETAILS              
*                                                                               
         XC    PBUNBYDT,PBUNBYDT                                                
         MVI   BYTE4,0             SET BYTE4 TO 0 - NO ERRORS                   
         L     R5,ATHISTMP                                                      
         USING PDELD,R5                                                         
         CLC   PDELTYPE,=C'DEL*'   SEE IF DELETION                              
         BNE   FILSCR5                                                          
*                                                                               
FILDEL   DS    0H                                                               
         MVC   BUYTR1,=C'R '                                                    
         FOUT  BUYTR1H                                                          
         MVI   BUYTR1H+5,1         SET INPUT LENGHT                             
         OI    BUYTR1H+1,X'01'     SET MODIFIED BIT                             
*                                                                               
         CLI   PDELDATE+2,0        SEE IF NO DAY                                
         BE    FILD5                                                            
*                                                                               
         CLC   PDELDATE,BESST                                                   
         BL    FILD5E                                                           
         CLC   PDELDATE,BESEND                                                  
         BH    FILD5E                                                           
*                                                                               
         XC    BUYDT1,BUYDT1                                                    
         MVI   BSUBLN,0            CLEAR LAST BUYLINE SAVED                     
         GOTO1 VDATCON,DMCB,(3,PDELDATE),(7,BUYDT1)                             
         LA    R1,BUYDT1+5                                                      
         LA    R4,5                                                             
         B     FILD10                                                           
*                                                                               
FILD5    DS    0H                                                               
         CLC   PDELDATE(2),BESST                                                
         BL    FILD5E                                                           
         CLC   PDELDATE(2),BESEND                                               
         BH    FILD5E                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PDELDATE),(9,BUYDT1)                             
         XC    BUYDT1+3(5),BUYDT1+3              CLEAR /YY                      
         LA    R4,3                                                             
         LA    R1,BUYDT1+3                                                      
         B     FILD10                                                           
*                                                                               
FILD5E   DS    0H                  DATE NOT IN EST                              
         LA    R3,PERERR                                                        
         STC   R3,PDELERNO+1                                                    
         MVI   BYTE4,1             ERROR RETURN                                 
         B     FILS99X                                                          
*                                                                               
FILD10   DS    0H                                                               
         CLI   PDELLINE,1          TEST SUBLINE TO DISPLAY                      
         BE    FILD10X                                                          
         LA    R4,3(R4)                                                         
         MVI   0(R1),C'-'                                                       
         ZIC   R0,PDELLINE                                                      
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    FILD105                                                          
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  2(1,R1),DUB+7(1)                                                 
         OI    2(R1),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R7,DUB                                                           
         LA    R7,FALPHTAB(R7)                                                  
         MVC   1(1,R1),0(R7)                                                    
         B     FILD109                                                          
*                                                                               
FILD105  OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R1),DUB                                                      
         CLI   1(R1),C'0'                                                       
         BNE   FILD109                                                          
         MVC   1(2,R1),2(R1)                                                    
         BCTR  R4,0                DECREMENT R4                                 
*                                                                               
FILD109  DS    0H                                                               
FILD10X  FOUT  BUYDT1H                                                          
         STC   R4,BUYDT1H+5        SET INPUT LENGTH                             
         OI    BUYDT1H+1,X'01'     SET MODIFIED BIT                             
         B     FILS99X                                                          
         DROP  R5                                                               
*                                                                               
FILSCR5  DS    0H                                                               
         LA    R5,2(R5)            MUST BUMP PAST LENGTH                        
*                                                                               
         USING PINSD,R5                                                         
         MVC   BUYTR1,=C'B '                                                    
         CLI   PINSACTN,C'A'                                                    
         BE    *+10                                                             
         MVC   BUYTR1,=C'R '       SET TO RECALL FOR C OR D                     
*                                                                               
         FOUT  BUYTR1H                                                          
         MVI   BUYTR1H+5,1         SET INPUT LENGTH                             
         OI    BUYTR1H+1,X'01'     SET MODIFIED BIT                             
*                                                                               
         CLI   PINSDATE,X'FF'      SEE IF INS DATE WAS BAD                      
         BNE   FILS3                                                            
         CLI   PINSDATE+2,X'FF'    DATE CHANGE?                                 
         BNE   FILS2                                                            
         MVC   BUYDT1(3),=C'XXX'   THIS WILL CAUSE ERROR                        
         LA    R4,3                                                             
         B     FILS10X                                                          
*                                  IF DATE CHANGE, RECALL OLD DATE              
FILS2    MVC   PBUNBYDT,PINSDATE+5 FIRST SAVE NEW DATE                          
         GOTO1 VDATCON,DMCB,(3,PINSDATE+2),(20,PINSDATE)                        
*                                                                               
FILS3    LA    R4,PINSDATE+2       PAST 19                                      
*                                                                               
         CLC   PINSDATE+6(2),=C'  '                                             
         BE    FILS5                                                            
*                                                                               
         CLC   PINSDATE+6(2),=C'00'                                             
         BE    FILS5                                                            
*                                                                               
*        BUY DATE HAS DAY                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(0,0(R4)),(7,BUYDT1)                                
         GOTO1 (RF),DMCB,(0,0(R4)),(3,WORK+10)                                  
         LA    R1,BUYDT1+5                                                      
         LA    R4,5                                                             
         B     FILS10                                                           
*                                                                               
*        MONTHLY BUY                                                            
*                                                                               
FILS5    MVC   WORK(4),0(R4)                                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(9,BUYDT1)                                 
         GOTO1 (RF),DMCB,(0,WORK),(3,WORK+10)                                   
         XC    BUYDT1+3(5),BUYDT1+3              CLEAR /YY                      
         LA    R1,BUYDT1+3                                                      
         LA    R4,3                                                             
*                                                                               
FILS10   DS    0H                                                               
*                                                                               
* WORK+10(3) HAS INS DATE                                                       
*                                                                               
         CLC   WORK+10(3),BESST    SEE IF BEFORE EST START                      
         BNL   FILS10C                                                          
*                                                                               
FILS10A  LA    R3,PERERR           NOT IN EST PERIOD                            
         STC   R3,PINSERNO+1                                                    
         MVI   PINSERF,2           ERROR ON DATE                                
         MVI   BYTE4,1             SET TO ERROR RETURN                          
         B     FILS99X             EXIT                                         
*                                  WILL SKIP TO NEXT INSERTION                  
FILS10C  CLC   WORK+10(3),BESEND   SEE IF AFTER EST END                         
         BH    FILS10A                                                          
*                                                                               
         CLI   BUYTR1,C'B'         SEE IF BUYING                                
         BNE   FILS10F             THEN NO LINE NUMBER                          
*                                                                               
         LA    R6,SAVUHDR                                                       
         USING PHDRD,R6                                                         
*                                                                               
         CLI   PHDRTSTB,C'Y'       SEE IF ADDING AS TEST BUYS                   
         BNE   FILS10X                                                          
*                                                                               
         MVC   WORK+1(5),BUYDT1                                                 
         MVI   WORK,C'T'                                                        
         LA    R4,1(R4)                                                         
         MVC   BUYDT1(6),WORK                                                   
         B     FILS10X                                                          
         DROP  R6                                                               
*                                                                               
FILS10F  CLI   PINSLINE,1          TEST SUBLINE TO DISPLAY                      
         BE    FILS10X                                                          
*                                                                               
         LA    R4,3(R4)                                                         
         MVI   0(R1),C'-'                                                       
         ZIC   R0,PINSLINE                                                      
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    FILS105                                                          
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  2(1,R1),DUB+7(1)                                                 
         OI    2(R1),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R7,DUB                                                           
         LA    R7,FALPHTAB(R7)                                                  
         MVC   1(1,R1),0(R7)                                                    
         B     FILS109                                                          
*                                                                               
FILS105  OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R1),DUB                                                      
         CLI   1(R1),C'0'                                                       
         BNE   FILS109                                                          
         MVC   1(2,R1),2(R1)                                                    
         BCTR  R4,0                MUST DECREMENT R4                            
*                                                                               
FILS109  DS    0H                                                               
*                                                                               
FILS10X  FOUT  BUYDT1H                                                          
         STC   R4,BUYDT1H+5        SET INPUT LENGTH                             
         OI    BUYDT1H+1,X'01'     SET MODIFIED BIT                             
*                                                                               
         CLI   BUYTR1,C'B'         SEE IF BUYING                                
         BNE   FILS99X             NO MUST EXIT NOW                             
*                                                                               
         BRAS  RE,FILSCR2          GO FILL REST OF SCR                          
*                                                                               
FILS99X  J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ENTER HERE TO DISPLAY INS DATA (EXCEPT TR AND DATE)                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILSCR2  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,BUYDTCHG         PROCESS INSERTION DATE CHANGE                
*                                                                               
         LA    R2,BUYAD1H          AD CODE                                      
         LA    R4,PINSADCD                                                      
*                                                                               
         CLI   PINSACTN,C'A'       IF ACTION ADD THEN                           
         BE    FILS22                                                           
         CLC   AGYALPHA,=C'GZ'     ONLY FOR GZ CHANGE IS NOT ALLOWED            
         BE    FILS21                                                           
         CLC   AGYALPHA,=C'SJ'     TEST                                         
         BE    FILS21                                                           
         B     FILS22                                                           
*                                                                               
FILS21   DS    0H                  LOAD ORIG VALUE OF ADCODE                    
         LA    R4,BUYAD1           FROM A BUY SCR( RECALL WAS DONE)             
*                                                                               
FILS22   DS    0H                                                               
         BRAS  RE,FSETIN           SET AD CODE                                  
*                                                                               
         CLI   BUYMD,C'N'          SEE IF NEWSPAPERS                            
         BE    FILNS                                                            
         CLI   BUYMD,C'O'          SEE IF OUTDOOR                               
         BE    FILOS                                                            
*                                                                               
FILMS    DS    0H                  MAGAZINE, SUPPLEMENT, TRADE MEDIA            
         LA    R2,BUYSP1H                                                       
         BRAS  RE,CLRFLD           CLEAR SPACE FIELD                            
         LA    R4,PINSSPAC                                                      
         BRAS  RE,FSETIN                                                        
*                                                                               
         CLI   SAVUHDR+PHDRUTYP-PHDRD,C'E'       SEE IF EDR UPDATE              
         BNE   FILMS5                                                           
         CLI   SAVUHDR+PHDRRATE-PHDRD,C'E'       USE EDR RATE                   
         BE    FILMS5                                                           
*                                                                               
* OTHER VALUES LEAVE RATE EMPTY, MUST CLEAR RATE FIELD                          
*                                                                               
         LA    R2,BUYRT1H                                                       
         BRAS  RE,CLRFLD                                                        
         B     FILMS10                                                          
*                                                                               
FILMS5   DS    0H                                                               
         LA    R2,BUYRT1H                                                       
         LA    R4,PINSCOST                                                      
         CLI   PINSACTN,C'C'       UPLOAD ACTION IS CHANGE?                     
         JNE   *+18                                                             
         CLC   PINSCOST,=CL11' '                                                
         JH    *+8                                                              
         LA    R4,8(R2)            USE DISPLAYED RATE IF NOT SUPPLIED           
         BRAS  RE,FSETIN                                                        
*                                                                               
FILMS10  DS    0H                                                               
*                                                                               
         BRAS  RE,OSSCMCDT         PROCESS OS,SC,MC DATES                       
         LA    R2,BUYMC1H          NEED TO POINT THERE TO CONTINUE              
*                                                                               
FILMS20  DS    0H                                                               
         CLI   BUYMD,C'I'          CHECK IF MEDIA= I                            
         BE    FILMS25                                                          
         CLI   BUYMD,C'L'          TREAT SOCIAL SAME AS INTERACTIVE             
         BE    FILMS25                                                          
         CLI   BUYMD,C'V'          TREAT NVIDEO SAME AS INTERACTIVE             
         BE    FILMS25                                                          
         CLI   BUYMD,C'W'          TREAT LVIDEO SAME AS INTERACTIVE             
         BE    FILMS25                                                          
         CLI   BUYMD,C'D'          TREAT DIGITAL AUDIO SAMS AS INTERAC          
         BE    FILMS25                                                          
         CLI   BUYMD,C'B'          TREAT MOBILE SAME AS INTERACTIVE             
         BE    FILMS25                                                          
         LA    R2,BUYMC1H          FILOPTS IS COMMON TO ALL MEDIAS              
         LHI   RF,2                                                             
         BRAS  RE,FBUMPFDS         BUMPING 2 FIELDS OVER (NOT ONE!)             
         B     FILOPTS                                                          
         EJECT                                                                  
FILMS25  DS    0H                                                               
         LA    R2,BUYMC1H          FILOPTS IS COMMON TO ALL MEDIAS              
         BRAS  RE,FBUMPFD          BUMPING 2 FIELDS OVER (NOT ONE!)             
         B     FILOPTS                                                          
*                                                                               
FILNS    DS    0H                  FILL NEWSPAPER FIELDS                        
         LA    R2,BUYAD1H                                                       
         BRAS  RE,FBUMPFD                                                       
         BRAS  RE,CLRFLD           CLEAR SPACE FIELD                            
         LA    R4,PINSSPAC                                                      
         BRAS  RE,FSETIN                                                        
         BRAS  RE,FBUMPFD                                                       
*                                                                               
         CLI   SAVUHDR+PHDRUTYP-PHDRD,C'E'       SEE IF EDR UPDATE              
         BNE   FILNS4                                                           
         CLI   SAVUHDR+PHDRRATE-PHDRD,C'E'       USE EDR RATE                   
         BE    FILNS4                                                           
*                                                                               
* OTHER VALUES LEAVE RATE EMPTY CLEAR RATE FIELD                                
*                                                                               
         BRAS  RE,CLRFLD                                                        
         B     FILNS10             OTHER VALUES LEAVE                           
*                                                                               
FILNS4   DS    0H                                                               
         CLI   PINSCOST,C' '       SEE IF I HAVE A "TOTAL" COST                 
         BE    FILNS5                                                           
         MVI   WORK,C' '                                                        
         MVC   WORK+1(12),WORK                                                  
         MVI   WORK,C'T'                                                        
         MVC   WORK+1(9),PINSCOST                                               
         LA    R4,WORK                                                          
         BRAS  RE,FSETIN                                                        
         B     FILNS5X                                                          
*                                                                               
FILNS5   DS    0H                                                               
FILNS5K  LA    R4,PINSRATE                                                      
         CLI   PINSACTN,C'C'       UPLOAD ACTION IS CHANGE?                     
         JNE   *+18                                                             
         CLC   PINSRATE,=CL11' '                                                
         JH    *+8                                                              
         LA    R4,8(R2)            USE DISPLAYED RATE IF NOT SUPPLIED           
         BRAS  RE,FSETIN                                                        
*                                                                               
FILNS5X  DS    0H                                                               
FILNS10  BRAS  RE,FBUMPFD                                                       
         CLI   PINSPDSC,C' '       SEE IF I HAVE A PREMIUM DESC                 
         BE    FILNS15                                                          
         MVC   8(2,R2),PINSPDSC                                                 
         CLI   PINSPCST,C' '                                                    
         BNE   FILNS17                                                          
         FOUT  (R2)                                                             
         MVI   5(R2),2             SET INPUT LENGTH                             
         OI    1(R2),X'01'         SET ON MODIFIED                              
         B     FILNS20X                                                         
*                                                                               
FILNS15  DS    0H                                                               
         CLI   PINSPCST,C' '       SEE IF I HAVE A PREM COST                    
         BE    FILNS20X                                                         
*                                                                               
FILNS17  MVI   WORK,C' '                                                        
         MVC   WORK+1(12),WORK                                                  
         MVC   WORK(2),PINSPDSC                                                 
         MVI   WORK+2,C'/'                                                      
         CLI   PINSPDSC,C' '       SEE IF I HAD A PREMIUM DESC                  
         BE    FILNS20             NO                                           
         MVC   WORK+3(L'PINSPCST),PINSPCST                                      
         LA    R4,WORK                                                          
         BRAS  RE,FSETIN                                                        
         B     FILNS20X                                                         
*                                                                               
FILNS20  XC    8(11,R2),8(R2)      JUST IN CASE                                 
         MVC   WORK(L'PINSPCST),PINSPCST                                        
         LA    R4,WORK                                                          
         BRAS  RE,FSETIN                                                        
*                                                                               
FILNS20X BRAS  RE,FBUMPFD                                                       
*                                                                               
FILNS25  DS    0H                                                               
         LA    R4,PINSMATC                                                      
         LA    R1,REC+(PBDMDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
*                                                                               
FILNSX   LHI   RF,2                                                             
         BRAS  RE,FBUMPFDS                                                      
         B     FILOPTS                                                          
*                                                                               
         EJECT                                                                  
FILOS    DS    0H                  FILL OUTDOOR FIELDS                          
         LA    R2,BUYAD1H                                                       
         BRAS  RE,FBUMPFD                                                       
         LA    R4,PINSSHOW                                                      
         BRAS  RE,FSETIN                                                        
         BRAS  RE,FBUMPFD                                                       
         LA    R4,PINSREG                                                       
         BRAS  RE,FSETIN                                                        
         BRAS  RE,FBUMPFD                                                       
         LA    R4,PINSILL                                                       
         BRAS  RE,FSETIN                                                        
         BRAS  RE,FBUMPFD                                                       
*                                                                               
         LA    R4,PINSCOST                                                      
         CLI   PINSACTN,C'C'       UPLOAD ACTION IS CHANGE?                     
         JNE   *+18                                                             
         CLC   PINSCOST,=CL11' '                                                
         JH    *+8                                                              
         LA    R4,8(R2)            USE DISPLAYED RATE IF NOT SUPPLIED           
         BRAS  RE,FSETIN                                                        
         BRAS  RE,FBUMPFD                                                       
*                                                                               
FILOS25  DS    0H                                                               
         LA    R4,PINSCLOS                                                      
         LA    R1,REC+(PBDCDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
*                                                                               
         BRAS  RE,FBUMPFD                                                       
*                                                                               
         LA    R4,PINSMATC                                                      
         LA    R1,REC+(PBDMDATE-PBUYREC)                                        
         BRAS  RE,SETDATE                                                       
*                                                                               
FILOSX   LHI   RF,2                                                             
         BRAS  RE,FBUMPFDS                                                      
         B     FILOPTS                                                          
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
FILOPTS  DS    0H                  FILL OPTIONAL DATA LINES                     
*                                                                               
* NOTE - R2 POINTS TO FIRST OPTIONAL DATA LINE                                  
*                                                                               
         ST    R5,SAVER5           SAVE ADDR OF INS* REC                        
         ST    R2,SAVER2           SAVE ADDR OF 1ST OPTIONAL DATA LINE          
*                                                                               
         LA    R6,5                FOR BCT                                      
         CLI   BUYMD,C'O'          SEE IF OUTDOOR                               
         BNE   FILOP                                                            
         USING PINSD,R5                                                         
         CLI   PINSSHOW,C' '       SEE IF SHOWING GIVEN                         
         BH    FILOP                                                            
         CLI   PINSSPAC,C' '       SEE IF SPACE GIVEN                           
         BNH   FILOP                                                            
         LA    R4,PINSSPAC         USE AS FIRST OPTIONAL DATA LINE              
         MVI   0(R2),X'19'         ALTER FIELD HEADER FOR FSETIN                
*                                  TO 25 (17 + 8) = X'19'                       
         BRAS  RE,FSETIN                                                        
         MVI   0(R2),COMLEN        RESET TO 55 (47 + 8) =X'37'                  
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALIDATED                
         BRAS  RE,FBUMPFD          BUMP TO NEXT OPTIONAL DATA LINE              
         BCTR  R6,0                DECREMENT R6                                 
*                                                                               
FILOP    DS    0H                                                               
         MVC   BYTE3,PINSACTN      SAVE ACTION INTO BYTE3                       
*                                                                               
         DROP  R5                                                               
*                                                                               
FILOP00  DS    0H                                                               
*                                                                               
         CLI   BYTE3,C'A'          CK IF ADD                                    
         BE    FILOP05                                                          
*                                                                               
         CLC   AGYALPHA,=C'GZ'     ONLY FOR GZ CHANGE IS NOT ALLOWED            
         BE    FILOP01                                                          
*                                                                               
* * * *  CLC   AGYALPHA,=C'SJ'     TEST - REMOVED TO REFLECT NORMAL             
* * * *  BE    FILOP01             OPT FIELD VALIDATION                         
         B     FILOP05                                                          
*                                                                               
FILOP01  DS    0H                                                               
*                                                                               
         ZICM  R1,0(R5),(3)                                                     
         AR    R5,R1                                                            
         USING POPTD,R5            PUT USING ON DATA                            
*                                                                               
         CLC   =C'IC=',POPTDATA    SKIP LINE IF ITS A COMMENT                   
         BE    FILOP04                                                          
*                                                                               
         CLC   =C'PI=',POPTDATA    SKIP LINE IF ITS A COMMENT                   
         BE    FILOP04                                                          
*                                                                               
         CLC   =C'IB=',POPTDATA    SKIP LINE IF ITS A COMMENT                   
         BE    FILOP04                                                          
*                                                                               
         CLC   =C'SRC=',POPTDATA    SKIP LINE IF ITS A COMMENT                  
         BE    FILOP04                                                          
*                                                                               
         LA    R3,KEYWTAB          COMPARE OPTDATA WITH KEYWORDS                
*                                                                               
*        SEE IF OPTION IN KEYWORD TABLE                                         
*                                                                               
FILOP02  DS    0H                                                               
         CLI   0(R3),X'FF'         CK FOR =                                     
         BE    FILOP04             IF YES THEN ITS NOT A PREFIX                 
         ZIC   R1,0(R3)                                                         
         EX    R1,FILOP03          EX COMPARE                                   
         BE    FILOP06             IF YES ITS A PREFIX                          
         AHI   R3,8                IF NOT BUMP TO NEXT ENTRY                    
         B     FILOP02                                                          
*                                                                               
FILOP03  CLC   1(0,R3),POPTDATA                                                 
*                                                                               
FILOP04  DS    0H                                                               
         BRAS  RE,FBUMPFD          BUMP TO NEXT OPTIONAL DATA LINE              
         BCT   R6,FILOP00          DECREMENT R6                                 
         B     FILZZZ                                                           
         DROP  R5                                                               
*                                                                               
FILOP05  DS    0H                                                               
*                                                                               
         ZICM  R1,0(R5),(3)        BUMP TO NEXT PBU RECORD                      
         AR    R5,R1                                                            
         USING POPTD,R5                                                         
*                                                                               
FILOP06  DS    0H                                                               
*                                                                               
         CLC   POPTTYPE,=C'EIN*'   SEE IF AT END OF INS                         
         BE    FILXX                                                            
         CLC   POPTTYPE,=C'ZZZ*'   SEE IF ZZZ DATA                              
         BE    FILZZZ                                                           
         CLC   POPTTYPE,=C'CCL*'   SEE IF CUSTOM COLUMN DATA                    
         BE    FILZZZ                                                           
         CLC   POPTTYPE,=C'ACH*'   SEE IF ADDITIONAL CHARGE DATA                
         BE    FILZZZ                                                           
         CLC   POPTTYPE,=C'OPT*'   MORE OPTIONS                                 
         BE    *+6                                                              
         DC    H'0'                BAD DATA                                     
*                                                                               
         ZICM  R1,0(R5),(3)        OPTION LENGTH                                
*                                                                               
         SR    R0,R0                                                            
         LA    R0,POPTLENQ+4       + 4 FOR OPT*                                 
         SR    R1,R0                                                            
         STC   R1,5(R2)            SET INPUT LENGTH                             
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    FILOP00             TO PROTECT MYSELF FROM BAD DATA              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),POPTDATA    PUT OPTION ON SCREEN                         
         FOUT  (R2)                FORCE OPTION DISPLAY                         
         OI    1(R2),X'01'         SET ON MODIFIED                              
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALIDATED                
*                                                                               
         BRAS  RE,FBUMPFD          BUMP TO NEXT FIELD ON SCREEN                 
*                                                                               
**ZHYK     AHI   R6,-1                                                          
**         BNZ   *+6                                                            
**         DC    H'0'                                                           
**         AHI   R6,1                                                           
*                                                                               
         BCT   R6,FILOP00                                                       
*                                                                               
FILZZZ   DS    0H                                                               
*                                                                               
         XC    BYTE3,BYTE3                                                      
         L     R5,SAVER5           RESET TO INS* REC                            
         L     R2,SAVER2                                                        
*                                                                               
FILZ5    ZICM  R1,0(R5),2          BUMP TO NEXT PBU RECORD                      
         AR    R5,R1                                                            
*                                                                               
         USING POPTD,R5                                                         
*                                                                               
         CLC   POPTTYPE,=C'EIN*'   SEE IF AT END OF INS                         
         BE    FILXX                                                            
         CLC   POPTTYPE,=C'OPT*'   ELSE KEEP LOOKING FOR END OF INS             
         BE    FILZ5                                                            
         CLC   POPTTYPE,=C'CCL*'   CUSTOM COLUMNS HANDLED LATER                 
         BE    FILZ5                  KEEP LOOKING FOR END OF INS               
         CLC   POPTTYPE,=C'ACH*'   ADDITIONAL CHG HANDLED LATER                 
         BE    FILZ5                  KEEP LOOKING FOR END OF INS               
         CLC   POPTTYPE,=C'ZZZ*'   SEE IF ZZZ DATA                              
         BE    *+6                                                              
         DC    H'0'                BAD DATA                                     
*                                                                               
         CLC   BUYPR,=C'ZZZ'       IGNORE IF PRODUCT IS NOT ZZZ                 
         BNE   FILXX                                                            
*                                                                               
         DROP  R5                                                               
         USING PZZZD,R5                                                         
         LHI   RF,6                                                             
         BRAS  RE,FBUMPFDS                                                      
*                                                                               
         ZICM  R1,0(R5),(3)                                                     
*                                                                               
         SR    R0,R0                                                            
         LA    R0,PZZZLENQ+4       +4 FOR ZZZ*                                  
         SR    R1,R0                                                            
         STC   R1,5(R2)            SET INPUT LENGTH                             
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    FILXX               TO PROTECT MYSELF FROM BAD DATA              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),PZZZDATA                                                 
         FOUT  (R2)                                                             
         OI    1(R2),X'01'         SET ON MODIFIED                              
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALIDATED                
*                                                                               
FILXX    J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
KEYWTAB  DS    0H                                                               
         DC    AL1(02),CL7'AC=    '                                             
         DC    AL1(02),CL7'PC=    '                                             
         DC    AL1(02),CL7'CD=    '                                             
         DC    AL1(02),CL7'BD=    '                                             
         DC    AL1(02),CL7'PD=    '                                             
         DC    AL1(02),CL7'ID=    '                                             
         DC    AL1(02),CL7'D2=    '                                             
         DC    AL1(02),CL7'SD=    '                                             
         DC    AL1(04),CL7'SREP=  '                                             
         DC    AL1(03),CL7'TAX=   '                                             
         DC    AL1(03),CL7'FSI=   '                                             
         DC    AL1(02),CL7'OR=    '                                             
         DC    AL1(03),CL7'CLE=   '                                             
         DC    AL1(05),CL7'MANIO= '                                             
         DC    AL1(02),CL7'CU=    '                                             
         DC    AL1(04),CL7'UPID=  '                                             
         DC    AL1(03),CL7'REF=   '                                             
         DC    AL1(03),CL7'PST/   '                                             
         DC    AL1(02),CL7'PV=    '                                             
         DC    AL1(02),CL7'CT=    '                                             
         DC    AL1(03),CL7'GST=   '                                             
         DC    AL1(03),CL7'SFH=   '                                             
         DC    AL1(06),CL7'EXDAYS='                                             
******** DC    AL1(04),CL7'IMPS=  '                                             
         DC    AL1(05),CL7'EIMPS= '                                             
         DC    AL1(05),CL7'AIMPS= '                                             
         DC    AL1(03),CL7'CLD=   '                                             
         DC    AL1(03),CL7'OSD=   '                                             
         DC    AL1(06),CL7'EXDATE='                                             
******** DC    AL1(02),CL7'LW=    '                                             
         DC    AL1(05),CL7'TRAFF= '                                             
         DC    AL1(03),CL7'ISS=   '                                             
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PUT DATA (R4) INTO FIELDS (R2 POINTS TO HDR), CHK FOR NUMERIC INPUT           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FSETIN   DS    0H                                                               
         LR    RF,R4               SAVE ADDRESS OF DATA                         
         ZIC   R1,0(R2)            LENGTH OF HEADER + FIELD                     
         AHI   R1,-9               R1 = LENGTH OF FIELD + ONE BYTE              
         AR    R4,R1               SETS ME TO END OF MAXIMUM INTO DATA          
         LA    R1,1(R1)            R1 SHOULD NOW BE LENGTH OF SCR FLD           
FSETIN5  CLI   0(R4),C' '          SCAN BACKWARDS FOR NON SPACE                 
         BH    FSETIN20                                                         
         BCTR  R4,0                                                             
         BCT   R1,FSETIN5                                                       
         B     FSETINX             NO INPUT                                     
*                                                                               
FSETIN20 DS    0H                                                               
         STC   R1,5(R2)            SET INPUT LENGTH INTO HEADER                 
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
         FOUT  (R2)                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RF)       MOVE DATA                                    
*                                                                               
         LA    R1,1(R1)            MUST READJUST R1                             
*                                                                               
* NOTE: R4 NOW POINTS TO FIRST NON-BLANK CHAR WHEN SCANNING BACKWARDS           
*                                                                               
FSETIN25 MVI   BYTE,0                                                           
         CLI   0(R4),C'0'          SEE IF NON-NUMERIC                           
         BL    FSETIN30                                                         
         MVI   BYTE,X'08'                                                       
         BCTR  R4,0                                                             
         BCT   R1,FSETIN25                                                      
         B     FSETIN30                                                         
*                                                                               
* NOTE: BYTE WILL NOW BE SET TO X'08' ONLY IF ALL CHARS WERE NUMERIC            
*                                                                               
FSETIN30 OC    4(1,R2),BYTE        MAY SET ON NUMERIC                           
*                                                                               
FSETINX  BR    RE                  RETURN                                       
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FBUMPFDS ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,FBUMPFDS                                                      
         BR    RE                                                               
*                                                                               
FBUMPFD  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FALPHTAB DS    0H                                                               
         DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CLEAR AND FOUT FIELD AT R2                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRFLD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    CLRFLDX             DONE IF NO LENGTH                            
*                                                                               
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         SKIP IF NOT EXTENDED HEADER                  
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         NI    6(R2),X'BF'         UNSET CURSOR                                 
*                                                                               
CLRFLDX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FILL IN FIELDS ON SCREEN FROM GLOBBER AREA (FROM CONTRACT PROGRAM)            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILSCRC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CLEAR ALL UNPROTECTED FIELDS ON SCREEN                                        
*                                                                               
         LA    R2,BUYIDH           POINT TO FIRST FIELD ON SCREEN               
         SR    RF,RF                                                            
*                                                                               
FSCRCTOP DS    0H                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    FSCRCBMP                                                         
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    FSCRCEND            DONE IF SCREEN END REACHED                   
*                                                                               
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         SKIP IF NOT EXTENDED HEADER                  
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
*                                                                               
FSCRCBMP DS    0H                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
         B     FSCRCTOP                                                         
*                                                                               
FSCRCEND DS    0H                                                               
         MVI   CONSW,C'Y'          INDICATE CALLED BY CONTRACT                  
*                                                                               
         L     RF,VGLOBBER         POINT TO GLOBBER                             
*                                                                               
* FILL IN HEADLINE FLDS, MEDIA, CLT, PRD, EST, PUB, AND BUY DATE                
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',BUYMDH,,GLVPRMD                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=C'DELE'              DELETE                           
         GOTO1 (RF),(R1),=C'GETF',BUYCLH,,GLVPRCLT                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=C'DELE'              DELETE                           
         GOTO1 (RF),(R1),=C'GETF',BUYPRH,,GLVPRPRD                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=C'DELE'              DELETE                           
         GOTO1 (RF),(R1),=C'GETF',BUYPBH,,GLVPRPUB                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=C'DELE'              DELETE                           
         GOTO1 (RF),(R1),=C'GETD',X,4,GLVPREST    ESTIMATE                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=C'DELE'              DELETE                           
*                                                                               
         LA    RE,4               LOOP COUNTER                                  
         LA    RF,X+3             POINT TO END OF EST NUMBER AREA               
FCLCEUP  CLI   0(RF),C' '                                                       
         BH    FCLCEX                                                           
         AHI   RF,-1              MOVE TO LEFT IN EST NUMBER AREA               
         BCT   RE,FCLCEUP                                                       
         DC    H'0'               ESTIMATE CANNOT BE BLANK                      
FCLCEX   OI    BUYESH+4,X'0A'     SET AS VALID NUMERIC AND HEX                  
         STC   RE,BUYESH+5        SET DATA LENGTH                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BUYES(0),X         ESTIMATE                                      
         OI    BUYESH+1,X'01'     SET MODIFIED                                  
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',X,8,GLVPRPER   BUY DATE                   
         CLI   DMCB+8,0                             (MMMDD...)                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=C'DELE'              DELETE                           
*                                                                               
         LA    RE,8               LOOP COUNTER                                  
         LA    RF,X+7             POINT TO END OF DATE AREA                     
FCLCDUP  CLI   0(RF),C' '                                                       
         BH    FCLCDX                                                           
         AHI   RF,-1              MOVE TO LEFT IN DATE AREA                     
         BCT   RE,FCLCDUP                                                       
         DC    H'0'               DATE CANNOT BE BLANK                          
FCLCDX   MVI   BUYDT1H+4,0        KILL INPUT INDICATORS                         
         STC   RE,BUYDT1H+5       SET DATA LENGTH                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BUYDT1(0),X        BUY DATE                                      
*                                                                               
         MVC   BUYNM(3),=C'CON'                                                 
         MVI   BUYNMH+4,0         KILL INPUT INDICATORS                         
         MVI   BUYNMH+5,3         SET DATA LENGTH                               
*                                                                               
         MVI   BUYTR1,C'R'                                                      
         MVI   BUYTR1H+4,0        KILL INPUT INDICATORS                         
         MVI   BUYTR1H+5,1        SET DATA LENGTH                               
         OI    BUYTR1H+1,X'01'     SET MODIFIED                                 
*                                                                               
         FOUT  BUYMDH             FORCE RE-TRANSMISSION OF MEDIA                
         FOUT  BUYCLH             FORCE RE-TRANSMISSION OF CLIENT               
         FOUT  BUYPRH             FORCE RE-TRANSMISSION OF PRODUCT              
         FOUT  BUYESH             FORCE RE-TRANSMISSION OF ESTIMATE             
         FOUT  BUYPBH             FORCE RE-TRANSMISSION OF PUB                  
         FOUT  BUYDT1H            FORCE RETRANSMISSION  OF BUY DATE             
         FOUT  BUYTR1H            FORCE RETRANSMISSION  OF BUY ACTION           
*                                                                               
FILSCRCX J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTRTN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   X,C' '              BLANK FILL                                   
         MVC   X+1(L'X-1),X                                                     
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BE    *+12                                                             
         CLI   PBDCOSIN,C'R'       ROADSIDE INPUT SO DISPLAY AS NET             
         BNE   FMTRTN1                                                          
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
FMTRTN1  CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    FMTRTN2                                                          
*                                                                               
         C     R1,=F'99999999'     TOTAL RATE OVER 999,999.99?                  
         BNH   FMTRTN1D                                                         
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          HAVE ENTERED PENNIES WHEN BUYING             
         LTR   R1,R1               (NO ROOM)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
FMTRTN1B EDIT  (R1),(9,X+5),0,FLOAT=-,ALIGN=LEFT                                
         B     FMTRTX                                                           
*                                                                               
FMTRTN1D CHI   R1,0                NEGATIVE RATE?                               
         BNL   FMTRTN1F                                                         
*                                                                               
         C     R1,=F'-99999999'    TOTAL RATE LESS THAN 999,999.99?             
         BH    FMTRTN1E                                                         
         MHI   R1,-1               DROP PENNIES AND DIMES                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         MHI   R1,-1                                                            
         B     FMTRTN1B                                                         
*                                                                               
FMTRTN1E C     R1,=F'-999999'      TOTAL RATE LESS THAN 9,999.99?               
         BNL   FMTRTN1F                                                         
         MHI   R1,-1               DROP PENNIES, LEAVE DIMES                    
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MHI   R1,-1                                                            
         EDIT  (R1),(9,X+5),1,FLOAT=-,ALIGN=LEFT                                
         B     FMTRTX                                                           
*                                                                               
FMTRTN1F EDIT  (R1),(9,X+5),2,FLOAT=-,ALIGN=LEFT                                
         B     FMTRTX                                                           
*                                                                               
FMTRTN2  EDIT  (R1),(11,X+5),5,FLOAT=-,ALIGN=LEFT                               
*                                                                               
         LA    R6,X+5              START OF OUTPUT                              
         AR    R6,R0               + LENGTH                                     
         AHI   R6,-3               BACK UP TO LAST 3 BYTES                      
         CLC   =C'000',0(R6)                                                    
         BNE   *+10                                                             
         MVC   0(3,R6),X+60        MOVE SOME BLANKS                             
FMTRTX   DS    0H                                                               
         LA    R1,X+5                                                           
*                                                                               
* IF COST TYPE NOT 'U' DISPLAY IT, ELSE DISPLAY COST IND IF NOT C' '            
*                                                                               
         CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    *+12                YES - CHECK PBDCOSIN                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSTY                                                 
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTRTX2                                                          
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
*                                                                               
FMTRTX2  DS    0H                                                               
         CLI   PBDCTYP,C'N'        NET INPUT                                    
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
*                                                                               
         TM    PBDRLIND,X'08'      TEST FROZEN RATE                             
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
*                                                                               
FMTRTX4  DS    0H                                                               
         CLI   9(R1),C'.'          LAST CHAR IS A DECIMAL POINT?                
         BNE   *+8                                                              
         MVI   9(R1),0             BLANK OUT MEANINGLESS DECIMAL PT             
         MVC   8(10,R2),0(R1)                                                   
         CP    PBDCOS,=P'0'                                                     
         BNZ   FMTRTX8                                                          
*                                                                               
         XC    8(10,R2),8(R2)                                                   
         CLI   PBDCOSIN,C'S'       TEST DEFAULT COST TYPE                       
         BE    FMTRTX5                                                          
         CLI   PBDCOSIN,C'R'                                                    
         BNE   FMTRTX6                                                          
FMTRTX5  MVC   8(1,R2),PBDCOSIN                                                 
         MVC   9(4,R2),=C'FREE'                                                 
         B     FMTRTX8                                                          
FMTRTX6  DS    0H                                                               
         MVC   8(4,R2),=C'FREE'                                                 
*                                                                               
FMTRTX8  DS    0H                                                               
         MVI   7(R2),0                                                          
         FOUT  (R2)                                                             
*                                                                               
         J      EXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP   RB                                                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTINS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,INVDTERR                                                      
         L     R2,TRADDR                                                        
         SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         JE    ERR_EXT                                                          
*                                                                               
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          BUYING?                                      
         BNE   *+16                                                             
         TM    SVPRDSTA,X'20'      NO TRAFFIC?                                  
         BZ    *+8                                                              
         OI    PBDSTAT,X'20'       THIS BUY IS NO TRAFFIC                       
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    *+12                                                             
         CLI   DDLINKSW,C'C'       CHG INSERTION UPLOAD?                        
         BNE   *+24                                                             
         CLI   PBDBFD,C'W'         WEEK-OF INSERTION TYPE (IN NEWREC)?          
         BE    *+12                                                             
         CLI   PBDBFD,C'B'         BEST-FOOD-DAY INS TYPE (IN NEWREC)?          
         BNE   *+08                                                             
         MVI   PBDBFD,0            RESET IT, IT WILL BE REVALIDATED             
*                                                                               
* MOVE WHOLE FIELD TO DUMEL                                                     
*                                                                               
         XC    DUMEL(20),DUMEL                                                  
         MVC   DUMEL(L'BUYDT1+8),0(R2)                                          
         LA    R7,DUMEL                                                         
         CLI   8(R7),C'B'          BEST FOOD DAY                                
         BE    EDTINSC                                                          
         CLI   8(R7),C'W'          WEEK OF                                      
         BE    EDTINSC                                                          
*                                                                               
EDTINS0  CLI   8(R7),C'T'                                                       
         BE    EDTINSC                                                          
         CLC   8(3,R7),=C'SEP'     MONTH OF SEP?                                
         BE    EDTINS1                                                          
         CLI   8(R7),C'S'          STEWARDSHIP?                                 
         BNE   EDTINSA                                                          
         TM    SVESPROF+29,X'40'   STEWARDSHIP ESTIMATE?                        
         JZ    ERR_EXT                                                          
         MVI   8(R7),C'T'          VALIDATE IT AS TEST STATUS                   
         B     EDTINSC                                                          
*                                                                               
EDTINSA  CLI   8(R7),C'P'                                                       
         BNE   EDTINS1                                                          
*                                                                               
EDTINSC  DS    0H                                                               
         MVC   PBDBFD,8(R7)        IN NEWREC                                    
         SR    R0,R0                                                            
         IC    R0,5(R7)                                                         
         BCTR  R0,R0                                                            
         STC   R0,5(R7)                                                         
         MVC   8(L'BUYDT1,R7),9(R7)                                             
*                                                                               
EDTINS1  TM    SVESPROF+29,X'80'   SEE IF TEST ESTIMATE                         
         BZ    EDTINS1B                                                         
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          SEE IF BUYING                                
         BNE   EDTINS1C                                                         
         CLI   PBDBFD,C'T'                                                      
         BE    EDTINS1C                                                         
         CLI   PBDBFD,C' '                                                      
         JH    ERR_EXT                                                          
         MVI   PBDBFD,C'T'         IF BUYING ON TEST EST, SET TEST BUY          
*                                                                               
EDTINS1B TM    SVESPROF+29,X'01'   SEE IF SFH ESTIMATE                          
         BZ    EDTINS1C                                                         
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          SEE IF BUYING                                
         BNE   EDTINS1C                                                         
         TM    PBDSTAT,X'0C'       X'08' AND X'04'                              
         BO    EDTINS1C                                                         
         OI    PBDSTAT,X'0C'                                                    
*                                                                               
EDTINS1C TM    SVESPROF+29,X'40'   STEWARDSHIP ESTIMATE?                        
         BZ    *+8                                                              
         OI    PBDSTAT2,X'40'      SET INSERTION STATUS TO STEWARDSHIP          
*                                                                               
* IF BUYING ON SFH  ESTIMATE SET ON SFH BUY AND HELD BITS                       
* "HELD" BUYS DO NOT APPEAR ON I/OS OR CONTRACTS                                
*                                                                               
         MVC   WORK(10),8(R7)                                                   
         MVI   PBDFREQ,0           SET NOT A 'MONTLY'                           
         CLI   WORK+3,C'0'         TEST MONTH ONLY                              
         BNL   EDTINS2                                                          
         MVI   PBDFREQ,C'M'        SET 'MONTLY' IF NO DAY GIVEN                 
*                                                                               
         CLI   PBDBFD,C'B'         BEST-FOOD-DAY MONTHLY INS DT FORMAT?         
         JE    ERR_EXT             BMMM FORMAT IS NOT ALLOWED                   
         CLI   PBDBFD,C'W'         WEEK-OF MONTHLY INS DT FORMAT?               
         JE    ERR_EXT             WMMM FORMAT IS NOT ALLOWED                   
*                                                                               
         IC    RF,5(R7)                                                         
         LA    RF,2(RF)                                                         
         STC   RF,5(R7)            ADJUST INPUT LENGTH                          
         MVC   WORK+3(2),=C'01'                                                 
         MVC   WORK+5(3),8+3(R7)                                                
EDTINS2  GOTO1 VDATVAL,DMCB,(1,WORK),WORK+16                                    
         L     R4,0(R1)            GET EDITED FIELD LEN                         
         LTR   R4,R4                                                            
         JZ    ERR_EXT                                                          
*                                                                               
* EDIT SUBLINE IF PRESENT                                                       
*                                                                               
         SR    R5,R5                                                            
         IC    R5,5(R7)            GET INPUT FIELD LEN                          
         SR    R5,R4               SUBTRACT EDITED LEN                          
         BZ    EDTINS3H            SUBLINE NOT GIVEN                            
         LA    R3,SUBLNERR                                                      
         CHI   R5,2                                                             
         JL    ERR_EXT                                                          
         LA    R4,WORK(R4)                                                      
         CLI   0(R4),C'-'                                                       
         JNE   ERR_EXT                                                          
         MVI   WORK+30,0                                                        
         BCTR  R5,0                DECREMENT FOR -                              
         BCTR  R5,0                SET FOR EXECUTE                              
         LA    R4,1(R4)            POINT TO FIRST DIGIT                         
         CLI   0(R4),C'0'          SEE IF DIGIT                                 
         BNL   EDTINS3             YES                                          
         CLI   0(R4),C'A'          ALLOW A0 TO N9                               
         JL    ERR_EXT             FOR 100 - 255                                
         CLI   0(R4),C'P'                                                       
         JH    ERR_EXT                                                          
         BNE   EDTINS2C                                                         
         CLI   1(R4),C'5'          MAX IS P5 - 255                              
         JH    ERR_EXT                                                          
*                                                                               
EDTINS2C MVC   WORK+30(1),0(R4)    SAVE IN WORK+30                              
         LA    R4,1(R4)                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,5(R7)                                                         
         CHI   R0,7                MAX INPUT LENGTH?                            
         BL    EDTINS2K                                                         
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         BNE   EDTINS2K                                                         
         SR    R0,R0                                                            
         ICM   R0,3,REC+(PBUYKLIN-PBUYREC)                                      
         CHI   R0,10                                                            
         BNL   EDTINS3E            USE SUBLINE FROM REC                         
*                                                                               
EDTINS2K BCTR  R5,0                DECREMENT FOR ALPHA                          
         CHI   R5,0                                                             
         JNE   ERR_EXT                                                          
*                                                                               
EDTINS3  EX    R5,PACKSUB                                                       
         LA    R5,1(R5)            RESTORE LEN                                  
EDTINS3A CLI   0(R4),C'0'                                                       
         JL    ERR_EXT                                                          
         CLI   0(R4),C'9'                                                       
         JH    ERR_EXT                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,EDTINS3A                                                      
         CVB   R0,DUB                                                           
         CLI   WORK+30,0                                                        
         BE    EDTINS3E                                                         
         LA    R4,EALPHTAB                                                      
         LHI   RF,100                                                           
EDTINS3B CLC   WORK+30(1),0(R4)                                                 
         BE    EDTINS3C                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,1(R4)                                                         
         AHI   RF,10                                                            
         B     EDTINS3B                                                         
*                                                                               
EDTINS3C AR    R0,RF                                                            
*                                                                               
EDTINS3E STC   R0,NEWREC+24                                                     
         STC   R0,BSUBLN                                                        
*                                                                               
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          TEST TR = BUY                                
         JE    ERR_EXT             YES - NO SUBLINE SHOULD BE GIVEN             
*                                                                               
EDTINS3H DS    0H                                                               
         GOTO1 VDATCON,(R1),(0,WORK+16),(3,DUB)                                 
         MVC   DUB(1),BESST                                                     
         CLC   BESST(1),BESEND     TEST EST ALL IN ONE YEAR                     
         BE    EDTINS4             YES                                          
         CLC   BESST+1(2),DUB+1    TEST INS M/D GT EST ST M/D                   
         BNH   EDTINS4             NO                                           
         MVC   DUB(1),BESEND                                                    
*                                                                               
EDTINS4  LA    R3,PERERR           DATE NOT IN EST PERIOD                       
         CLC   BESST,DUB                                                        
         JH    ERR_EXT                                                          
         CLC   BESEND,DUB                                                       
         JL    ERR_EXT                                                          
*                                                                               
* MUST RE-EDIT DATE SINCE PREVIOUS CODE ALLOWS FEB/29 FOR ANY YEAR              
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DUB),(5,WORK)                                    
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+16                                    
         LA    R3,INVDTERR                                                      
         L     R4,0(R1)            GET EDITED FIELD LEN                         
         LTR   R4,R4                                                            
         JZ    ERR_EXT                                                          
         CLC   SVINSDT,DUB         SEE IF DATE CHANGED                          
         BNE   EDTINS4B                                                         
         CLI   DDLINKSW,0          ADBUYER UPLOAD?                              
         BNE   EDTINS4F                                                         
         TM    4(R2),X'20'         SEE IF PREV VALIDATED                        
         BZ    EDTINS4D            NO, NEW LINE NUMBER                          
*                                                                               
* NEW LINE NUMBER MEANS NEW INSERTION, SO CLEAR LASTPBEL                        
* LASTPBEL=ADDR OF LAST BILL/PAY ELEM                                           
*                                                                               
         B     EDTINS4F                                                         
*                                                                               
* SPECIAL CODE FOR BACKER - PHILIP MORRIS                                       
* THEY CAN'T CHG DATE OF BUY BECAUSE OF SPECIAL                                 
* INTERFACE WITH PHILIP MORRIS SYSTEM AND PM REPORT                             
*                                                                               
EDTINS4B CLC   AGYALPHA,=C'BS'     SPECIAL EDIT FOR BACKER                      
         BNE   EDTINS4C                                                         
         CLC   BUYCL(3),=C'PM '                                                 
         BNE   EDTINS4C            SPECIAL FOR PHILIP MORRIS                    
         LA    R3,NOCHGERR         DATE CAN'T BE CHANGED FOR PM                 
         L     R4,TRADDR                                                        
         CLI   8(R4),C'C'          TEST TR = CHA                                
         JE    ERR_EXT             YES - CAN'T CHG DATE                         
*                                                                               
EDTINS4C DS    0H                  SEE IF MATCHED (NO DATE CHANGE)              
         L     R4,TRADDR                                                        
         CLI   8(R4),C'C'          SEE IF CHANGE                                
         BNE   EDTINS4K                                                         
         LA    R4,REC                                                           
         TM    PBDSTAT-NEWREC(R4),X'40'          SEE IF MATCHED                 
         BZ    EDTINS4D                                                         
         LA    R3,MATERR                                                        
         J     ERR_EXT                                                          
*                                                                               
EDTINS4D DS    0H                                                               
         LA    R5,REC+33           CHK FOR UPLOAD ELEM                          
         MVI   ELCODE,X'90'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   EDTINS4K                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTINS4K                                                         
**NO-OP  LA    R3,NOCHGERR         DATE CAN'T BE CHANGED IF UPLOADED            
**05/19  J     ERR_EXT                                                          
*                                                                               
* THEY SHOULD DELETE AND RE-ADD                                                 
*                                                                               
EDTINS4K DS    0H                  I GET HERE ON DATE CHANGES                   
         XC    LASTPBEL,LASTPBEL   CLEAR RX CONTROL                             
         XC    LASTCELD,LASTCELD   CLEAR RA DATE                                
         MVI   CHGELCNT,0          INITIALIZE CHG ELEM COUNTER                  
         B     EDTINS4X                                                         
*                                                                               
EDTINS4F DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED IF DATE ITSELF                 
*                                  NOT CHANGED                                  
EDTINS4X DS    0H                                                               
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          SEE IF BUYING                                
         BE    EDTINS4Y                                                         
         CLI   8(R4),C'C'          SEE IF CHANGING                              
         BNE   EDTINS4Z                                                         
         CLC   SVINSDT,DUB         SEE IF DATE CHANGED                          
         BE    EDTINS4Z            NO                                           
*                                                                               
EDTINS4Y DS    0H                                                               
         BRAS  RE,VALISS           VALIDATE INS DATE VS. THOSE                  
*                                  IN THE ISSUE DATE RECORD                     
EDTINS4Z DS    0H                                                               
*                                                                               
         MVC   PBUYKDAT,DUB        MOVE TO KEY IN NEWREC                        
         CLI   8(R4),C'B'          SEE IF BUYING                                
         BE    E4ZTOP              TEST FROZEN CLIENT                           
         CLI   8(R4),C'C'          SEE IF CHANGING                              
         BNE   E4ZOK               NOT CHANGE (OR BUY)                          
         CLC   SVINSDT,DUB         SEE IF DATE CHANGED                          
         BE    E4ZOK               NO                                           
*                                                                               
* SVCLPROF+30 CONTAINS PCLTSTAT FROM PCLTREC                                    
*                                                                               
E4ZTOP   DS    0H                                                               
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT ?                              
         BNO   E4ZOK               NO                                           
         TM    SVCLPROF+30,X'10'   FROZEN WITH DATE ?                           
         BNO   E4ZOK               NO                                           
*                                                                               
         LA    R3,FDTERR           ERROR 139 - DATE FROZEN FOR CLIENT           
*                                                                               
* SVCLPROF+27 CONTAINS INDICATOR FROM FREEZE STATUS ELEM IN PCLTREC             
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         BO    E4ZFORW             YES                                          
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         BO    E4ZPAST             YES                                          
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         BO    *+6                 YES                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
* SVCLPROF+28 CONTAINS DATE (YM) FROM FREEZE STATUS ELEM IN PCLTREC             
*                                                                               
         CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BNE   E4ZOK                                                            
         J     ERR_EXT             NO BUYING FOR THIS MONTH                     
*                                                                               
E4ZFORW  CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BL    E4ZOK                                                            
         J     ERR_EXT             NO BUYING FOR THIS MONTH & FORWARD           
*                                                                               
E4ZPAST  CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BH    E4ZOK                                                            
         J     ERR_EXT             NO BUYING FOR THIS MONTH & PRIOR             
*                                                                               
E4ZOK    DS    0H                                                               
         MVC   SVINSDT,DUB                                                      
         MVC   BINSDT,DUB                                                       
*                                                                               
         J     EXIT                                                             
*                                                                               
PACKSUB  PACK  DUB,0(0,R4)                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
* CHECK INSERTION DATE VS. ISSUE RECORD DATES                                   
*                                                                               
VALISS   NTR1                                                                   
         OC    SADVDATA,SADVDATA   CHECK IF NEW AOR SYSTEM                      
         BZ    BVALISS             IF NOT CHECK BYPROF                          
         TM    SVAORC,X'80'        SEE IF AOR ISSUE RECORD DATE                 
         BNO   BVALISS             IS REQUIRED                                  
*                                  IF NOT STILL CHECK BYPROF                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'29'                                                      
         MVC   KEY+4(6),BPUB                                                    
         ZIC   R0,DUB              YEAR                                         
         CVD   R0,WORK                                                          
         UNPK  KEY+12(2),WORK+6(2)                                              
         OI    KEY+13,X'F0'                                                     
         MVC   KEY+10(2),=C'19'                                                 
         CLC   KEY+12(2),=C'70'    IF YEAR IS HIGHER THAN 70                    
         BH    *+10                ASSUME 19XX                                  
         MVC   KEY+10(2),=C'20'    IF LOWER ASSUME 20XX                         
*                                                                               
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BE    VALI5                                                            
*                                                                               
         TM    SVAORC,X'01'        TEST PUB LINK REQUIRED                       
         BZ    *+10                                                             
         MVC   KEY+4(6),SVADVPUB                                                
         MVC   KEY(2),SVAOR                                                     
*                                  MUST SWITCH TO AOR                           
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    VALI5                                                            
         LA    R3,ADVFERR          AOR NOT ACTIVE                               
         J     ERR_EXT                                                          
*                                                                               
VALI5    GOTOR HIGH                                                             
VALI10   CLC   KEY(14),KEYSAVE     CHECK THROUGH YEAR                           
         BE    VALI15              FOUND PROPER  KEY                            
*                                                                               
* SEE IF I WAS LOOKING FOR ALL ZONE/EDT REC                                     
*                                                                               
         CLC   KEYSAVE+8(2),=X'FFFF'                                            
         BE    VALIMISS                                                         
*                                  NO ISSUE DATE FOUND                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+8(2),=X'FFFF'   TRY FOR ALL ZONE/EDT RECORD                  
         B     VALI5                                                            
*                                                                               
VALI15   DS    0H                                                               
         MVC   AREC,AWRKREC                                                     
         GOTOR GETPRT                                                           
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BE    VALI18                                                           
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
VALI18   DS    0H                                                               
         L     R4,AWRKREC                                                       
         LA    R5,33(R4)                                                        
         MVI   ELCODE,X'29'                                                     
         CLI   0(R5),X'29'                                                      
         BE    VALI25                                                           
VALI20   BRAS  R9,NEXTEL                                                        
         BNE   VALIDERR            DATE NOT FOUND                               
*                                                                               
VALI25   CLC   DUB(3),2(R5)                                                     
         BE    VALIOK                                                           
         CLI   4(R5),0             SEE IF DAY GIVEN                             
         BNE   VALI20              YES, THEN MUST MATCH                         
         MVC   WORK(3),2(R5)                                                    
         MVI   WORK+2,1            SET DAY TO 1 AND TRY AGAIN                   
         CLC   DUB(3),WORK                                                      
         BE    VALIOK                                                           
         B     VALI20              KEEP LOOKING                                 
*                                                                               
VALIOK   DS    0H                                                               
         B     VALISSX                                                          
*                                                                               
VALIMISS DS    0H                  NO ISSUE DATE RECORD FOUND                   
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BE    VALISSX                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
         B     VALISSX                                                          
*                                                                               
* GET HERE IF NO ISSUE DATE RECORD FOUND (NON-AOR)                              
*                                                                               
VALIERR  DS    0H                                                               
         CLI   BYPROF+4,C'D'       ALLOW BUY IF RECORD NOT FOUND                
         BNE   VALISSX             UNLESS ISSUE DATE RECORD AND DATE            
*                                  ARE REQUIRED                                 
         LA    R3,ISSERR           ERR 234, DATE NOT IN ISSUE DATE REC          
         J     ERR_EXT                                                          
*                                                                               
* GET HERE IF RECORD FOUND BUT DATE NOT IN IT (AOR AND NON-AOR)                 
*                                                                               
VALIDERR DS    0H                                                               
         LA    R3,ISSERR           ERR 234, DATE NOT IN ISSUE DATE REC          
         J     ERR_EXT                                                          
*                                                                               
BVALISS  DS    0H                  NON-AOR ISSUE DATE LOGIC                     
         CLI   BYPROF+4,0          FIRST CHECK IF BYPROF OPTION PRESENT         
         BE    VALISSX             NO - THEN DONE                               
         CLI   BYPROF+4,C'N'       SEE IF ISSUE DATE REQUIRED                   
         BE    VALISSX             NO - THEN DONE                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'29'                                                      
         MVC   KEY+4(6),BPUB                                                    
         ZIC   R0,DUB              YEAR                                         
         CVD   R0,WORK                                                          
         UNPK  KEY+12(2),WORK+6(2)                                              
         OI    KEY+13,X'F0'                                                     
         MVC   KEY+10(2),=C'19'                                                 
         CLC   KEY+12(2),=C'70'    IF YEAR IS HIGHER THAN 70                    
         BH    *+10                ASSUME 19XX                                  
         MVC   KEY+10(2),=C'20'    IF LOWER ASSUME 20XX                         
*                                                                               
BVALI5   GOTOR HIGH                                                             
BVALI10  CLC   KEY(14),KEYSAVE     CHECK THROUGH YEAR                           
         BE    BVALI15             FOUND PROPER  KEY                            
*                                                                               
* SEE IF I WAS LOOKING FOR ALL ZONE/EDT REC                                     
*                                                                               
         CLC   KEYSAVE+8(2),=X'FFFF'                                            
         BE    VALIERR                                                          
*                                  NO ISSUE DATE FOUND                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+8(2),=X'FFFF'   TRY FOR ALL ZONE/EDT RECORD                  
         B     BVALI5                                                           
*                                                                               
BVALI15  DS    0H                                                               
         MVC   AREC,AWRKREC                                                     
         GOTOR GETPRT                                                           
         LA    R0,REC                                                           
         ST    R0,AREC             MUST RESTORE AREC                            
*                                                                               
BVALI18  DS    0H                                                               
         L     R4,AWRKREC                                                       
         LA    R5,33(R4)                                                        
         MVI   ELCODE,X'29'                                                     
         CLI   0(R5),X'29'                                                      
         BE    BVALI25                                                          
BVALI20  BRAS  R9,NEXTEL                                                        
         BNE   VALIDERR            DATE NOT FOUND                               
*                                                                               
BVALI25  CLC   DUB(3),2(R5)                                                     
         BE    BVALIOK                                                          
         CLI   4(R5),0             SEE IF DAY GIVEN                             
         BNE   BVALI20             YES - THEN MUST MATCH                        
         MVC   WORK(3),2(R5)                                                    
         MVI   WORK+2,1            SET DAY TO 1 AND TRY AGAIN                   
         CLC   DUB(3),WORK                                                      
         BE    BVALIOK                                                          
         B     BVALI20             KEEP LOOKING                                 
*                                                                               
BVALIOK  DS    0H                                                               
         B     VALISSX                                                          
*                                                                               
VALISSX  J     EXIT                                                             
*                                                                               
ERR_EXT  L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUYDTCHG NTR1  BASE=*,LABEL=*                                                   
         CLI   MADSW,C'Y'                                                       
         JNE   EXIT                                                             
         OC    PBUNBYDT,PBUNBYDT   IS INSERTION DATE CHANGED?                   
         JZ    EXIT                                                             
         L     R5,ATHISTMP                                                      
         LA    R5,2(R5)                                                         
         USING PINSD,R5                                                         
         CLI   PINSACTN,C'C'       IF ACTION CHANGE                             
         JNE   EXIT                                                             
         GOTO1 VDATCON,DMCB,(3,PBUNBYDT),(20,PINSDATE) NEW DATE                 
         GOTO1 VDATCON,DMCB,(3,PBUNBYDT),(4,BUYDT1)    NEW DATE ON SCRN         
         DROP  R5                                                               
                                                                                
         FOUT  BUYDT1H                                                          
         MVI   BUYDT1H+5,5         SET INPUT LENGTH                             
         OI    BUYDT1H+1,X'01'     SET MODIFIED BIT                             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETSCDEL NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,FILSCR           FILL REST OF SCREEN FOR DEL*                 
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,5              CALL 05 TO RECALL BUY                        
         GOTO1 VCALLOV,DMCB,,(RA)  SET OVERLAY                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA) CALL OVERLAY FOR 05                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HIGH     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'PRTDIR',          +        
               KEY,KEY,(TERMNAL,0)                                              
         CLC   KEY(25),KEYSAVE     RECORD FOUND?                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETPRT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,KEY+27                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         +        
               (R2),AREC,(TERMNAL,DMWORK)                                       
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS       SET CONDITION CODE                           
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
READPUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMREAD'),=C'PUBDIR',          +        
               KEY,KEY,(TERMNAL,0)                                              
         CLC   KEY(25),KEYSAVE     RECORD FOUND?                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HIGHPUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'PUBDIR',          +        
               KEY,KEY,(TERMNAL,0)                                              
         CLC   KEY(25),KEYSAVE     RECORD FOUND?                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VGOPFM   NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
*        TRANSFER TO PFM                                                        
*                                                                               
         L     RF,ATIOB            POINT TO INPUT AREA                          
         USING TIOBD,RF            ESTABLISH AREA                               
         CLC   TIOBCURD,=X'0467'                                                
         BL    VGOPFMX                                                          
*                                                                               
         L     R3,VTWA             ADDR OF TWA                                  
         AH    R3,TIOBCURD         ADD DISP TO THE FIELD                        
*                                  R3 POINTS TO THE HEADER                      
         LA    R3,8(R3)            POINT TO FIELD DATA                          
*                                                                               
         CLI   1(R3),C'*'          REGULAR RECORD                               
         BE    VGOPFM10                                                         
         CLI   1(R3),C'D'          DELETED RECORD                               
         BE    VGOPFM10                                                         
         B     VGOPFMX                                                          
*                                                                               
VGOPFM10 DS    0H                                                               
         LA    R3,70(R3)           70(DISP TO ADDR FIELD ON SCREEN)             
         DROP  RF                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(R3),DUB,8,4      HEXIN CALL                           
*                                                                               
         XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R3,WORK                                                          
         USING GLVXFRSY,R3                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'BUY'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'PFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL SEND XCTL ELM             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK             ESTABLISH PFM CONTROL BLOCK                  
         USING GLPFMD,R3                                                        
*                                                                               
         MVI   GLPFMCD,GLPFMCDQ    SET TRANSFER CODE                            
         MVI   GLPFMLEN,56         SET CONTROL BLOCK LENGTH                     
         MVC   GLPFMFIL,=CL8'PRTFIL'  SET FILE                                  
         MVC   GLPFMDA,DUB         ADDRESS OF RECORD                            
******   MVC   GLPFMKEY(4),=C'*   '                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',GLPFMFIL,54,GLPFMCDQ                      
*                                                                               
*                                                                               
VGOPFMX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
ISSERR   EQU   234                 DATE NOT IN ISSUE DATE RECORD                
ACHGRERR EQU   29              ADDTNL CHRGS/CUST COLS NOT ALLOWED ERROR         
*                                                                               
EALPHTAB DS    0H                                                               
         DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         DROP  R3,RB                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FILL IN CUSTOM COLUMN SCREEN FROM PBU DATA                             
*                                                                               
*NTRY                                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DS    0D                                                               
FILSCR3  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ATHISTMP         START OF PBU RECORD                          
         LA    R5,2(R5)            BUMP PAST TOTAL RECORD LENGTH                
*                                                                               
         USING PCCLD,R5            ESTABLISH PBU RECORD AS CUSTOM COL           
*                                                                               
         LA    R2,BUYDT1H          POINT TO DATE FIELD                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO CUSTCOL ID FIELD                     
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO CUSTCOL ID FIELD                     
*                                                                               
FS3LOOP  DS    0H                                                               
*                                                                               
         CLC   =C'EIN*',PCCLTYPE   DONE AT END OF INSERTION                     
         BE    FS3DONE                                                          
*                                                                               
         CLC   =C'CCL*',PCCLTYPE   SKIP IF NOT CUSTCOL RECORD                   
         BNE   FS3CONT                                                          
*                                                                               
         CLC   PCCLCODE,=CL80' '   SKIP IF NO CODE                              
         BNH   FS3CONT                                                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO CUSTCOL ID FIELD                     
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
*        CCL ID TO FIELD ON SCREEN                                              
*                                                                               
         MVC   8(L'PCCLCODE,R2),PCCLCODE PUT CODE ON SCREEN                     
*                                                                               
*        FIND LENGTH OF ID                                                      
*                                                                               
         LA    RF,L'PCCLCODE       MAX LENGTH OF CODE                           
         LA    R1,8+L'PCCLCODE-1(R2) POINT TO LAST CHARACTER IN CODE            
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK IN CODE                  
         BH    *+10                                                             
         BCTR  R1,0                DECREMENT DATA POINTER                       
         BCT   RF,*-10             CHECK PREVIOUS CHARACTER                     
*                                                                               
         STC   RF,5(R2)            SET SCREEN FIELD LENGTH                      
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
         FOUT  (R2)                FORCE RE-DISPLAY                             
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO CUSTCOL DATA FIELD                   
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PCCLLEN        GET RECORD LENGTH                            
         SHI   RF,PCCLDATA-PCCLD   DECREMENT BY RECORD HEADER LENGTH            
*                                                                               
         STC   RF,5(R2)            SET DATA LENGTH                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),PCCLDATA    MOVE DATA TO SCREEN                          
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
         FOUT  (R2)                FORCE RE-DISPLAY                             
*                                                                               
FS3CONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PCCLLEN        GET RECORD LENGTH                            
         LA    R5,0(RF,R5)         BUMP TO NEXT PBU FIELD                       
         B     FS3LOOP                                                          
*                                                                               
FS3DONE  DS    0H                                                               
*                                                                               
FILSCR3X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FILL IN ADDITIONAL CHARGE SCREEN FROM PBU DATA                         
*                                                                               
*NTRY                                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DS    0D                                                               
FILSCR4  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        CLEAR OUT DISPLAYED SCREEN                                             
*                                                                               
         LA    R2,BUYDT1H          POINT TO DATE FIELD                          
*                                                                               
         LA    RF,7                                                             
         BRAS  RE,BUMPFLDS         BUMP TO 1ST ADDITIONAL CHARGE ID FLD         
*                                                                               
         LA    R5,10               TEN LINES ON SCREEN                          
*                                                                               
FS4CLRLP DS    0H                                                               
*                                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    *+8                                                              
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         LR    R3,R2               SAVE FIELD POINTER                           
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO ADDITIONAL CHARGE DESC FIELD         
         BRAS  RE,BUMPFLD                                                       
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO ADDITIONAL CHARGE DATA FIELD         
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO COMMISSION Y/N FIELD                 
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         TM    1(R3),X'20'         SKIP IF ADD CHG ID NOT PROTECTED             
         BNO   *+12                                                             
         MVI   8(R2),C'N'          DEFAULT TO NON COMMISSIONABLE                
         MVI   5(R2),1             LENGTH OF 1                                  
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO COMMISSION % FIELD                   
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO CD Y/N FIELD                         
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO ADDITIONAL CHG ID FLD                
*                                                                               
FS4CLRCN DS    0H                                                               
*                                                                               
         BCT   R5,FS4CLRLP                                                      
*                                                                               
FS4CLRDN DS    0H                                                               
*                                                                               
*        FILL IN SCREEN                                                         
*                                                                               
         L     R5,ATHISTMP         START OF PBU RECORD                          
         LA    R5,2(R5)            BUMP PAST TOTAL RECORD LENGTH                
*                                                                               
         USING PACHD,R5            ESTABLISH PBU RECORD AS CUSTOM COL           
*                                                                               
FS4LOOP  DS    0H                                                               
*                                                                               
         CLC   =C'EIN*',PACHTYPE   DONE AT END OF INSERTION                     
         BE    FS4DONE                                                          
*                                                                               
         CLC   =C'ACH*',PACHTYPE   SKIP IF NOT ADDITIONAL CHARGE RECORD         
         BNE   FS4CONT                                                          
*                                                                               
         CLC   PACHCODE,=CL2' '    SKIP IF NO CODE                              
         BNH   FS4CONT                                                          
*                                                                               
*        FIND LINE FOR DATA                                                     
*                                                                               
         LA    R2,BUYDT1H          POINT TO DATE FIELD                          
*                                                                               
         LA    RF,7                                                             
         BRAS  RE,BUMPFLDS         BUMP TO 1ST ADDITIONAL CHARGE ID FLD         
*                                                                               
         SR    R3,R3               INIT POINTER                                 
         LA    R4,10               10 LINES ON SCREEN                           
*                                                                               
FS4LNELP DS    0H                                                               
*                                                                               
         CLC   PACHCODE,8(R2)      IF CODE ALREADY ON SCREEN                    
         BNE   FS4LNE10                                                         
*                                                                               
         LR    R4,R2               SAVE CURRENT FIELD POINTER                   
*                                                                               
         LA    RF,3                BUMP TO CHARGE AMOUNT FIELD                  
         BRAS  RE,BUMPFLDS                                                      
*                                                                               
         LR    RF,R2               SAVE CHARGE FIELD POINTER                    
         LR    R2,R4               RESTORE CHARGE CODE POINTER                  
*                                                                               
         CLI   5(RF),0             USE IF CHARGE NOT ENTERED                    
         BE    FS4LNEFD                                                         
*                                                                               
FS4LNE10 DS    0H                                                               
*                                                                               
         CLC   8(2,R2),=CL2' '     SKIP IF NOT BLANKS                           
         BH    FS4LNECN                                                         
*                                                                               
         LTR   R3,R3               SKIP IF BLANK LINE ALREADY FOUND             
         BNZ   *+6                                                              
         LR    R3,R2                  SAVE POINTER TO FIELD                     
*                                                                               
FS4LNECN DS    0H                                                               
*                                                                               
         BCT   R4,*+8              COUNT LINES                                  
         B     FS4LNEDN            END OF SCREEN                                
*                                                                               
         LA    RF,7                NUMBER OF FIELDS ON LINE                     
         BRAS  RE,BUMPFLDS         NEXT ACH CODE FIELD                          
         B     FS4LNELP                                                         
*                                                                               
FS4LNEFD DS    0H                  CODE ALREADY ON SCREEN                       
*                                                                               
         LR    R3,R2               SET POINTER TO THIS FIELD                    
*                                                                               
FS4LNEDN DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF NO LINE AVAILABLE ON SCREEN               
         BZ    FS4CONT                GO TO NEXT INPUT CODE                     
*                                                                               
         LR    R2,R3               POINT TO LINE FOR THIS CODE                  
*                                                                               
*        ACH ID TO FIELD ON SCREEN                                              
*                                                                               
         TM    1(R3),X'20'         SKIP IF ADD CHG ID PROTECTED                 
         BO    FS4LP10                                                          
*                                                                               
         MVC   8(L'PACHCODE,R2),PACHCODE PUT CODE ON SCREEN                     
*                                                                               
*        FIND LENGTH OF ID                                                      
*                                                                               
         LA    RF,L'PACHCODE       MAX LENGTH OF CODE                           
         LA    R1,8+L'PACHCODE-1(R2) POINT TO LAST CHARACTER IN CODE            
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK IN CODE                  
         BH    *+10                                                             
         BCTR  R1,0                DECREMENT DATA POINTER                       
         BCT   RF,*-10             CHECK PREVIOUS CHARACTER                     
*                                                                               
         STC   RF,5(R2)            SET SCREEN FIELD LENGTH                      
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
*                                                                               
FS4LP10  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO ADDITIONAL CHARGE DESC FIELD         
         BRAS  RE,CLRFLD           CLEAR OUT FIELD                              
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO ADDITIONAL CHARGE DATA FIELD         
         BRAS  RE,BUMPFLD                                                       
*                                                                               
*        ADDITIONAL CHARGE TO FIELD ON SCREEN                                   
*                                                                               
         MVC   8(L'PACHCHG,R2),PACHCHG  PUT CHARGE ON SCREEN                    
*                                                                               
*        FIND LENGTH OF CHARGE                                                  
*                                                                               
         LA    RF,L'PACHCHG        MAX LENGTH OF CHARGE                         
         LA    R1,8+L'PACHCHG-1(R2) POINT TO LAST CHARACTER IN CHARGE           
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK IN CHARGE                
         BH    *+10                                                             
         BCTR  R1,0                DECREMENT DATA POINTER                       
         BCT   RF,*-10             CHECK PREVIOUS CHARACTER                     
*                                                                               
         STC   RF,5(R2)            SET SCREEN FIELD LENGTH                      
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO COMMISSION Y/N FIELD                 
*                                                                               
*        COMMISSION Y/N TO FIELD ON SCREEN                                      
*                                                                               
         MVC   8(L'PACHCOM,R2),PACHCOM  PUT COMMISSION Y/N TO SCREEN            
*                                                                               
*        FIND LENGTH OF COMMISSION Y/N                                          
*                                                                               
         LA    RF,L'PACHCOM        MAX LENGTH OF CHARGE                         
         LA    R1,8+L'PACHCOM-1(R2) POINT TO LAST CHARACTER IN COM Y/N          
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK IN COM Y/N               
         BH    *+10                                                             
         BCTR  R1,0                DECREMENT DATA POINTER                       
         BCT   RF,*-10             CHECK PREVIOUS CHARACTER                     
*                                                                               
         STC   RF,5(R2)            SET SCREEN FIELD LENGTH                      
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO COMMISSION % FIELD                   
*                                                                               
*        COMMISSION % TO FIELD ON SCREEN                                        
*                                                                               
         MVC   8(L'PACHCOMP,R2),PACHCOMP  PUT COMMISSION % TO SCREEN            
*                                                                               
*        FIND LENGTH OF COMMISSION %                                            
*                                                                               
         LA    RF,L'PACHCOMP       MAX LENGTH OF CHARGE                         
         LA    R1,8+L'PACHCOMP-1(R2) POINT TO LAST CHARACTER IN COM %           
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK IN COM %                 
         BH    *+10                                                             
         BCTR  R1,0                DECREMENT DATA POINTER                       
         BCT   RF,*-10             CHECK PREVIOUS CHARACTER                     
*                                                                               
         STC   RF,5(R2)            SET SCREEN FIELD LENGTH                      
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO CD Y/N FIELD                         
*                                                                               
*        COMMISSION % TO FIELD ON SCREEN                                        
*                                                                               
         MVC   8(L'PACHCD,R2),PACHCD  PUT CD Y/N TO SCREEN                      
*                                                                               
*        FIND LENGTH OF CD Y/N                                                  
*                                                                               
         LA    RF,L'PACHCD         MAX LENGTH OF CD Y/N                         
         LA    R1,8+L'PACHCD-1(R2) POINT TO LAST CHARACTER IN CD Y/N            
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK IN CD Y/N                
         BH    *+10                                                             
         BCTR  R1,0                DECREMENT DATA POINTER                       
         BCT   RF,*-10             CHECK PREVIOUS CHARACTER                     
*                                                                               
         STC   RF,5(R2)            SET SCREEN FIELD LENGTH                      
*                                                                               
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
*                                                                               
FS4CONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PACHLEN        GET RECORD LENGTH                            
         LA    R5,0(RF,R5)         BUMP TO NEXT PBU FIELD                       
         B     FS4LOOP                                                          
*                                                                               
FS4DONE  DS    0H                                                               
*                                                                               
FILSCR4X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069PPBUY00   10/21/19'                                      
         END                                                                    

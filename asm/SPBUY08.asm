*          DATA SET SPBUY08    AT LEVEL 068 AS OF 04/17/19                      
*PHASE T21108C                                                                  
         TITLE 'T21108 - SPOTPAK BUY - DEMO CHANGE FUNCTIONS'                   
T21108   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21108,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21108+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO L     R8,AOVWORK                                                       
         USING LOCALD,R8                                                        
         LR    RE,R8               CLEAR LOCAL STORAGE                          
         LA    RF,L'OVWORK                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   BUDEMSW,0           RESET DEMO RESEQ SWITCH                      
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)            ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         B     CHA1                                                             
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BADKERR  MVI   ERRCD,BADKEYWD                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
CHA1     DS    0H                                                               
         TM    UPSW,UPON+UPCHA     TEST UPLOADING                               
         BO    CHA4                YES-BUY IS ALREADY IN REC                    
*                                                                               
         CLC   =C'LOCK',0(R4)      TEST SPECIAL LOCKIN INPUT                    
         BNE   CHA2                                                             
         BRAS  RE,LOCKIN                                                        
         MVI   BUYINP2H+5,0        IGNORE FURTHER INPUT                         
         B     CHA54               DISPLAY ROUTINES CALLED ALREADY              
*                                                                               
CHA2     MVI   ERRCD,NORECALL                                                   
         L     RE,ADRSVKEY                                                      
         OC    14(4,RE),14(RE)                                                  
         BZ    BUYERR                                                           
         MVC   KEY,0(RE)                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         MVI   ERRCD,NOTFOUND                                                   
         TM    REC+15,X'80'        TEST DELETED                                 
         BO    BUYERR                                                           
*                                                                               
CHA4     MVC   BUSTAT,BDSTAT                                                    
         EJECT                                                                  
*                                                                               
         MVI   ERRCD,TRCDERR                                                    
         CLI   FLEN+1,1            C OR R                                       
         BNE   BUYERR                                                           
*                                                                               
         CLI   FSTOP,C','                                                       
         BE    CHA10                                                            
*                                                                               
* DEMOS CAN BE CHANGED IN DISPLAY AREA IF PREVIOUSLY RECALLED                   
*                                                                               
CHA6     CLI   SVRCLOPT,RCLDEM                                                  
         BNE   *+12                                                             
         BAS   RE,DEMD                                                          
         B     CHA50                                                            
*                                                                               
         CLI   SVRCLOPT,RCLDEMS                                                 
         BNE   *+12                                                             
         BAS   RE,DEMSP                                                         
         B     CHA50                                                            
*                                                                               
         CLI   SVRCLOPT,RCLPDEM    POST BUY DEMOS                               
         BE    *+12                                                             
         CLI   SVRCLOPT,RCLPDEMX                                                
         BNE   CHA10                                                            
*                                                                               
         BAS   RE,POSTBDEM                                                      
         B     CHA50                                                            
         EJECT                                                                  
*=================================================================              
* CHECK FOR A KEYWORD                                                           
*=================================================================              
                                                                                
CHA10    XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,1             MISSING INPUT FLD                            
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         MVI   ERRCD,BADKEYWD                                                   
*                                                                               
         L     R1,=A(CHGTAB)                                                    
         A     R1,RELO                                                          
         USING CHGTABD,R1                                                       
         LA    RE,L'CHGTAB                                                      
         L     RF,=A(CHGTABX)                                                   
         A     RF,RELO                                                          
         BCTR  R5,0                                                             
CHA14    CLC   FLEN+1(1),CHGMINL                                                
         BL    CHA16                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   CHGNAME(0),0(R4) *EXECUTED*                                      
         BE    CHA25                                                            
CHA16    BXLE  R1,RE,CHA14                                                      
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CHA18                                                            
*                                                                               
         CLC   =C'DEM/',0(R4)                                                   
         BNE   *+12                                                             
         BAS   RE,CSPILL                                                        
         B     CHA50                                                            
*                                                                               
         CLC   =C'SPI',0(R4)       CHANGE SPILL MARKETS                         
         BNE   *+12                                                             
         BAS   RE,CSPMKT                                                        
         B     CHA50                                                            
*                                                                               
CHA18    CLC   =C'DEM',0(R4)                                                    
         BNE   *+12                                                             
         BAS   RE,DEMI                                                          
         B     CHA50                                                            
*                                                                               
CHA20    CLC   =C'PBD/',0(R4)                                                   
         BE    CHA22                                                            
         CLC   =C'PBD',0(R4)                                                    
         BNE   CHA24                                                            
*                                                                               
CHA22    BRAS  RE,PBSTR            EDIT POST BUY STRING                         
         B     CHA50                                                            
*                                                                               
CHA24    B     BADKERR                                                          
         EJECT                                                                  
CHA25    MVC   BUCHGOV,CHGOV       SAVE OVLY                                    
         MVC   EDTVAL,CHGEDT       SET EDIT ROUTINE CODE                        
         LR    R7,R1               SAVE A(CHANGE TABLE ENTRY)                   
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   CHA25A              NO                                           
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   CHA26                                                            
         LA    RE,EXP              CODES FOR EXP BUYS                           
         OC    SVNDEF(16),SVNDEF                                                
         BZ    *+8                                                              
         LA    RE,NBUY             CODES FOR NTWK BUYS                          
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    CHGCTL,0                                                         
         BZ    CHA20                                                            
         B     CHA26                                                            
*                                                                               
CHA25A   TM    CHGCTL,CANAD        NOT CANAD AGY - TEST CANAD ONLY              
         BO    CHA20                                                            
*                                                                               
         EJECT                                                                  
CHA26    TM    CHGCTL,NOEDT        TEST IF EDIT NEEDED                          
         BO    CHA30               NO                                           
         GOTO1 CALLEDT             EDIT INPUT DATA                              
*                                                                               
CHA30    DS    0H                                                               
         CLI   BUCHGOV,0           TEST CHANGE LOGIC IN THIS OVLY               
         BE    CHA32               YES                                          
         GOTO1 CALLCHA             GET CHANGE LOGIC                             
*                                                                               
         CLI   DEMLKSW,C'Y'        TEST DEMO LOOK-UP REQ'D                      
         BNE   *+8                                                              
         BAS   RE,LKUP                                                          
         B     CHA50                                                            
*                                                                               
CHA32    DS    0H                                                               
         LR    R1,R7               RESTORE A(CHANGE TABLE ENTRY)                
         SR    RF,RF               GET DATA ROUTINE ADDRESS                     
         ICM   RF,7,CHGROUT                                                     
         GOTO1 (RF),RR=RELO                                                     
         B     CHA50                                                            
         EJECT                                                                  
* DISPLAY LINE                                                                  
*                                                                               
CHA50    DS    0H                                                               
         GOTO1 CALLDSP                                                          
*                                                                               
* MOVE '*' TO FIRST POSITION OF INPUT LINE                                      
*                                                                               
CHA52    C     R2,FLAST            TEST IN DSPLY AREAS                          
         BNH   *+12                NO                                           
         LA    R2,BUYINP1H         YES-POINT TO COMMAND                         
         MVI   BUYINP2H+5,0         AND IGNORE FURTHER INPUT                    
*                                                                               
CHA54    MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
CHA55    CLI   BUYMSG,0                                                         
         BNE   *+10                                                             
         MVC   BUYMSG(22),=C'** ACTION COMPLETED **'                            
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO HANDLE DEMOS IN DISPLAY AREA                                   
*                                                                               
DEMD     NTR1                                                                   
**NOP**  OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
**NOP**  BNZ   DEMDR                                                            
         LA    R2,BUYOUTH          US DEMO DISPLAY                              
         BAS   RE,FNDUF            FIND FIRST INPUT FIELD                       
         LA    R4,8(R2)            RESET FLDVAL TABLE                           
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         LA    R0,1                SET DEMO NUMBER                              
         B     DEMCH                                                            
*                                                                               
DEMDR    B     BADKERR                                                          
         EJECT                                                                  
* CANAD SPILL MKT DEMOS                                                         
*                                                                               
CSPILL   NTR1                                                                   
         LA    R4,4(R4)            POINT TO MARKET NUMBER                       
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BNO   BADKERR                                                          
*                                                                               
         CVB   R0,DUB                                                           
         STH   R0,HALF             SAVE SPILL MARKET NUM                        
* FIND SPILL DEMO ELEM                                                          
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
CSPILL2  BAS   RE,NEXTEL                                                        
         BE    CSPILL4                                                          
         MVI   ERRCD,NOSPLL                                                     
         B     BUYERR                                                           
*                                                                               
CSPILL4  CLC   HALF,4(R6)          TEST RIGHT MARKET                            
         BNE   CSPILL2                                                          
         LA    R0,1                                                             
         MVC   SVSPLMKT,HALF                                                    
         B     DEMCH                                                            
         EJECT                                                                  
* SUB-ROUTINE TO HANDLE SPILL DEMOS IN DISPLAY AREA                             
*                                                                               
DEMSP    NTR1                                                                   
         MVI   ERRCD,BADKEYWD                                                   
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   BUYERR              YES-ERROR                                    
         LA    R7,SVDEMOS          BUILD LIST OF DEMOS IN WORK2                 
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R7,SVBRDEMS                                                      
         XC    WORK2,WORK2                                                      
         LA    R0,8                MAX 8 DEMOS                                  
         LA    R1,WORK2            BUILD DEMO LIST IN WORK2                     
*                                                                               
DEMSP2   CLI   1(R7),0             TEST E-O-L                                   
         BE    DEMSP8                                                           
         CLI   BUYMD,C'R'          RADIO KEEPS ALL DEMOS                        
         BE    DEMSP4                                                           
         CLI   1(R7),C'R'          TEST RATING                                  
         BE    DEMSP4                                                           
         CLI   1(R7),C'E'          TEST EXTENDED RATING                         
         BE    DEMSP4                                                           
*                                                                               
         CLI   1(R7),X'21'         TEST USER DEMO                               
         BNE   DEMSP3              NO                                           
*                                                                               
         LLC   RE,2(R7)            GET USER DEMO SEQNUM                         
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         LA    RE,SVUSRNMS(RE)                                                  
         CLI   0(RE),C'R'                                                       
         BE    DEMSP3X                                                          
         CLI   0(RE),C'E'                                                       
         BE    DEMSP3X                                                          
         B     DEMSP6                                                           
*                                                                               
DEMSP3   CLI   2(R7),0             TEST NONT DEMO                               
         BNE   DEMSP6                                                           
         LLC   RE,1(R7)            GET NONT DEMO SEQNUM                         
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         AHI   RE,SVNTDMS-BUYSAVE                                               
         AR    RE,RA                                                            
*                                                                               
DEMSP3X  CLI   0(RE),C'R'                                                       
         BE    DEMSP4                                                           
         CLI   0(RE),C'E'                                                       
         BNE   DEMSP6                                                           
*                                                                               
DEMSP4   MVC   0(3,R1),0(R7)                                                    
         LA    R1,3(R1)                                                         
         BCT   R0,DEMSP6                                                        
         B     DEMSP8                                                           
*                                                                               
DEMSP6   LA    R7,3(R7)                                                         
         B     DEMSP2                                                           
*                                                                               
DEMSP8   LA    R2,BUYOUTH                                                       
         SR    R0,R0                                                            
*                                                                               
DEMSP10  LR    R1,R2               FIND FIRST UNPROT FIELD                      
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    DEMSP10                                                          
*                                                                               
DEMSP12  PACK  DUB,8(4,R1)         JUST ASSUME MKT IS 4 LONG                    
         CVB   RE,DUB                                                           
         STCM  RE,3,SVSPLMKT       SAVE SPILL MARKET NUMBER                     
         MVI   BUDEMSW,0           RESET SWITCH TO BUILD AGAIN                  
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         GOTO1 VBLDDEM             REBUILD DEMO ELEMENT                         
         LA    R7,WORK2            R7=A(DEMO LIST)                              
         MVI   ERRCD,MAXDEMS                                                    
         CLI   1(R7),0             TEST ANY DEMOS                               
         BE    BUYERR              NO-ERROR                                     
*                                                                               
DEMSP14  LA    R4,8(R2)            RESET FLDVAL TABLE                           
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,DEMEDT                                                    
         MVC   SPDEMTYP,1(R7)      SET DEMO TYPE                                
         GOTO1 CALLEDT             EDIT DEMO INPUT FIELD                        
         OC    FLEN,FLEN           TEST ANY DATA                                
         BZ    DEMSP20             NO-NEXT DEMO                                 
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    DEMSP20             NO-NEXT DEMO                                 
         MVI   ELCDLO,3            FIND THE SPILL DEMO ELEMENT                  
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
*                                                                               
DEMSP16  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NDELEM,R6                                                        
         CLC   SVSPLMKT,NDPROG     MATCH THE SPILL MARKET                       
         BNE   DEMSP16                                                          
         MVI   ERRCD,BADBRDEM                                                   
         ZIC   R0,NDLEN                                                         
         SH    R0,=Y(NDEMNO-NDELEM)                                             
         BNP   BUYERR                                                           
         SRL   R0,3                R0=N'DEMOS IN ELEMENT                        
         LA    R6,NDEMNO           FIND CURRENT DEMO IN THE ELEMENT             
         CLC   0(3,R7),0(R6)                                                    
         BE    DEMSP18                                                          
         LA    R6,8(R6)                                                         
         BCT   R0,*-14                                                          
         B     BUYERR                                                           
*                                                                               
DEMSP18  MVC   4(4,R6),BUNEWDEM    MOVE IN NEW VALUE AND 2DEC FLAG              
         OI    4(R6),X'80'         OVERRIDE                                     
         MVI   3(R6),100           SVI                                          
         CLC   =X'FFFF',BUDEM      TEST NO OVERRIDE                             
         BNE   DEMSP20                                                          
         XC    4(4,R6),4(R6)                                                    
         MVI   DEMLKSW,C'Y'        SET LOOKUP REQD                              
*                                                                               
DEMSP20  LA    R7,3(R7)            NEXT DEMO                                    
         ZIC   R0,0(R2)            NEXT TWA FIELD                               
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   DEMSP22                                                          
         CLI   1(R7),0             YES-TEST END OF DEMOS                        
         BE    DEMSP28                 YES-DONE                                 
         DC    H'0'                    NO-FATAL ERROR                           
*                                                                               
DEMSP22  TM    1(R2),X'20'         TEST PROTECTED FIELD                         
         BO    *+16                                                             
         CLI   0(R2),9             NO-TEST TAB FIELD                            
         BH    DEMSP14                NO-DEMO VALUE                             
         B     DEMSP26                YES                                       
         CLI   1(R7),0             NEXT MARKET                                  
         BE    DEMSP24             TEST END OF DEMOS                            
         DC    H'0'                NO-FATAL ERROR                               
*                                                                               
DEMSP24  LR    R1,R2               R1=A(MKT NUMBER FIELD HEADER)                
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST NEXT FIELD PROTECTED                    
         BO    DEMSP24             YES-SEARCH FOR FIRST UNPROT                  
         CLI   0(R2),9             NO-TEST TAB FIELD                            
         BH    DEMSP12                NO-FIRST DEMO OF NEW MARKET               
*                                                                               
DEMSP26  CLI   1(R7),0             TEST END OF DEMOS                            
         BE    DEMSP28             YES-DONE                                     
         DC    H'0'                NO-FATAL ERROR                               
*                                                                               
DEMSP28  MVI   RCLOPT,RCLDEMS      SET FOR SPILL DEMO DISPLAY                   
         MVI   BUWHY2,X'08'        REASON FOR LAST CHG = DEMO OVERRIDE          
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
*                                                                               
         CLI   DEMLKSW,C'Y'        TEST DEMO LOOK-UP REQ'D                      
         BNE   DEMSP30                                                          
         BRAS  RE,LKUP             LOOKUP AND DO PUTREC                         
         B     EQXIT                                                            
*                                                                               
DEMSP30  BAS   RE,CHGPUT                                                        
         B     EQXIT                                                            
         DROP  R6                                                               
         EJECT                                                                  
* DEM= OR DEMNN=                                                                
*                                                                               
DEMI     NTR1                                                                   
*                                                                               
DEMI1    OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    DEMI2                                                            
         CLC   =C'DEM=NSP',0(R4)                                                
         BNE   DEMIR                                                            
         XC    BUBOOK,BUBOOK                                                    
         BAS   RE,CNET                                                          
         B     EQXIT                                                            
*                                                                               
DEMI2    MVI   EDTVAL,DEMEDT                                                    
         MVI   RCLOPT,RCLDEM                                                    
*                                                                               
         CLI   FLEN+1,3            TEST DEMO NUMBER PRESENT                     
         BNE   *+12                                                             
         LA    R0,1                                                             
         B     DEMCH                                                            
* EDIT DEMO NUMBER                                                              
         CLI   FLEN+1,4            MUST BE AT LEAST 4 CHARS                     
         BL    DEMIR                                                            
         LA    R4,3(R4)            POINT TO DEMO NUMBER                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    DEMIR                                                            
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    DEMIR                                                            
         LA    RE,20               MAXIMUM DEMOS                                
         CR    R0,RE                                                            
         BH    DEMIR                                                            
         B     DEMCH                                                            
*                                                                               
DEMIR    B     BADKERR                                                          
         EJECT                                                                  
* COMMON LOGIC FOR DEMO CHANGE ROUTINES                                         
*                                                                               
DEMCH    XC    SVFLAGS,SVFLAGS                                                  
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JE    DEMCH1A                                                          
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         GOTO1 VBLDDEM             TRY TO BUILD 50 ELEM                         
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   DEMCH1X             IF NOT THERE, NO COMSCORE DEMOS              
*                                                                               
DEMCH1A  BRAS  RE,FIX50EL          ROUTINE SETS SV50EL                          
*                                                                               
DEMCH1X  LA    R7,SVDEMOS          POINT TO APPROPRIATE LIST                    
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R7,SVBRDEMS                                                      
* NOW POSITION TO SPECIFIED ENTRY                                               
         SH    R7,SVEDEMLN         BACK UP ONE ENTRY                            
         BAS   RE,NEXTDEM                                                       
         BNE   BUYERR                                                           
         SPACE 1                                                                
DEMCH2   MVI   EDTVAL,DEMEDT                                                    
         GOTO1 CALLEDT                                                          
         OC    FLEN,FLEN           TEST NO DATA                                 
         BZ    DEMCH18                                                          
         TM    4(R2),X'20'         TEST UNCHANGED                               
         BO    DEMCH18                                                          
* FIND DEMO IN ELEMENT                                                          
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    DEMCH4                                                           
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    DEMCH4                                                           
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   DEMCH6                                                           
DEMCH4   OC    SVSPLMKT,SVSPLMKT                                                
         BNZ   DEMCH8                                                           
*                                                                               
DEMCH6   MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DEMCH12                                                          
*                                                                               
DEMCH8   MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
DEMCH10  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVSPLMKT,4(R6)                                                   
         BNE   DEMCH10                                                          
                                                                                
* MATCH DEMO CODE *                                                             
                                                                                
DEMCH12  LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BZ    DEMCH14A                                                         
         LA    R6,24(R6)                                                        
                                                                                
DEMCH14  SRL   R0,3                SET FOR BCT                                  
*                                                                               
         CLC   0(3,R7),0(R6)                                                    
         BE    DEMCH16                                                          
         LA    R6,8(R6)                                                         
         BCT   R0,*-14                                                          
*                                                                               
DEMCH14A MVI   ERRCD,BADBRDEM                                                   
         B     BUYERR                                                           
*                                                                               
DEMCH16  CLI   2(R6),0             TEST COMSCORE DEMO                           
         JNE   DEMCH17                                                          
         LLC   RE,1(R6)            GET INDEX VALUE                              
         BCTR  RE,0                                                             
         MHI   RE,9                                                             
         A     RE,SV50EL                                                        
         LA    RE,2(RE)            FIRST DEMO IS AT +2                          
*                                                                               
         OC    SVSPLMKT,SVSPLMKT                                                
         JNZ   *+12                                                             
         OI    8(RE),NTDDFDLK      SET FOR NO DEMO LOOKUP                       
         J     DEMCH17                                                          
*                                                                               
         OI    8(RE),NTDDFSDL      SET FOR NO SPILL LOOKUP                      
*                                                                               
DEMCH17  MVC   4(4,R6),BUNEWDEM    MOVE VALUE AND 2DEC FLAG                     
*                                                                               
         L     RE,FADDR            LOOK AT THE INPUT                            
         CLI   0(RE),C'L'          "LOOKED UP" VALUE?                           
         JE    *+8                                                              
         OI    4(R6),X'80'                                                      
*                                                                               
         MVI   3(R6),100                                                        
         CLC   =X'FFFF',BUDEM                                                   
         BNE   DEMCH18                                                          
         XC    4(4,R6),4(R6)                                                    
         MVI   DEMLKSW,C'Y'        SET DEMO LOOK-UP REQD                        
*                                                                               
DEMCH18  C     R2,FLAST            ARE WE IN DSP AREA                           
         BH    DEMCH30             YES                                          
*                                                                               
         CLI   FSTOP,C'/'                                                       
         BNE   DEMCH24                                                          
DEMCH20  DS    0H                                                               
         BAS   RE,NEXTDEM          POINT TO NEXT DEMO                           
         BNE   BUYERR              IF NONE, ERROR                               
         B     DEMCH2              AND CONTINUE                                 
*                                                                               
DEMCH24  CLI   FSTOP,C','          TEST MORE DEM..=                             
         BE    DEMCH28                                                          
*                                                                               
DEMCH26  MVI   RCLOPT,X'07'        SET FOR DEMO DISPLAY                         
         MVI   BUWHY2,X'08'                                                     
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         CLI   DEMLKSW,C'Y'        TEST DEMO LOOK-UP REQ'D                      
         BNE   DEMCH26X                                                         
         BRAS  RE,LKUP             DO LOOKUP AND PUTREC                         
         B     EQXIT                                                            
*                                                                               
DEMCH26X BAS   RE,CHGPUT                                                        
         B     EQXIT                                                            
*                                                                               
DEMCH28  XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         CLC   =C'DEM',0(R4)                                                    
         BE    DEMI1                                                            
         MVI   ERRCD,MULTCHG                                                    
         B     BUYERR                                                           
         SPACE 1                                                                
* GET NEXT DEMO IN DSP AREA *                                                   
         SPACE 1                                                                
DEMCH30  BAS   RE,NEXTDEM          NEXT DEMO                                    
         BNE   DEMCH26             IF NO MORE, DONE                             
*                                                                               
         BAS   RE,FNDUF                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         B     DEMCH2                                                           
         SPACE 2                                                                
NEXTDEM  AH    R7,SVEDEMLN         POINT TO NEXT DEMO                           
         CLI   1(R7),0             TEST FOR A DEMO                              
         BNE   NEXTDEM2            YES - CONTINUE                               
         MVI   ERRCD,MAXDEMS       ELSE SET ERROR MESSAGE                       
         LTR   RE,RE               SET CONDITION CODE NOT =                     
         BR    RE                  AND EXIT                                     
*                                                                               
NEXTDEM2 CLI   1(R7),X'21'         TEST USER DEMO                               
         JE    NEXTDEM3                                                         
         CLI   2(R7),0             TEST NONT DEMO                               
         JE    NEXTDEM4                                                         
         LA    R1,1(R7)            ELSE POINT TO DEMO MODIFIER                  
         J     NEXTDEM5                                                         
*                                                                               
NEXTDEM3 LLC   R1,2(R7)            GET USER DEMO SEQNUM                         
         BCTR  R1,0                                                             
         MHI   R1,7                                                             
         LA    R1,SVUSRNMS(R1)                                                  
         J     NEXTDEM5                                                         
*                                                                               
NEXTDEM4 LLC   R1,1(R7)            GET NONT DEMO SEQNUM                         
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         AHI   R1,SVNTDMS-BUYSAVE                                               
         AR    R1,RA                                                            
*                                                                               
NEXTDEM5 MVC   SPDEMTYP,0(R1)      SAVE DEMO TYPE                               
         OC    SVSPLMKT,SVSPLMKT   TEST SPILL MKT                               
         BZR   RE                  NO, EXIT WITH CC =                           
         CLI   BUYMD,C'R'          RADIO KEEPS ALL DEMOS                        
         BER   RE                                                               
         CLI   0(R1),C'R'          TEST RATING                                  
         BER   RE                                                               
         CLI   0(R1),C'E'          TEST EXTENDED RATING                         
         BER   RE                                                               
         B     NEXTDEM             EXIT WITH CC NOT EQ                          
         EJECT                                                                  
*=====================================================================          
* SUB-ROUTINE FOR PERCENTAGE CHANGE - FORMAT IS DEMPCT= OR DEM%= +10            
*                                            OR DEMALL=                         
* DEMOS WITH OVERRIDES WILL NOT BE CHANGED UNLESS                               
* 1. AN UPGRADE FORMULA IS PRESENT                                              
* 2. DEMALL= IS ENTERED                                                         
* 3. ALL DEMOS ALREADY HAVE OVERRIDES                                           
*=====================================================================          
         SPACE 1                                                                
DP       NTR1                                                                   
         MVI   BUXSW,0             CLEAR ALL DEMO CHANGE FLAG                   
         CLC   =C'ALL',3(R4)       TEST INPUT IS DEMALL                         
         BNE   *+8                                                              
         MVI   BUXSW,C'A'                                                       
* IF UPGRADE ELEM PRESENT, SET TO CHANGE ALL DEMOS                              
         MVI   ELCDLO,X'62'                                                     
         MVI   ELCDHI,X'62'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         MVI   BUXSW,C'U'                                                       
* CHECK IF ALL DEMOS HAVE OVERRIDES                                             
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   DP1X                                                             
*                                                                               
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         AHI   R7,-24                                                           
         BNP   DP1X                                                             
         LA    R6,24(R6)                                                        
         SRL   R7,3                SET FOR BCT                                  
*                                                                               
DP1A     TM    4(R6),X'80'                                                      
         BZ    DP1X                                                             
         LA    R6,8(R6)                                                         
         BCT   R7,DP1A                                                          
         MVI   BUXSW,C'V'          SET ALL HAVE OVERRIDES                       
*                                                                               
DP1X     LA    R4,2(R4,R5)         POINT TO DATA                                
         MVI   ERRCD,BADPCTCH                                                   
         CLI   0(R4),C'+'                                                       
         BE    *+12                                                             
         CLI   0(R4),C'-'                                                       
         BNE   DPR                                                              
         MVC   BUDEM,0(R4)         SAVE SIGN OF CHANGE                          
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    DPR                                                              
         CLI   FLEN+1,2                                                         
         BH    DPR                                                              
         TM    FVAL,X'08'                                                       
         BZ    DPR                                                              
         CVB   R0,DUB              SAVE PERCENT CHANGE                          
         ST    R0,FULL                                                          
*                                                                               
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    DP2                                                              
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    DP2                                                              
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   DP4                                                              
DP2      OC    SVSPLMKT,SVSPLMKT                                                
         BNZ   DP6                                                              
*                                                                               
DP4      MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DP10                                                             
*                                                                               
DP6      MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
DP8      BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVSPLMKT,4(R6)                                                   
         BNE   DP8                                                              
*                                                                               
DP10     ZIC   R7,1(R6)                                                         
         AHI   R7,-24                                                           
         BNP   DP20                                                             
         LA    R6,24(R6)                                                        
         SPACE 1                                                                
* CONVERTED DEMO PROCESSING *                                                   
         SPACE 1                                                                
DP12     SRL   R7,3                SET FOR BCT                                  
*                                                                               
DP14     CLI   BUXSW,0             TEST TO ADJUST ALL DEMOS                     
         BNE   *+12                NON-ZERO MEANS YES                           
         TM    4(R6),X'80'         ELSE TEST EXISTING OVERRIDE                  
         BO    DP16                                                             
*                                                                               
         NI    4(R6),X'7F'         DROP OVRD BIT                                
         ICM   RF,15,4(R6)                                                      
         M     RE,FULL                                                          
         AHI   RF,50                                                            
         D     RE,=F'100'                                                       
         CLI   BUDEM,C'+'                                                       
         BE    *+6                                                              
         LCR   RF,RF                                                            
         A     RF,4(R6)            ADD TO ORIGINAL DEMO                         
         ST    RF,4(R6)                                                         
         OI    4(R6),X'80'                                                      
         MVI   3(R6),100                                                        
DP16     LA    R6,8(R6)                                                         
         BCT   R7,DP14                                                          
*                                                                               
DP20     MVI   BUWHY2,X'08'                                                     
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         BAS   RE,CHGPUT                                                        
         MVI   RCLOPT,RCLDEM                                                    
         B     EQXIT                                                            
*                                                                               
DPR      B     BUYERR                                                           
         EJECT                                                                  
* SUB-ROUTINE TO LOOK UP DEMOS AND UPDATE FILE                                  
*                                                                               
LKUP     NTR1                                                                   
         CLI   DEMLKSW,0           TEST DEMO LOOK-UP REQ'D                      
         BE    LKUP4               NO                                           
*                                                                               
         LHI   RE,SVD0PROF-BUYSAVE ELSE CHECK D0 PROF                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    LKUP2                                                            
*                                                                               
         AR    RE,RA                                                            
         CLI   13(RE),C'Y'                                                      
         JE    LKUP2                                                            
         CLI   13(RE),C'B'         TEST LOOK UP NETWORK DEMOS                   
         JNE   LKUP4                                                            
*                                                                               
LKUP2    MVI   BUWHY,X'02'         SET DEMO CHANGE                              
         GOTO1 DEMLKUP                                                          
*                                                                               
LKUP4    DS    0H                                                               
         BAS   RE,UPDREQ                                                        
*                                                                               
LKUPX    B     EQXIT                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE BUY AND BUILD REQUEST PRD LIST                          
*                                                                               
UPDREQ   NTR1                                                                   
         BAS   RE,CHGPUT                                                        
         GOTO1 VBLDQLST            GENERATE REQUEST PRD LIST                    
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE FOR CANAD NTWK                                                    
*                                                                               
CNET     NTR1                                                                   
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
* UPDATE NTWK BUY                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(2,R6),BUBOOK                                                   
         MVI   BUWHY,X'42'                                                      
         GOTO1 SETCHGDT                                                         
         OC    BUBOOK,BUBOOK       TEST DEM=NSP                                 
         BZ    CNET2                                                            
         BAS   RE,PUTIT                                                         
* READ EXPLODED BUYS                                                            
CNET2    L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         LR    R7,R6                                                            
*                                                                               
CNET4    LR    R6,R7               RESTORE EL POINTER                           
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CNET12                                                           
         LR    R7,R6               SAVE EL POINTER                              
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* RESET DEMO OVERRIDES                                                          
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,2                                                         
         OC    BUBOOK,BUBOOK       TEST DEM=NSP                                 
         BNZ   *+8                                                              
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
CNET6    BAS   RE,NEXTEL                                                        
         BNE   CNET10                                                           
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'24'                                                        
         BNP   CNET10                                                           
         LA    RE,24(R6)                                                        
*                                                                               
CNET8    SRL   R0,3                                                             
*                                                                               
CNET9    XC    4(4,RE),4(RE)                                                    
         OC    BUBOOK,BUBOOK                                                    
         BNZ   *+8                                                              
         OI    4(RE),X'80'                                                      
         LA    RE,8(RE)                                                         
         BCT   R0,CNET9                                                         
         B     CNET6                                                            
*                                                                               
CNET10   GOTO1 DEMLKUP                                                          
         MVI   BUWHY,X'42'                                                      
         BAS   RE,CHGPUT                                                        
         B     CNET4                                                            
*                                                                               
CNET12   MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
         B     CHA50                                                            
         EJECT                                                                  
*=================================================================              
* SUB-ROUTINE TO HANDLE POST BUY DEMOS IN DISPLAY AREA                          
*=================================================================              
                                                                                
POSTBDEM NTR1                                                                   
         MVI   ERRCD,BADKEYWD                                                   
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   PDERR               YES-ERROR                                    
         CLI   SVAPROF+6,C'Y'      TEST -S AUTH REQUIRED                        
         BNE   PD0                                                              
         TM    T211FFD+12,X'80'    TEST BIT ON                                  
         BO    PD0                                                              
         MVI   ERRCD,NOFNACCS                                                   
         B     PDERR                                                            
*                                                                               
PD0      LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         CLI   SVRCLOPT,RCLPDEMX   TEST 2ND SET OF DEMOS                        
         BNE   *+8                                                              
         LA    R4,24(R4)           YES-START AT 9TH DEMO                        
         ST    R4,SAVER4           R4=A(DEMO LIST)                              
         LR    R1,R4                                                            
         LA    R5,8                                                             
         CLI   1(R1),0                                                          
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         BCT   R5,*-12                                                          
         AHI   R5,-8                                                            
         LPR   R5,R5                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         ST    R5,SAVER5           R5=N'DEMOS                                   
*                                                                               
         LA    R2,BUYOUTH                                                       
         SR    R0,R0               FIND FIRST UNPROTECTED FIELD                 
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         XC    SVSPLMKT,SVSPLMKT   ORIGINAL MARKET FIRST                        
         SR    R6,R6                                                            
         B     PD4                                                              
*                                                                               
PD2      PACK  DUB,8(4,R2)         GET SPILL MARKET NUMBER                      
         CVB   RE,DUB                                                           
         STCM  RE,3,SVSPLMKT       SAVE SPILL MARKET NUMBER                     
         SR    R6,R6                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2=A(FIRST DEMO VALUE FLD HDR)               
*                                                                               
PD4      MVI   BUDEMSW,0           FORCE REBUILD                                
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         GOTO1 VBLDDEM             REBUILD DEMO ELEMENT                         
         LM    R4,R5,SAVER4        R4=A(DEMO LIST),R5=N'DEMOS                   
         MVI   FIRSTDEM,C'Y'                                                    
*                                                                               
PD6      TM    1(R2),X'20'         TEST PROTECTED FIELD                         
         BO    PD8                 YES-NEXT DEMO                                
         CLI   0(R2),9             TEST HIT TAB FIELD                           
         BNH   PD9                                                              
         LA    R1,8(R2)            RESET FLDVAL TABLE                           
         ST    R1,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,DEMEDT                                                    
         MVC   SPDEMTYP,1(R4)      SET DEMO TYPE FOR EDIT ROUTINES              
         GOTO1 CALLEDT             EDIT DEMO INPUT FIELD                        
         OC    FLEN,FLEN           TEST ANY DATA                                
         BZ    PD8                 NO-NEXT DEMO                                 
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    PD8                 NO-NEXT DEMO                                 
         LTR   R6,R6               TEST DEMO ELEMENTS FOUND YET                 
         BNZ   *+8                                                              
         BAS   RE,PDELS            NO-GET THEM                                  
         BAS   RE,PDVAL            ALTER DEMO VALUE IN POST BUY DEMO EL         
*                                                                               
         TM    SVAFLAG2,AGYFLAG2_PBD TEST POST BUY ADJ OPTION                   
         BZ    PD8                                                              
         CLI   FIRSTDEM,C'Y'                                                    
         BNE   PD8                                                              
         BRAS  RE,ADJPBD           GO ADJUST VALUES                             
*                                                                               
PD8      MVI   FIRSTDEM,C'N'       ONLY ADJUST WHEN DEMO1 CHANGES               
         LA    R4,3(R4)            NEXT DEMO                                    
         SR    R0,R0                                                            
         IC    R0,0(R2)            NEXT TWA FIELD                               
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST END OF SCREEN                           
         BE    PD9                                                              
         BCT   R5,PD6                                                           
*                                  END OF DEMOS -                               
PD9      LTR   R6,R6               TEST ANY POST BUY DEMOS ENTERED              
         BZ    PD12                                                             
         LA    R1,ELEM             YES-TEST NEW POST BUY ELEMENT                
         CR    R1,R6                                                            
         BNE   PD10                WE MODIFIED EXISTING PBDELEM                 
         ZIC   R0,1(R7)            YES-ADD IT AFTER REGULAR DEMO ELE            
         AR    R7,R0                                                            
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R7)                                     
         B     PD12                                                             
*                                                                               
PD10     SR    RE,RE               SEE IF ANY VALUES LEFT IN PBDELEM            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    2(0,R6),2(R6)                                                    
         BNZ   PD12                                                             
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  REMOVE EMPTY PBDELEM                    
*                                                                               
PD12     CLI   0(R2),0             TEST END OF SCREEN                           
         BE    PD20                YES-DONE                                     
         TM    1(R2),X'20'         TEST FIELD IS PROTECTED                      
         BO    PD2                 YES-IT'S NEXT MARKET                         
         CLI   0(R2),9             TEST TAB FIELD                               
         BNH   PD20                YES-DONE                                     
         DC    H'0'                FATAL ERROR                                  
*                                                                               
PD20     MVC   RCLOPT,SVRCLOPT     SET FOR DISPLAY                              
         MVI   BUWHY2,X'08'        REASON FOR LAST CHG = DEMO OVERRIDE          
         GOTO1 SETCHGDT                                                         
         GOTO1 PUTREC              GO UPDATE FILE                               
*                                                                               
PDX      B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*=================================================================              
* SUB-ROUTINE TO HANDLE POST BUY DEMOS INPUT STRING                             
* ON ENTRY R4 POINTS TO PBD= OR PBD/1234=                                       
*=================================================================              
                                                                                
PBSTR    NTR1                                                                   
         XC    SVSPLMKT,SVSPLMKT                                                
         CLC   =C'PBD/',0(R4)      TEST SPILL INPUT                             
         BE    PBS2                YES                                          
         AHI   R4,4                POINT TO DEMO INPUT                          
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         B     PBS4                                                             
                                                                                
* VALIDATE SPILL MARKET NUMBER                                                  
                                                                                
PBS2     LA    R4,4(R4)            POINT TO MARKET NUMBER                       
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADKEYWD                                                   
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    PBSERR                                                           
         CLI   FSTOP,C'='                                                       
         BNE   PBSERR                                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,SVSPLMKT       SAVE SPILL MARKET NUM                        
*                                                                               
PBS4     MVI   BUDEMSW,0           RESET SWITCH                                 
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         GOTO1 VBLDDEM             REBUILD DEMO ELEMENT                         
         BAS   RE,PDELS            R7 TO DEMO ELEM, R6 TO PBD ELEM              
*                                                                               
         CLI   SVAPROF+6,C'Y'      TEST -S AUTH REQUIRED                        
         BNE   PBS8                                                             
         TM    T211FFD+12,X'80'    TEST BIT ON                                  
         BO    PBS8                                                             
         MVI   ERRCD,NOFNACCS                                                   
         B     PBSERR                                                           
*                                                                               
PBS8     LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         MVI   FIRSTDEM,C'Y'                                                    
*                                                                               
PBS10    OC    SVSPLMKT,SVSPLMKT  TEST SPILL INPUT                              
         BZ    PBS10X                                                           
         CLI   BUYMD,C'R'          RADIO HAS RTGS AND IMPS                      
         BE    PBS10X                                                           
*                                                                               
         CLI   1(R4),C'R'          TEST RATING                                  
         BE    PBS10X                                                           
         CLI   1(R4),C'E'          TEST EXTENDED RATING                         
         BE    PBS10X                                                           
*                                                                               
         CLI   1(R4),X'21'         TEST USER DEMO                               
         BNE   PBS10A              NO                                           
         LLC   RF,2(R4)            GET DEMO NUMBER                              
         BCTR  RF,0                                                             
         MHI   RF,7                                                             
         LA    RF,SVUSRNMS(RF)     RF = A(USER DEMO NAME)                       
         B     PBS10B                                                           
*                                                                               
PBS10A   CLI   2(R4),0             TEST NONT DEMO                               
         BNE   PBS14               NO                                           
         LLC   RF,1(R4)            GET NONT DEMO SEQNUM                         
         BCTR  RF,0                                                             
         SLL   RF,3                X 8                                          
         LAY   RF,SVNTDMS(RF)      RF = A(NT DEMO NAME)                         
*                                                                               
PBS10B   CLI   0(RF),C'R'          RATING?                                      
         BE    PBS10X                                                           
         CLI   0(RF),C'E'          OR EXTENDED?                                 
         BNE   PBS14                                                            
*                                                                               
PBS10X   MVC   SPDEMTYP,1(R4)      SET DEMO TYPE                                
         MVI   EDTVAL,DEMEDT                                                    
         GOTO1 CALLEDT             EDIT DEMO INPUT FIELD                        
         OC    FLEN,FLEN           TEST ANY DATA                                
         BZ    PBS12               NO-NEXT DEMO                                 
*                                                                               
         BAS   RE,PDVAL            SET DEMO VALUE IN POST BUY DEMO EL           
*                                                                               
         TM    SVAFLAG2,AGYFLAG2_PBD  TEST POST BUY ADJ OPTION                  
         BZ    PBS12                                                            
         CLI   FIRSTDEM,C'Y'          TEST FIRST DEMO                           
         BNE   PBS12                                                            
         BRAS  RE,ADJPBD              GO ADJUST VALUES                          
*                                                                               
PBS12    MVI   FIRSTDEM,C'N'                                                    
         CLI   FSTOP,C'/'          ANY MORE DEMOS                               
         BNE   PBS20                                                            
*                                                                               
PBS14    LA    R4,3(R4)            NEXT DEMO                                    
         OC    0(3,R4),0(R4)                                                    
         BNZ   PBS10                                                            
         MVI   ERRCD,DEMERR                                                     
         B     PBSERR                                                           
*                                                                               
PBS20    CLI   ELEM,0              TEST NEW POST BUY ELEMENT                    
         BE    PBS22               NO - WE MODIFIED EXISTING                    
         SR    R0,R0                                                            
         IC    R0,1(R7)            ADD IT AFTER REGULAR DEMO ELE                
         AR    R7,R0                                                            
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R7)                                     
         B     PBS24                                                            
*                                                                               
PBS22    SR    RE,RE               SEE IF ANY VALUES LEFT IN PBDELEM            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    2(0,R6),2(R6)                                                    
         BNZ   PBS24                                                            
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  REMOVE EMPTY PBDELEM                    
*                                                                               
PBS24    MVC   RCLOPT,SVRCLOPT     SET FOR DISPLAY                              
         MVI   BUWHY2,X'08'        REASON FOR LAST CHG = DEMO OVERRIDE          
         GOTO1 SETCHGDT                                                         
         GOTO1 PUTREC              GO UPDATE FILE                               
*                                                                               
PBSX     B     EXIT                                                             
*                                                                               
PBSERR   GOTO1 ERROR                                                            
         EJECT                                                                  
PDELS    ST    RE,FULL             GET THE DEMO ELEMENTS                        
         XC    ELEM,ELEM           CLEAR NEW ELEMENT AREA                       
         LA    R6,BDELEM           ON EXIT, R6=A(POST BUY DEMO ELE)             
         USING NDELEM,R6                    R7=A(DEMO ELE)                      
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         OC    SVSPLMKT,SVSPLMKT                                                
         BZ    PDELS2                                                           
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
PDELS2   BAS   RE,NEXTEL           GET DEMO ELEM                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),3                                                          
         BNE   *+14                                                             
         CLC   SVSPLMKT,NDPROG                                                  
         BNE   PDELS2                                                           
         DROP  R6                                                               
*                                                                               
         MVC   SVDEMO1,24+4(R6)    SAVE VALUE OF FIRST DEMO                     
*                                                                               
         LR    R7,R6                                                            
         USING NDELEM,R7                                                        
         LA    R6,BDELEM           GET POST BUY DEMO ELEMENT                    
         MVI   ELCDLO,X'22'                                                     
         MVI   ELCDHI,X'22'                                                     
         CLI   NDCODE,3                                                         
         BNE   PDELS4                                                           
         MVI   ELCDLO,X'23'                                                     
         MVI   ELCDHI,X'23'                                                     
*                                                                               
PDELS4   BAS   RE,NEXTEL                                                        
         BNE   PDELS6                                                           
         CLI   NDCODE,3                                                         
         BNE   PDELSX                                                           
         CLC   SVSPLMKT,SDAGYMKT-SDELEM(R6)                                     
         BNE   PDELS4                                                           
         B     PDELSX                                                           
*                                  POST BUY DEMO ELEM NOT FOUND                 
PDELS6   LA    R6,ELEM                                                          
         MVC   0(1,R6),ELCDLO      BUILD NEW ONE IN ELEM                        
         SR    R1,R1                                                            
         LA    RE,SVDEMOS                                                       
         CLI   1(RE),0             COUNT TOTAL N'DEMOS                          
         BE    *+16                                                             
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         B     *-16                                                             
*                                                                               
         LA    RE,PDEMO-PDELEM(R1) ELEMENT LENGTH                               
         OC    SVSPLMKT,SVSPLMKT   TEST SPILL                                   
         BZ    *+14                                                             
         LA    RE,SDEMO-SDELEM(R1)                                              
         MVC   SDAGYMKT-SDELEM(4,R6),NDPROG   AGY & RTG SVC MKTS                
         STC   RE,1(R6)                                                         
*                                                                               
PDELSX   L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
PDERR    GOTO1 ERROR                                                            
         SPACE 2                                                                
PDVAL    NTR1                                                                   
         USING NDELEM,R7                                                        
         SR    R1,R1                                                            
         IC    R1,NDLEN                                                         
         AHI   R1,-24                                                           
         BP    *+6                                                              
         DC    H'0'                                                             
         SRL   R1,3                R1=N'DEMOS IN DEMO ELEMENT                   
*                                                                               
         LA    R3,NDEMNO                                                        
         LA    R5,PDEMO-PDELEM(R6) SET REGS FOR BXLE                            
         CLI   NDCODE,3                                                         
         BNE   *+8                                                              
         LA    R5,SDEMO-SDELEM(R6)                                              
*                                                                               
         LHI   RE,3                                                             
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    RF,R6                                                            
         BCTR  RF,0                                                             
*                                                                               
PDVAL2   CLC   0(3,R4),0(R3)       FIND DEMO IN DEMO ELEMENT                    
         BE    PDVAL4                                                           
         LA    R3,8(R3)                                                         
         BCT   R1,*+8                                                           
         B     *+8                                                              
         BXLE  R5,RE,PDVAL2                                                     
         MVI   ERRCD,DEMERR                                                     
         B     BUYERR                                                           
*                                                                               
PDVAL4   XC    0(3,R5),0(R5)                                                    
         CLC   =X'FFFF',BUDEM      TEST NO OVERRIDE INDICATOR                   
         BE    PDX                                                              
         MVC   0(3,R5),BUNEWDEM+1  MOVE IN NEW VALUE                            
         OI    0(R5),X'80'         SET OVRD                                     
         OC    0(1,R5),BUNEWDEM    SET 2DEC FLAGS                               
         B     PDX                                                              
         DROP  R7                                                               
         EJECT                                                                  
ADDEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         B     EXIT                                                             
*                                                                               
DELEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     EXIT                                                             
*                                                                               
CHGPUT   NTR1                                                                   
         GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT            GO UPDATE FILE                               
         B     EQXIT                                                            
*                                                                               
PUTIT    LR    R0,RE                                                            
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+10                NO                                           
         CLI   CMPASS,1                                                         
         BER   RE                                                               
         TM    UPSW,UPON+UPCHA     TEST UPLOADING                               
         BO    PUTIT2              YES-DON'T WRITE THE RECORD                   
         GOTO1 PUTREC                                                           
*                                                                               
PUTIT2   TM    BUWHY,X'70'         TEST TYPE OF CHANGE                          
         BZ    *+8                                                              
         OI    SVUPDATE,X'40'      SET BUY CHANGE FLAG                          
         TM    BUWHY3,X'80'        TEST SKED CHANGE                             
         BZ    *+8                                                              
         OI    SVUPDATE,X'40'      SET BUY CHANGE FLAG                          
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
FNDUF    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    FNDUFX                                                           
         TM    1(R2),X'20'                                                      
         BO    FNDUF                                                            
         CLI   0(R2),9                                                          
         BNH   FNDUF                                                            
         CR    RB,RB               EXIT WITH CC =                               
         BR    RE                                                               
FNDUFX   LTR   RB,RB               EXIT WITH CC NOT =                           
         BR    RE                                                               
         EJECT                                                                  
OVLY00   EQU   X'00'                                                            
*                                                                               
CHGTAB   DS    0XL(CHGTABL)                                                     
*                                                                               
         DC    CL7'DEMPCT',AL1(6),AL1(OVLY00),AL1(0)                            
         DC    AL1(NOEDT+EXP),AL3(DP),AL3(0)                                    
*                                                                               
         DC    CL7'DEMALL',AL1(6),AL1(OVLY00),AL1(0)                            
         DC    AL1(NOEDT+EXP),AL3(DP),AL3(0)                                    
*                                                                               
CHGTABX  EQU   *-1                                                              
         EJECT                                                                  
*                                                                               
DEMENT   DC    CL7'DEM    ',AL1(3),AL1(OVLY00),AL1(DEMEDT)                      
         DC    AL1(NOEDT+NBUY+EXP),AL3(DEMI),AL3(0)                             
*                                                                               
         EJECT                                                                  
*                                                                               
*        CHANGE MAKE UP OF SPILL MARKETS                                        
*                                                                               
CSPMKT   NTR1  LABEL=*                                                          
*                                                                               
         LA    R7,BUYMSTA          POINT TO MARKET/STATION                      
*                                                                               
         GOTO1 VGETSPLL            GET NEW SET OF SPILL ELEMENTS                
*                                                                               
         GOTO1 DEMLKUP             LOOK UP SPILL FOR NEW MARKETS                
*                                                                               
*        ADD POOL SPILL PASSIVE POINTERS                                        
*                                                                               
*        FIND SPILL DEMO ELEM                                                   
*                                                                               
         MVI   ELCDLO,3            LOOKING FOR X'03' DEMO ELEMENT               
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
CSPMKTLP DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CSPMKTDN                                                         
*                                                                               
         USING NDELEM,R6           ESTABLISH DEMO ELEMENT                       
*                                                                               
* ADD SPILL PASSIVE FOR POOL                                                    
*                                                                               
         MVC   KEY,SVKEY           MOVE KEY INCLUDING DISK ADDRESS              
         MVC   KEY+4(2),NDPROG     REPLACE MKT WITH AGY SPILL MKT               
         MVI   KEY+10,X'80'        SET SPILL INDICATOR                          
         MVC   KEY+11(1),BUYKEY+10 MOVE LINE NUMBER                             
         MVI   KEY+12,1                                                         
         CLI   SV1OR2,2                                                         
         BNE   *+10                                                             
         MVC   KEY+11(2),BUYKEY+10       MOVE 2-BYTE LINE NUMBER                
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                       
*                                                                               
         TM    8(R1),X'FF'-X'20'   TEST ALL ERRORS BUT DUP KEY                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* REMEMBER MEDIA C TOO FOR CANADA                                               
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   CSPMKTCN                                                         
*                                                                               
         CLI   BUYMD,C'T'          TEST TV                                      
         BE    *+12                                                             
         CLI   BUYMD,C'N'          OR NETWORK TV                                
         BNE   CSPMKTCN                                                         
*                                                                               
         NI    KEY,X'F0'                                                        
         OI    KEY,X'08'                                                        
         GOTO1 (RF),(R1)                                                        
*                                                                               
         TM    8(R1),X'FF'-X'20'   TEST ALL ERRORS BUT DUP KEY                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CSPMKTCN DS    0H                                                               
*                                                                               
         B     CSPMKTLP                                                         
*                                                                               
CSPMKTDN DS    0H                                                               
*                                                                               
*                                                                               
* ADD PRODUCTS TO PRDLIST FOR PUTREC TO ADD PASSIVE SPILL PTRS                  
*                                                                               
         XC    PRDLIST,PRDLIST     INIT PRODUCT LIST                            
*                                                                               
         LA    RF,BDMASPRD         FIRST MASPRD                                 
         BAS   RE,ADDPRDL                                                       
*                                                                               
         LA    RF,1(RF)            SECOND MASPRD                                
         BAS   RE,ADDPRDL                                                       
*                                                                               
         MVI   ELCDLO,X'0B'        LOOKING FOR BUY ELEMENTS                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
CSPPRDLP DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CSPPRDDN                                                         
*                                                                               
         USING REGELEM,R6          ESTABLISH REGULAR POOL BUY ELEMENT           
*                                                                               
* ADD PRODUCT(S) TO PRDLIST                                                     
*                                                                               
         CLI   RLEN,14             SKIP IF NO PRODUCTS ALLOCATED                
         BL    CSPPRDCN                                                         
         LA    RF,10(R6)           POINT TO FIRST PRODUCT                       
         BAS   RE,ADDPRDL                                                       
*                                                                               
         CLI   RLEN,18             SKIP IF NO PIGGYBACK ALLOCATED               
         BL    CSPPRDCN                                                         
*                                                                               
         LA    RF,14(R6)           POINT TO SECOND PRODUCT                      
         BAS   RE,ADDPRDL                                                       
         B     CSPPRDCN                                                         
         SPACE 1                                                                
*================================================================*              
*        ADD PRODUCT IF NOT ALREADY IN LIST                                     
*        RF==> PRODUCT TO ADD TO LIST                                           
*================================================================*              
         SPACE 1                                                                
ADDPRDL  CLI   0(RF),0             TEST NO PRODUCT                              
         BER   RE                  YES - IGNORE                                 
         LA    R1,PRDLIST          INIT PRODUCT LIST                            
*                                                                               
ADDPRDL2 CLI   0(R1),0             DONE AT END OF LIST                          
         BE    ADDPRDL4                                                         
         CLC   0(1,R1),0(RF)       SKIP IF PRODUCT IN LIST                      
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     ADDPRDL2                                                         
*                                                                               
ADDPRDL4 MVC   0(1,R1),0(RF)       ADD PRODUCT TO LIST                          
         BR    RE                                                               
*                                                                               
CSPPRDCN DS    0H                                                               
         B     CSPPRDLP                                                         
*                                                                               
CSPPRDDN DS    0H                                                               
*                                                                               
         OC    PRDLIST,PRDLIST     SKIP IF NO PRODUCTS IN LIST                  
         BZ    CSPPRDX                                                          
*                                                                               
         LA    RE,PRDLIST          PASS A(PRDLIST) WITH PUTREC                  
         ST    RE,DMCB+20                                                       
*                                                                               
CSPPRDX  DS    0H                                                               
*                                                                               
         MVC   KEY,SVKEY           RESTORE BUY RECORD KEY                       
         GOTOR HIGH                RE-READ FILE POINTER                         
*                                                                               
         ICM   R0,15,AREC          SAVE CURRENT IOAREA POINTER                  
         MVC   AREC,AREC2          READ BUYREC INTO REC2                        
         GOTOR GETREC              RESETS FILE POINTERS FOR UPDATE              
         STCM  R0,15,AREC          RESTORE CURRENT IOAREA POINTER               
*                                                                               
         MVI   RCLOPT,RCLSPILL     DISPLAY SPILL LIST                           
*                                                                               
         MVI   BUWHY,X'02'         LAST CHG = DEMO LOOK UP/CHANGE               
*                                                                               
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
*                                                                               
         BAS   RE,CHGPUT           RE-WRITE RECORD                              
*                                                                               
CSPMKTX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SAVE FLAGS FROM NONT (X'50') ELEMENT AND REBUILD IT                           
* THEN MOVE SAVED FLAGS TO NEW ELEMENT                                          
* ON ENTRY R6 POINTS TO X'50' ELEMENT                                           
*===============================================================                
                                                                                
FIX50EL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LLC   R0,1(R6)            GET ELEM LEN                                 
         SRDL  R0,32                                                            
         D     R0,=F'9'                                                         
         LTR   R0,R1               SET FOR BCT                                  
         JZ    EXIT                                                             
         LA    R1,2(R6)            POINT TO FIRST DEMO                          
         LA    RE,SVFLAGS                                                       
*                                                                               
FIX50EL2 MVC   0(1,RE),8(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,9(R1)                                                         
         JCT   R0,FIX50EL2                                                      
                                                                                
* NOW CALL BASE TO REBUILD DEMO ELEMENT TO INSURE CURRENT DEMOS                 
                                                                                
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         GOTO1 VBLDDEM                                                          
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         ST    R6,SV50EL                                                        
*                                                                               
         LLC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'9'                                                         
         LTR   R0,R1                                                            
         JZ    EXIT                                                             
         LA    R1,2(R6)            FIRST DEMO IN NEW ELEM                       
         LA    RE,SVFLAGS                                                       
*                                                                               
FIX50EL4 MVC   8(0,R1),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,9(R1)                                                         
         JCT   R0,FIX50EL4                                                      
         J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* SPECIAL FOR LCI TO INPUT DOLLARS OR DEMOS INTO LOCKIN RECORD                  
* INPUT LINE FROM SCRIPT SHOULD LOOK LIKE                                       
* LOCK=DPT,SLN,BRDMON,DOLLARS,DEMO1,DEMO2,DEMO3,DEMO4                           
*==============================================================                 
                                                                                
LOCKIN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,AREC1                                                       
         MVI   FSTOPS+1,C'='                                                    
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         GOTO1 FLDVAL                                                           
         MVI   FSTOPS+1,0                                                       
*                                                                               
         XC    ELEM,ELEM           CLEAR DOLLARS/SPOTS                          
E        USING LOKEL,ELEM                                                       
         MVI   E.LOKEL,X'03'                                                    
         MVI   E.LOKELLN,LOKELLNQ                                               
*                                                                               
         MVI   FSTOPS,C','                                                      
         CLI   5(R2),5             TEST FOR INPUT STRING OF LOCK=               
         BE    LOCKIN20            WHICH JUST DISPLAYS                          
* BUILD DATE TABLE OF BDCST MONTHS FROM START DATE                              
         L     R4,AREC2                                                         
         XC    0(128,R4),0(R4)                                                  
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'  GETBROAD                             
         XC    WORK,WORK                   WORK+20=DUMMY PROFILE                
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(4),VADDAY                                                 
         MVC   WORK+8(4),VGETDAY                                                
         MVC   WORK+12(4),VDATCON                                               
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A72'  GET ADDRESS OF MOBILE                
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(13,SVSTART),(0,AREC2),WORK,WORK+20                    
* REPLACE 2-BYTE MONTH END DATE WITH BINARY Y/M                                 
         L     R4,AREC2                                                         
*                                                                               
LOCKIN1  GOTO1 VDATCON,DMCB,(2,2(R4)),(3,DUB)                                   
         MVC   2(2,R4),DUB         MOVE BINARY Y/M                              
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   LOCKIN1                                                          
* GET MONTH                                                                     
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(LKBRDMON)                                              
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    LOCKERR                                                          
         CLI   FLEN+1,2                                                         
         BH    LOCKERR                                                          
         CVB   R0,DUB                                                           
* GET 2-BYTE MONTH START DATE                                                   
         L     R4,AREC2                                                         
LOCKIN1A CLM   R0,1,3(R4)          MATCH MONTH                                  
         BE    LOCKIN1B                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   LOCKIN1A                                                         
         B     LOCKERR                                                          
*                                                                               
LOCKIN1B MVC   E.LOKWEEK,0(R4)     SET BROADCAST MONTH START DATE               
*                                                                               
         MVI   EDTVAL,DPTEDT       GET DAYPART                                  
         GOTO1 CALLEDT                                                          
* GET SLN                                                                       
         MVI   EDTVAL,SLNEDT                                                    
         GOTO1 CALLEDT                                                          
* GET DOLLARS                                                                   
         GOTO1 FLDVAL              NOW GET DOLLARS                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(LKDOLREQ)                                              
         LTR   R5,R5                                                            
         BZ    LOCKERR                                                          
         GOTO1 VCASHVAL,DMCB,(0,(R4)),(R5),0                                    
         MVC   E.LOKDOLS,4(R1)     SAVE DOLLARS                                 
* GET POINTS                                                                    
         LA    R6,E.LOKDEM         UP TO 4 DEMO VALUES                          
         LHI   R7,4                                                             
*                                                                               
LOCKIN2  GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    LOCKIN4                                                          
*                                                                               
         GOTO1 VCASHVAL,DMCB,(1,(R4)),(R5),0                                    
         MVC   0(4,R6),4(R1)       SAVE POINTS                                  
*                                                                               
         AHI   R6,4                                                             
         BCT   R7,LOCKIN2                                                       
                                                                                
* ADD/UPDATE LOCKIN RECORD                                                      
                                                                                
LOCKIN4  XC    WORK,WORK                                                        
S        USING SLKRECD,WORK                                                     
*                                                                               
         MVC   S.SLKKTYP(2),=X'0D73'                                            
         MVC   S.SLKKAGMD,SVKEY      A-M                                        
         MVC   S.SLKKCLT,SVKEY+1     CLT                                        
         MVC   S.SLKKMKT(5),SVKEY+4  MKT/STA                                    
         MVC   S.SLKKPRD,SVKEY+3     PRD                                        
         CLI   SVPOLPRD,0                                                       
         BE    *+10                                                             
         MVC   S.SLKKPRD,SVPOLPRD                                               
         MVC   S.SLKKEST,SVKEY+9     EST                                        
         MVC   S.SLKKDPT,BUDPT       DPT                                        
         MVC   S.SLKKLEN,BUSLN       SLN                                        
         DROP  S                                                                
*                                                                               
         MVC   WORK2,WORK                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WORK,WORK                    
         CLC   WORK(L'SLKKEY),WORK2                                             
         BNE   LOCKIN6                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFIL',WORK+36,AREC,DMWORK          
*                                                                               
         L     R8,AREC                                                          
         USING SLKRECD,R8                                                       
*                                                                               
         LA    R6,SLKFSTEL                                                      
         USING SLKEL,R6                                                         
         GOTO1 VDATCON,DMCB,(5,0),(3,SLKUPDT)                                   
         MVC   SLKDEM1(4),SVDEMLST    FIRST 4 DEMO CODES                        
         DROP  R6                                                               
                                                                                
* FIND ELEMENT FOR THIS MONTH OR ADD IT                                         
                                                                                
LOCKIN4A SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    LOCKIN4B                                                         
         CLI   0(R6),3                                                          
         BNE   LOCKIN4A                                                         
*                                                                               
         USING LOKEL,R6                                                         
         CLC   E.LOKWEEK,LOKWEEK                                                
         BL    LOCKIN4B            IF WE'RE LOW, INSERT HERE                    
         BH    LOCKIN4A            IF WE'RE HIGH, KEEP LOOKING                  
* EQUAL - UPDATE EXISTING ELEMENT                                               
         MVC   LOKDOLS,E.LOKDOLS      UPDATE EXISTING ELEMENT                   
         MVC   LOKDEM(16),E.LOKDEM    MOVE 4 DEMO VALUES                        
         B     LOCKIN5                                                          
*                                                                               
LOCKIN4B GOTO1 SVRECUP,DMCB,(C'T',AREC),ELEM,(R6)                               
*                                                                               
LOCKIN5  GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',WORK+36,AREC,DMWORK          
         B     LOCKIN10                                                         
         DROP  E                                                                
                                                                                
* CREATE NEW RECORD                                                             
                                                                                
LOCKIN6  L     R8,AREC                                                          
         USING SLKRECD,R8                                                       
         XC    0(256,R8),0(R8)    CLEAR RECORD AREA                             
*                                                                               
         MVC   SLKKEY,WORK2        SAVED KEY                                    
         MVC   SLKLEN,=Y(SLKFSTEL-SLKKEY+SLKELLNQ)  INCLUDE L'FIRSTEL           
         MVI   SLKFSTEL,X'01'                                                   
         MVI   SLKELLN,SLKELLNQ                                                 
         MVC   SLKDEM1,SVDEMLST    FIRST DEMO CODE                              
         GOTO1 VDATCON,DMCB,(5,0),(3,SLKCRDT)                                   
* INSERT DATA ELEMENT                                                           
         LA    R6,SLKFSTEL                                                      
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
* RECUP CALLS ARE INTERCEPTED IN SPBUY00 - THIS GOES DIRECT !                   
         GOTO1 SVRECUP,DMCB,(C'T',AREC),ELEM,(R6)                               
         GOTO1 VDATAMGR,DMCB,=C'ADDREC',=C'XSPFIL',WORK+36,AREC,DMWORK          
                                                                                
LOCKIN10 MVI   RCLOPT,RCLLKIN                                                   
         GOTO1 CALLDSP                                                          
         J     EXIT                                                             
                                                                                
*=================================================================              
* DISPLAY MULTIPLE LOCKIN RECORDS                                               
*=================================================================              
                                                                                
LOCKIN20 XC    WORK,WORK                                                        
S        USING SLKRECD,WORK                                                     
         MVC   S.SLKKTYP(2),=X'0D73'                                            
         MVC   S.SLKKAGMD,SVKEY      A-M                                        
         MVC   S.SLKKCLT,SVKEY+1     CLT                                        
         MVC   S.SLKKMKT(5),SVKEY+4  MKT/STA                                    
         MVC   S.SLKKPRD,SVKEY+3     PRD                                        
         CLI   SVPOLPRD,0                                                       
         BE    *+10                                                             
         MVC   S.SLKKPRD,SVPOLPRD                                               
         MVC   S.SLKKEST,SVKEY+9     EST                                        
*                                                                               
         MVC   WORK2,WORK                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WORK,WORK                    
         B     LOCKIN24                                                         
*                                                                               
LOCKIN22 GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',WORK,WORK                    
*                                                                               
LOCKIN24 CLC   WORK(SLKKDPT-SLKKEY),WORK2   SAME A-M/CL/MKST/PR/ES              
         BNE   LOCKIN26                                                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFIL',WORK+36,AREC,DMWORK          
*                                                                               
         MVI   RCLOPT,RCLLKIN                                                   
         GOTO1 CALLDSP                                                          
         B     LOCKIN22                                                         
*                                                                               
LOCKIN26 J     EXIT                                                             
*                                                                               
LOCKERR  GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* BY AGENCY OPTION, ADJUST ALL POST-BUY DEMOS BY SAME PERCENTAGE                
* AS CHANGE BETWEEN ESTIMATED DEMO 1 AND POST-BUY DEMO 1                        
* ON ENTRY R7 POINTS TO ORIGINAL DEMO ELEMENT                                   
*          R6 POINTS TO POST-BUY DEMO ELEMENT                                   
* SVDEMO1 HAS ORIG DEMO 1 VALUE/BUNEWDEM HAS NEW DEMO 1 VALUE                   
*================================================================               
                                                                                
ADJPBD   NTR1  BASE=*,LABEL=*                                                   
         L     R8,AOVWORK                                                       
         USING LOCALD,R8                                                        
*                                                                               
         CLC   =X'FFFF',BUDEM      TEST NO OVERRIDE INDICATOR                   
         BE    ADJPBDX                                                          
*                                                                               
         TM    SVDEMO1,X'40'       TEST OLD DEMO1 TO 2-DEC                      
         BZ    ADJPBD2             NO                                           
         TM    BUNEWDEM,X'40'      YES -TEST NEW DEMO1 TO 2-DEC                 
         BO    ADJPBD10            YES - BOTH 2-DEC                             
*                                                                               
         L     R1,SVDEMO1          ELSE SET DEMO1 BACK TO 1-DEC                 
         N     R1,=X'3FFFFFFF'                                                  
         M     R0,=F'2'                                                         
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,SVDEMO1                                                       
         B     ADJPBD10                                                         
*                                                                               
ADJPBD2  TM    BUNEWDEM,X'40'      OLD IS 1-DEC, TEST NEW                       
         BZ    ADJPBD10            BOTH 1-DEC - FINE                            
         L     R0,SVDEMO1                                                       
         MHI   R0,10                                                            
         ST    R0,SVDEMO1          ELSE SCALE OLD TO 2-DEC                      
         OI    SVDEMO1,X'40'                                                    
*                                                                               
ADJPBD10 LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         B     ADJPBD20            SKIP FIRST DEMO                              
*                                                                               
ADJPBD12 OC    SVSPLMKT,SVSPLMKT  TEST SPILL INPUT                              
         BZ    ADJPBD14                                                         
         CLI   BUYMD,C'R'          RADIO HAS RTGS AND IMPS                      
         BE    ADJPBD14                                                         
*                                                                               
         CLI   1(R4),C'R'          TEST RATING                                  
         BE    ADJPBD14                                                         
         CLI   1(R4),C'E'          TEST EXTENDED RATING                         
         BE    ADJPBD14                                                         
*                                                                               
         CLI   1(R4),X'21'         TEST USER DEMO                               
         BNE   ADJPBD20            NO                                           
         SR    RF,RF                                                            
         IC    RF,2(R4)            GET DEMO NUMBER                              
         BCTR  RF,0                                                             
         MHI   RF,7                                                             
         LA    RF,SVUSRNMS(RF)                                                  
         CLI   0(RF),C'R'                                                       
         BE    ADJPBD14                                                         
         CLI   0(RF),C'E'                                                       
         BNE   ADJPBD20                                                         
*                                                                               
ADJPBD14 BAS   RE,ADJFIND          FIND SLOTS IN 02/22 03/23 ELEMS              
*                                                                               
         LM    RE,RF,DUB           22/23 SLOT -- 02/03 SLOT                     
*                                                                               
         XC    0(3,RE),0(RE)       CLEAR OLD VALUE                              
*                                                                               
         ICM   R0,15,4(RF)         ORIG VALUE OF THIS DEMO                      
         N     R0,=X'3FFFFFFF'                                                  
         ICM   R1,15,BUNEWDEM      NEW VALUE OF DEMO 1                          
         N     R1,=X'3FFFFFFF'                                                  
         AR    R1,R1                X 2                                         
         MR    R0,R0                                                            
*                                                                               
         L     R5,SVDEMO1          ORIG VALUE OF DEMO 1                         
         N     R5,=X'3FFFFFFF'     DROP FLAGS                                   
         LTR   R5,R5                                                            
         BZ    ADJPBD20                                                         
         DR    R0,R5                                                            
         SRL   R1,1                                                             
         STCM  R1,7,0(RE)                                                       
*                                                                               
         OI    0(RE),X'80'                                                      
         OC    0(1,RE),4(RF)       USE 2-DEC FLAG FROM ORIG VALUE !             
*                                                                               
ADJPBD20 AHI   R4,3                NEXT DEMO                                    
         OC    0(3,R4),0(R4)                                                    
         BNZ   ADJPBD12                                                         
*                                                                               
ADJPBDX  XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
*==================================================================             
* FIND ORIGINAL DEMO IN 02/03 ELEMENT                                           
* AND POSITION FOR PBD VALUE IN 22/23 ELEMENT                                   
* R7 POINTS TO ORIGINAL DEMO ELEMENT                                            
* R6 POINTS TO POST-BUY DEMO ELEMENT                                            
*==================================================================             
                                                                                
ADJFIND  NTR1                                                                   
*                                                                               
         LHI   R0,PDEMO-PDELEM     DSPL TO FIRST PBD DEMO VALUE                 
         OC    SVSPLMKT,SVSPLMKT   TEST SPILL                                   
         BZ    *+8                                                              
         LHI   R0,SDEMO-SDELEM     ALTERNATE DSPL                               
         AR    R6,R0               FIRST PBD VALUE                              
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AHI   R0,-24                                                           
         SRL   R0,3                SET FOR BCT                                  
         AHI   R7,24               FIRST ORIG DEMO SLOT                         
*                                                                               
ADJFIND2 CLC   0(3,R4),0(R7)       MATCH DEMO                                   
         BE    ADJFINDX                                                         
         AHI   R6,3                                                             
         AHI   R7,8                                                             
         BCT   R0,ADJFIND2                                                      
         DC    H'0'                                                             
*                                                                               
ADJFINDX STM   R6,R7,DUB           SET POST/ORIG DEMO POSITIONS                 
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* DSECT TO COVER OVERLAY WORKING STORAGE                                        
*                                                                               
LOCALD   DSECT                                                                  
*                                                                               
SAVER4   DS    A                                                                
SAVER5   DS    A                                                                
SVDEMO1  DS    F                                                                
SV50EL   DS    A                                                                
SVFLAGS  DS    CL32                NONT DEMO FLAGS                              
SVFLDVAL DS    XL(SLNTAB-FADDR)    SAVE FLDVAL VALUES                           
FIRSTDEM DS    C                                                                
*                                                                               
         DS    XL(L'OVWORK-(*-LOCALD))  SPARE                                   
         SPACE 2                                                                
* DSECT TO COVER CHANGE KEYWORD TABLE                                           
*                                                                               
CHGTABD  DSECT                                                                  
CHGNAME  DS    CL7                 KEYWORD NAME                                 
CHGMINL  DS    X                   MINIMUM LENGTH FOR KEYWORD                   
CHGOV    DS    X                   CHANGE OVERLAY                               
CHGEDT   DS    X                   EDIT VALUE                                   
CHGCTL   DS    X                   CONTROL BITS                                 
CHGROUT  DS    AL3                 A(CHANGE ROUTINE)                            
CHGORIG  DS    AL3                 A(ORIGINAL DATA CHECK ROUTINE)               
CHGTABL  EQU   *-CHGTABD                                                        
         SPACE 2                                                                
NOEDT    EQU   X'80'               CHANGE KEYWORD DOES NOT NEED EDIT            
NBUY     EQU   X'40'               CANADIAN NETWORK BUY                         
EXP      EQU   X'20'               BUY EXPLODE                                  
CANAD    EQU   X'10'               CANADIAN AGENCIES                            
         EJECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE SPGENXLK                                                       
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068SPBUY08   04/17/19'                                      
         END                                                                    

*          DATA SET SPBUY04    AT LEVEL 032 AS OF 11/19/19                      
*PHASE T21104C                                                                  
                                                                                
*===========================================================*                   
* 06MAR97                                                   *                   
* WATCH OUT FOR CODE ABOUT CANADIAN BUYS ON STATIONS WITH   *                   
* HST CROSSING THE DATE IT WENT LIVE - APR01/97.            *                   
*                                                           *                   
*                                                           *                   
* =========> NOP ERROR IF NO EXCHANGE RATE IN BLDXCH        *                   
*            SO CAN FINALLY INSTALL THIS VERSION            *                   
*                                                           *                   
*===========================================================*                   
         TITLE 'T21104 - SPOTPAK BUY - NEW BUYS AND COPIES'                     
T21104   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,T21104,R9,R8,RR=R7                                             
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R7,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R7,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BZ    B2                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CLTFRZN)                                               
         B     BUYERR                                                           
*                                                                               
B2       CLI   QSTA,C'0'                TEST CABLE STATION                      
         BL    B4                                                               
         L     R4,=A(SVB0PROF-BUYSAVE)  READ B0 PROFILE                         
         AR    R4,RA                                                            
         CLI   2(R4),C'Y'               TEST NETWORK REQUIRED                   
         BNE   B4                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOCBLNET)                                              
         CLI   QCBLNET,C' '        TEST NETWORK INPUT                           
         BNH   BUYERR                                                           
*                                                                               
B4       XC    BUHIATS,BUHIATS     CLEAR HIATUS WEEK LIST                       
         XC    BUDSKED,BUDSKED     CLEAR DAILY SCHEDULE AREA                    
*                                                                               
         MVI   BUDEMSW,0           RESET DEMO RESEQ SWITCH                      
         MVI   SVMGINIT,C'N'                                                    
         XC    SVSPLMKT,SVSPLMKT                                                
* BUILD SKELETON DEMO ELEM                                                      
         XC    BUDEMS,BUDEMS                                                    
         LA    R6,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R6,SVBRDEMS                                                      
         LA    R1,BUDEMS+24                                                     
*                                                                               
B10      CLI   1(R6),0                                                          
         BE    B12                                                              
         MVC   0(3,R1),0(R6)                                                    
         LA    R1,8(R1)                                                         
         LA    R6,3(R6)                                                         
         B     B10                                                              
*                                                                               
B12      LA    R0,BUDEMS                                                        
         SR    R1,R0                                                            
         STC   R1,BUDEMS+1         SET ELEM LENGTH                              
         MVI   BUDEMS,2            AND ELEM CODE                                
         EJECT                                                                  
B15      XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)            ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         CLI   BUTRCODE,C'B'       BUY                                          
         BE    B200                                                             
         MVI   ERRCD,TRCDERR                                                    
         CLI   FLEN+1,4                                                         
         BH    BUYERR                                                           
         CLC   =C'COPY',0(R4)                                                   
         BE    B400                                                             
         CLC   =C'CPY',0(R4)                                                    
         BE    B400                                                             
         CLC   =C'MOVE',0(R4)                                                   
         BE    B700                                                             
         B     BUYERR                                                           
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERRX  EQU   *                                                                
BUYERR   GOTO1 ERROR                                                            
RELO     DC    A(0)                                                             
         EJECT                                                                  
* NEW BUY                                                                       
*                                                                               
B200     DS    0H                                                               
         TM    SVOPT1,SVOPT1_VNDRLCK  X'10' - VENDOR LOCKED?                    
         BNZ   B202                   YES, NO NEW BUYS                          
         OC    SVKEY+4(2),SVKEY+4  TEST BUYING MKT 0                            
         BNZ   B203                NO - GOOD                                    
         LA    R2,BUYSTH                                                        
         MVI   ERRCD,MKTERR                                                     
         B     BUYERR                                                           
*                                                                               
B202     LA    R2,BUYSTH           POINT TO THE STATION                         
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(STALCKED)  STA LOCKED - CANNOT TRANSFER..            
         B     BUYERR                                                           
*                                                                               
B203     XC    WORK2,WORK2         CLEAR AREA FOR OVERLAY STORAGE               
         BAS   RE,TESTID                                                        
         MVI   EDTVAL,PEREDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,NPWEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,TIMEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,DPTEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,SLNEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,PGMEDT                                                    
         GOTO1 CALLEDT                                                          
         TM    WRKRUPSW,WRKRUPSW_NEWDRMG    TEST NEW DARE MAKEGOOD              
         BO    *+12                                                             
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BO    B204                         YES - NO ADJ CODES !                
         MVI   EDTVAL,ADJEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
B204     MVI   EDTVAL,COSTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         CLI   FSTOP,C','                                                       
         BNE   B250                                                             
* NEXT FIELD MAY BE DEMO OVERRIDES                                              
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
*                                                                               
         CLI   FSTOP,X'FF'         TEST E-O-D                                   
         BE    B250                                                             
         CLI   FSTOP,C'='          TEST KEYWORD STOP CHAR                       
         BE    B213                YES - NO DEMOS                               
         EJECT                                                                  
* EDIT DEMO OVERRIDES                                                           
         MVI   EDTVAL,DEMEDT                                                    
         LA    R7,BUDEMS+24                                                     
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         LA    R6,1                                                             
         SPACE 1                                                                
B206     STC   R6,UPNDX                                                         
         MVC   SPDEMTYP,1(R7)      SET DEMO TYPE                                
         GOTO1 CALLEDT                                                          
         OC    FLEN,FLEN                                                        
         BZ    B208                                                             
         MVC   4(4,R7),BUNEWDEM                                                 
         L     RF,FADDR                                                         
         CLI   0(RF),C'L'          LOOKUP?                                      
         BE    *+8                                                              
         OI    4(R7),X'80'                                                      
         MVI   3(R7),100                                                        
B208     CLI   FSTOP,C'/'                                                       
         BNE   B212                                                             
         LA    R6,1(R6)                                                         
         LA    R7,8(R7)                                                         
B210     CLI   1(R7),0              TEST NO MORE DEMOS                          
         BNE   B206                                                             
         MVI   ERRCD,MAXDEMS                                                    
         B     BUYERR                                                           
         SPACE 1                                                                
* DEMOS DONE - SEE IF MORE DATA *                                               
         SPACE 1                                                                
B212     DS    0H                                                               
         CLI   FSTOP,C','                                                       
         BNE   B250                                                             
B212A    MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'='                                                       
         BE    B213                                                             
         MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,X'FF'         SHOULD BE NO STOP CHAR                       
         BNE   BUYERR                                                           
         LTR   R5,R5               AND NO MORE INPUT                            
         BNZ   BUYERR                                                           
         B     B250                                                             
         EJECT                                                                  
* CHECK FOR MASTER ALLOC OR PARTNER ENTRY OR UPGRADE DATA *                     
         SPACE 1                                                                
B213     CLC   =C'M=',0(R4)                                                     
         BE    B220                                                             
         CLC   =C'PA=',0(R4)                                                    
         BE    B230                                                             
         CLC   =C'UPP=',0(R4)                                                   
         BE    B240                                                             
         CLC   =C'UPT=',0(R4)                                                   
         BE    B240                                                             
         CLC   =C'BK=',0(R4)                                                    
         BE    B240                                                             
         CLC   =C'DT=',0(R4)                                                    
         BE    B240                                                             
         CLC   =C'H=',0(R4)                                                     
         BE    B245                                                             
         CLC   =C'LI=',0(R4)                                                    
         BE    B225                                                             
         CLC   =C'SD',0(R4)                                                     
         BE    B235                                                             
         MVI   ERRCD,BADCOMMA                                                   
         B     BUYERR                                                           
         EJECT                                                                  
* EDIT MASTER PRD ENTRY                                                         
*                                                                               
B220     DS    0H                                                               
         XC    BUELDATA,BUELDATA                                                
         MVI   ERRCD,INVBRPOL                                                   
         CLI   SVPRD,X'FF'         TEST BUYING POL                              
         BNE   BUYERR              NO-ERROR                                     
         CLI   SVPOLPRD,0          TEST BRAND POL UNDER BRAND                   
         BNE   BUYERR              YES - ERROR                                  
*                                                                               
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   ERRCD,BADMAS                                                     
         TM    BUELPRSW,X'E7'      TEST ANY BITS BUT FREE-RIDER                 
         BNZ   BUYERR                                                           
         CLI   BUELPRD,0           M=UNALL IS INVALID TOO                       
         BE    BUYERR                                                           
*                                                                               
         B     B212A                                                            
         SPACE 1                                                                
* EDIT FOR ASSIGNED LINE NUMBER                                                 
*                                                                               
B225     XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,LINERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         CLI   FLEN+1,3                                                         
         BH    BUYERR1                                                          
         TM    FVAL,X'08'          TEST FOR NUMERIC VALUE                       
         BZ    BUYERR1                                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR1                                                          
         STH   R0,NEWLINE                                                       
         LA    RE,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    RE,255                                                           
         CR    R0,RE                                                            
         BH    BUYERR1                                                          
*                                                                               
B226     GOTO1 LOOKLIN,0                                                        
         BNE   B227                DID NOT FIND EXISTING LINE                   
         MVI   ERRCD,OVERWERR                                                   
         TM    SVOPT2,X'08'        TEST OVERWRITE OF BUY ALLOWED                
         BZ    BUYERR1             NO                                           
         TM    BUYRCNTL,X'80'      TEST IF RECORD DELETED                       
         BO    B227                YES-OK TO OVERWRITE                          
         BAS   RE,CHKREP           CHECK OUT IF REPLACEMENT OK                  
         BNE   BUYERR1             NO                                           
*                                                                               
B227     B     B212A               NEXT OPTION                                  
         SPACE 1                                                                
* EDIT PARTNER ENTRY  FORMAT IS PA='PRD'-'EST'-'SLN' *                          
         SPACE 1                                                                
B230     DS    0H                                                               
         MVI   ERRCD,INVPTNR                                                    
         CLI   SVPRD,X'FF'         TEST BUYING POL                              
         BE    BUYERR                                                           
*                                                                               
         MVI   EDTVAL,PTNREDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         B     B212A                                                            
*                                                                               
B235     CLI   SVPRD,X'FF'         TEST BUYING POL                              
         BNE   BUYERR                                                           
         MVI   EDTVAL,DSKEDT       SET FOR DAILY SKED EDIT                      
         GOTO1 CALLEDT                                                          
         B     B212A                                                            
         SPACE 1                                                                
* INTERFACE TO UPGRADE DATA EDIT *                                              
         SPACE 1                                                                
B240     MVI   ERRCD,BADKEYWD                                                   
         CLI   ADBLOCK,C'U'        TEST BEEN HERE BEFORE                        
         BE    BUYERR                                                           
*                                                                               
         MVI   EDTVAL,UPEDT                                                     
         GOTO1 CALLEDT                                                          
         B     B212A                                                            
         SPACE 2                                                                
B245     GOTO1 =A(EDHIAT),RR=RELO                                               
         B     B212A                                                            
         EJECT                                                                  
B250     MVI   ERRCD,BADCOMMA                                                   
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,X'FF'         TEST NO MORE DATA                            
         BNE   BUYERR                                                           
*                                                                               
         CLI   SVPRD,X'FF'         TEST BUYING POL                              
         BNE   B252                NO                                           
         CLI   BUELPRD,0           TEST MASPRD ENTERED                          
         BNE   B252                                                             
         MVC   BUELPRD(2),SVPOLPRD  USE DEFAULT                                 
         CLI   BUELPRD,0                                                        
         BNE   B252                                                             
         MVI   ERRCD,NOMASPRD                                                   
         CLI   SVCPROF,C'0'        TEST BRAND POL                               
         BNE   BUYERR              YES-ERROR                                    
*                                                                               
* CLEAR BUYREC                                                                  
*                                                                               
B252     CLI   REPLSW,C'Y'         TEST REPLACING BUY LINE                      
         BNE   *+8                                                              
         BAS   RE,SETPRDL          BUILD A PRODUCT LIST                         
*                                                                               
         LA    R0,BUYREC                                                        
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
* FIND LINE NUMBER                                                              
         OC    NEWLINE,NEWLINE     TEST FOR ASSIGNED NUMBER                     
         BNZ   *+12                YES                                          
         BAS   RE,NXTBUYLN         NO-READ FOR NEXT LINE                        
         B     B254                                                             
*                                                                               
         MVC   BUYKEY(10),SVKEY                                                 
         MVC   BUYKEY+10(2),NEWLINE                                             
*                                                                               
B254     MVC   BUYRLEN,=H'94'                                                   
         MVC   BUYALPHA,AGYALPHA                                                
*                                                                               
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,70                                                         
*                                                                               
         MVC   BDSTART(6),BUSTARTB                                              
*======================================================*                        
* NOTE BDWKS SET IN CALEND                             *                        
*======================================================*                        
         MVC   BDINPUT,BUPERIND                                                 
         MVC   BDWKIND,BUWKIND                                                  
         MVC   BDDAY,BUDAYS                                                     
         MVC   BDSEDAY,BUDAYNUM                                                 
         MVC   BDNOWK,BUNPW                                                     
         MVC   BDSEC,BUSLN                                                      
         MVC   BDDAYPT,BUDPT                                                    
         MVC   BDTIMST(4),BUTIME                                                
         MVC   BDPROGRM,BUPROG                                                  
         MVC   BDPROGT,BUADJ                                                    
         MVC   BDCOST(4),BUCOST                                                 
         MVC   BDCIND2,BUCIND2                                                  
*                                                                               
B257     BRAS  RE,BLDCOST2                                                      
*                                                                               
B257X    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   *+8                                                              
         OI    BDCIND2,X'20'       SET CANADIAN FLAG                            
*                                                                               
         MVC   BDREP,SVESTREP                                                   
         OC    SVOPTREP,SVOPTREP                                                
         BZ    *+10                                                             
         MVC   BDREP,SVOPTREP                                                   
         OC    BDREP,BDREP                                                      
         BNZ   *+10                                                             
         MVC   BDREP,BUREP                                                      
         TM    SVOPT2,X'40'       TEST NO TAX OPTION                            
         BO    *+10                                                             
         MVC   BDNTAX,SVNTAX                                                    
         OC    BDSTAT,SVPOLNPW                                                  
*                                                                               
         MVI   BUWHY,X'80'         SET NEW BUY                                  
         GOTO1 SETCHGDT                                                         
*                                                                               
         MVC   BDADVAGY,SVADVAGY                                                
*                                                                               
         CLI   SVPRD,X'FF'         TEST POL                                     
         BNE   B258                NO                                           
         MVC   BDMASPRD,BUELPRD                                                 
         B     B260                                                             
         SPACE 2                                                                
*************************************                                           
*          NON-POL BUY              *                                           
*************************************                                           
         SPACE 1                                                                
B258     MVC   BDTIME(2),BUACTV    SET ACTV P/B SHR                             
         CLI   BUPSSV,0            TEST P/B                                     
         BE    B260                NO                                           
* ADD PBELEM                                                                    
         LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVC   ELEM(2),=X'0409'                                                 
         MVC   ELEM+2(7),BUPSSV                                                 
         BAS   RE,ELEMADD                                                       
         SPACE 1                                                                
* INSERT DEMO ELEMENT *                                                         
         SPACE 1                                                                
B260     LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,BUYREC,BUDEMS,(R6)                                   
*                                                                               
         GOTO1 VBLDDEM             RESEQUENCE TO POL SEQ IF NEEDED              
*                                                                               
         GOTO1 VBLDEL              BUILD ELEMENT                                
         BAS   RE,CALEND                                                        
         BAS   RE,CHKMAXEL                                                      
         BNE   BUYERR                                                           
*                                                                               
*****    CLI   BUYMD,C'T'                                                       
*****    BNE   B268                                                             
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    B267                                                             
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    B267                                                             
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   B268                                                             
         MVI   BDCANAD,X'80'        GEN CMBD PTRS                               
*                                                                               
B267     LA    R7,SVKEY+4           POINT TO MKT/STA                            
         GOTO1 VGETSPLL                                                         
*                                                                               
B268     DS    0H                                                               
         CLI   ADBLOCK,C'U'        TEST DBLOCK SET FOR UPGRADE                  
         BE    B268X               YES                                          
         CLI   SVUPFIL,0           TEST UPGRADE SET IN OPTIONS                  
         BE    B269                NO - DO DEMO LOOKUP                          
* CALL UPEDT TO FORMAT DBLOCK NOW *                                             
         MVI   EDTVAL,UPEDT                                                     
         MVI   ADBLOCK,C'X'        SET FLAG FOR DUMMY EDIT                      
         GOTO1 CALLEDT                                                          
*                                                                               
B268X    GOTO1 DEMUPGD                                                          
*                                                                               
B269     TM    SVARBF94,QARBF94    CAN STATION USE ARBF94                       
         BZ    B269X               NO                                           
         CLC   BDEND,F94START      BUY END BEFORE F94 START                     
         BL    B269X               YES                                          
         CLC   BDSTART,F94END      BUY START AFTER F94 END                      
         BH    B269X               YES                                          
         MVI   ERRCD,INVF94        MUST INPUT OPTION                            
         TM    SVARBF94,QF94NSI    NSI SET                                      
         BO    B269X               YES - IGNORE                                 
         TM    SVARBF94,QF94ARB    ARB SET                                      
         BZ    BUYERR              NO - TELL THEM IT'S NOT VALID                
         OI    BDSTAT2,X'04'       SET ARBF94 FLAG IN BUYREC                    
*                                                                               
* FOLLOWING OPTION SUPPORTED AT AGY AND CLIENT LEVELS                           
*                                                                               
B269X    TM    SVAFLAG1,X'02'      TEST TRADE AGENCY (NOT CTA)                  
         BZ    *+8                                                              
         OI    BDSTAT2,X'20'       SET TRADE AGENCY BUY                         
*                                                                               
         TM    SVCOPT2,COP2TRAD    TEST TRADE AGENCY (NOT CTA)                  
         BZ    *+8                                                              
         OI    BDSTAT2,X'20'       SET TRADE AGENCY BUY                         
*                                                                               
         TM    SVCOPT2,X'01'       TEST DIY TRADE CLIENT                        
         BZ    *+8                                                              
         OI    BDSTAT2,X'10'       SET DIY TRADE BUY                            
*                                                                               
         GOTO1 DEMLKUP                                                          
         B     B270                                                             
*                                                                               
F94START DC    AL1(93,12,27)                                                    
F94END   DC    AL1(94,03,27)                                                    
         EJECT                                                                  
B270     DS    0H                                                               
         BAS   RE,BLDID                                                         
*                                                                               
         BAS   RE,BLDPST           ADD PST ELEMENT                              
*                                                                               
         OC    SVCCOST2,SVCCOST2                                                
         BNZ   *+14                                                             
         OC    SVECOST2,SVECOST2                                                
         BZ    *+12                                                             
         MVI   BUCOS2IND,X'80'     SET FLAG COST2 INPUT                         
         BRAS  RE,SETCOST2                                                      
*                                                                               
         CLI   SVAPROF+7,C'C'           TEST CANADIAN AGENCY                    
         BNE   *+8                                                              
         BRAS  RE,BLDXCH                BUILD EXCHANGE RATE ELEMENT             
*                                                                               
B270A    TM    WRKRUPSW,WRKRUPSW_ISME  IS THIS A WRKR UPLOAD                    
         BO    B270B                   YES                                      
         TM    UPSW,UPON               TEST UPLOADING                           
         BZ    B271                    NO                                       
*                                                                               
B270B    BAS   RE,BLDUPL               YES-BUILD UPLOAD ELEM                    
*                                                                               
B271     BRAS  RE,SET50EL          SET DO NOT LOOKUP FLAGS IN 50 ELEM           
*                                                                               
         CLC   =C'COP',BUTRCODE    TEST FOR COPY                                
         BE    B272                YES-GO RIGHT TO ADDREC                       
         OC    NEWLINE,NEWLINE     TEST IF LINE NUMBER ASSIGNED                 
         BZ    *+12                NO                                           
         BAS   RE,LINADD           YES-SPECIAL IO HANDLING                      
         B     B274                                                             
*                                                                               
B272     TM    UPSW,UPON+UPCHA     TEST ACTION=CHANGE FOR UPLOAD                
         BO    EXIT                YES-EXIT, LEAVING BUY IN REC                 
*                                                                               
B273     DS    0H                                                               
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    B274                                                             
         GOTO1 GOGETCTA,DMCB,('CIBADDQ',AREC)                                   
*                                                                               
B274     DS    0H                                                               
         TM    WRKRUPSW,WRKRUPSW_NOIO    TEST SUPPRESS IO                       
         BO    B275                                                             
*                                                                               
         TM    WRKRUPSW,WRKRUPSW_NEWDRMG    TEST NEW DARE MAKEGOOD              
         BZ    B274X                                                            
         BRAS  RE,BLDNEWDR                  ADD DARE TRACE ELEM                 
* ADD BUY RECORD                                                                
*                                                                               
B274X    CLC   =C'COP',BUTRCODE    TEST FOR COPY -NEWLINE DOESN'T EXIST         
         BE    *+14                YES-GO RIGHT TO ADDREC                       
         OC    NEWLINE,NEWLINE     TEST LINE NUMBER SPECIFIED                   
         BNZ   B274X2              YES- ALREADY DID ADDREC/PUTREC               
*                                                                               
         BRAS  RE,CHKRSN           CHECK RSNCODE PRESENT IF REQ'D               
         MVC   AREC,AREC1                                                       
*                                                                               
         BRAS  RE,CKEXISTS                                                      
         GOTO1 ADDREC                                                           
*                                                                               
B274X2   MVC   SVKEY+14(4),KEY+14                                               
         OI    SVUPDATE,X'80'        SET NEW BUY ADDED                          
         OC    SVKEY+14(4),SVKEY+14  TEST NOT MARKING FILE                      
         BZ    B275                  YES - DON'T MAKE IT WORSE                  
         BRAS  RE,DARBATCH                                                      
*                                                                               
         TM    UPSW,UPON           TEST UPLOADING                               
         BZ    B275                                                             
         GOTO1 GETREC                GTFB (GET THE BUY RECORD !)                
*                                                                               
         SPACE 1                                                                
B275     GOTO1 VBLDQLST            GENERATE REQ PRD LIST                        
*                                                                               
         MVC   BUYMSG(33),=C'** BUY ADDED. NOTE LINE NUMBER **'                 
         TM    VCALLBAS,X'80'      TEST CALLBASE MODE                           
         BZ    B275A                                                            
         MVC   BMGELINE,SVKEY+11   PASS ADDED LINE NUMBER                       
*                                                                               
B275A    MVI   RCLOPT,0                                                         
         CLC   =C'COP',BUTRCODE                                                 
         BNE   *+12                                                             
         CLI   NREPS,0             TEST N= FEATURE                              
         BNE   B276                YES                                          
         TM    SVOPTS,X'02'        DID USER SAY NO ROTATIONS                    
         BO    B276                YES                                          
         MVI   RCLOPT,RCLDEM       USER REQUEST DEMS                            
         TM    SVOPTS,X'08'                                                     
         BO    B276                                                             
         MVI   RCLOPT,RCLROT       DISPLAY ROTATION                             
*                                                                               
B276     DS    0H                                                               
         OC    BUDSKED,BUDSKED                                                  
         BZ    B276A                                                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              MUST REBUILD GETREC TABLE AFTER ADD          
         GOTO1 VDSKED,DMCB,(RC)    DO DAILY SCHEDULING                          
         B     B276B                                                            
*                                                                               
B276A    DS    0H                                                               
         GOTO1 CALLDSP                                                          
*                                                                               
B276B    CLC   =C'COP',BUTRCODE                                                 
         BNE   B278                                                             
         CLI   NREPS,0             TEST COPY,N= FEATURE                         
         BE    B278                NO                                           
         ZIC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         C     RE,FLAST                                                         
         BH    *+8                                                              
         MVI   5(RE),0             SET TO IGNORE FURTHER INPUT                  
         ZIC   R0,NREPS                                                         
         BCTR  R0,0                DECREMENT COUNT                              
         STC   R0,NREPS             AND SAVE                                    
         LTR   R0,R0                                                            
         BZ    B278                                                             
         BAS   RE,NXTBUYLN         GET NEXT LINE NUM                            
         B     B270                                                             
         SPACE 1                                                                
*======================================================*                        
* MOVE '*' TO CHAR 1  OF INPUT LINE                    *                        
*======================================================*                        
         SPACE 1                                                                
B278     DS    0H                                                               
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
         TM    ABUYINPH,X'80'      TEST DATA WAS CONTINUED                      
         BZ    B279                NO                                           
*                                                                               
B278A    SR    RF,RF                                                            
         ICM   RF,7,ABUYINPH+1     GET ORIGINAL FIELD ADDRESS                   
         CR    R2,RF               TEST IT'S NOT BEYOND ORIGINAL!               
         BNH   B278X                                                            
*                                                                               
         AHI   R2,-(L'BUYINP1H+L'BUYINP1)                                       
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
         B     B278A                                                            
*                                                                               
B278X    XC    ABUYINPH,ABUYINPH                                                
*                                                                               
* IF MATCHMAKER CALLING BUT NOT UPDATING, TERMINATE INPUT                       
*                                                                               
B279     TM    SVSPOMAK,SVSPOMAK_NOBUY TEST DO-NOT-ADD                          
         BZ    EXIT                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         C     R2,FLAST                                                         
         BH    EXIT                                                             
         MVI   5(R2),0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
B280     DS    0H                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* CREATE REGELEMS FOR ALL WEEKS IN BUY PERIOD                                   
*                                                                               
CALEND   NTR1                                                                   
* FIND LAST ELEM                                                                
         LA    R6,BDELEM                                                        
         SR    R7,R7               CLEAR COUNTER                                
CAL4     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'20'         INSERT AFTER ANY ELEM > X'1F'                
         BH    CAL4X                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         BNE   CAL4                                                             
* GET EBCDIC START/END DATES                                                    
CAL4X    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 (RF),(R1),(3,BDEND),WORK+12                                      
*                                                                               
         CLI   BDWKIND,0           TEST IF ZERO                                 
         BNE   *+8                                                              
         MVI   BDWKIND,C'O'        YES-COVER FOR BWS BUG                        
         LA    RE,7                                                             
         CLI   BDWKIND,C'O'                                                     
         BE    CAL6                                                             
         LA    RE,14                                                            
         CLI   BDWKIND,C'A'                                                     
         BE    CAL6                                                             
         LA    RE,21                                                            
         CLI   BDWKIND,C'T'                                                     
         BE    CAL6                                                             
         LA    RE,28                                                            
         CLI   BDWKIND,C'F'                                                     
         BE    CAL6                                                             
         DC    H'0'                                                             
CAL6     ST    RE,FULL             SAVE NUMBER OF DAYS BETWEEN SPOTS            
         LA    R5,BUHIATS          POINT TO LIST OF HIATUS WEEKS                
*                                                                               
CAL8     GOTO1 VDATCON,DMCB,WORK,(2,ELEM+2)                                     
         LA    R7,1(R7)                                                         
*                                                                               
         CLI   0(R5),0             ANY MORE HIATUS WEEKS                        
         BE    CAL8X               NO                                           
         CLC   ELEM+2(2),0(R5)     MATCH HIATUS WEEK                            
         BL    CAL8X               LOW - CONTINUE                               
         MVI   ERRCD,BADWEEK                                                    
         BH    BUYERR              HIGH - ERROR - BAD DATE                      
         LA    R5,2(R5)            ADVANCE HIATUS POINTER                       
         B     CAL11                 AND CONTINUE                               
*                                                                               
CAL8X    LA    R0,1                NON-POL GETS 1 ELEM/WEEK                     
         LA    R1,SVPOLNPW                                                      
         CLI   BUTRCODE,C'B'       IF NEW BUY                                   
         BE    *+8                                                              
         LA    R1,BDSTAT                                                        
         TM    0(R1),X'80'                                                      
         BO    CAL9                                                             
         CLI   ELEM,X'06'                                                       
         BE    *+8                                                              
         IC    R0,BDNOWK           POL GETS 1 ELEM PER SPOT                     
*                                                                               
CAL9     CLC   =C'COP',BUTRCODE    TEST FOR COPY                                
         BNE   *+14                NO                                           
         CLC   FRSTLIN,FRENDLIN    TEST FOR MULTIPLE LINE COPY                  
         BNE   CAL10               YES-SKIP GOALS TEST                          
         GOTO1 TESTGLS,DMCB,ELEM   TEST GOALS PRESENT IF REQD                   
*                                                                               
CAL10    BAS   RE,ELEMADD                                                       
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         BCT   R0,CAL10            LOOP FOR NUMBER OF ELEMENTS                  
*                                                                               
CAL11    L     R0,FULL                                                          
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         CLC   WORK+6(6),WORK+12   TEST PAST END                                
         BH    CAL12                                                            
         MVC   WORK(6),WORK+6                                                   
         B     CAL8                                                             
*                                                                               
CAL12    STC   R7,BDWKS            SET NUMBER OF WEEKS IN BDELEM                
         MVI   ERRCD,BADWEEK                                                    
         CLI   0(R5),0             TEST ANY REMAINING HIATUS WEEKS              
         BNE   BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO LOOK FOR AN EXISTING BUY LINE                                  
* AT ENTRY, R1=0 TO READ INTO AREC1, R1=1 TO READ INTO AREC2                    
* ON EXIT, CC=EQ IF BUY FOUND, CC=NEQ IF NOT FOUND                              
*                                                                               
LOOKLIN  NTR1                                                                   
         STC   R1,IOAREASW         SAVE PARAMETER VALUE                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       A-M/CLT/PRD/MKT/STA/EST                      
         MVC   KEY+11(2),NEWLINE   ASSIGNED LILNE NUMBER                        
         MVC   SVKEY,KEY                                                        
         OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEQXIT              DID NOT FIND IT                              
*                                                                               
LOOKLIN2 L     R0,AREC                                                          
         CLI   IOAREASW,0          TEST TO READ INTO AREC1                      
         BE    *+10                YES                                          
         MVC   AREC,AREC2                                                       
         OI    DMINBTS,X'88'                                                    
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
         MVI   REPLSW,C'Y'                                                      
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CHECK IF BUY LINE CAN BE REPLACED (OVERWRITTEN)                
* CC=EQ ON EXIT IF OK, CC=NEQ IF ERROR                                          
*                                                                               
CHKREP   NTR1                                                                   
         MVI   ELCDLO,X'05'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         MVI   ERRCD,REPMGERR                                                   
         OC    BDMGDATE,BDMGDATE   TEST IF MAKE-GOOD LINE                       
         BNZ   NEQXIT              YES                                          
*                                                                               
CHKREP2  BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
*                                                                               
         MVI   ERRCD,REPPKERR                                                   
         CLI   0(R6),X'05'         TEST FOR PACKAGE ELEMENT                     
         BE    NEQXIT              YES-CANNOT REPLACE LINE                      
         BAS   RE,TESTPD           TEST IF SPOT BILLED OR PAID                  
         BNE   NEQXIT                                                           
         BAS   RE,TESTMTCH         TEST IF MATCHED AFFIDAVIT                    
         BNE   NEQXIT                                                           
         MVI   ERRCD,REPMSERR                                                   
         TM    6(R6),X'02'         TEST MADE-GOOD                               
         BO    NEQXIT              YES                                          
         B     CHKREP2                                                          
         EJECT                                                                  
* TEST SPOT AT 0(R6), BILLED OR PAID                                            
* CALLED BY CHKREP, CC=NEQ ON EXIT IF BILLED OR PAID                            
*                                                                               
TESTPD   NTR1                                                                   
         MVI   ERRCD,REPBPERR                                                   
*                                                                               
         OC    4(2,R6),4(R6)                                                    
         BNZ   NEQXIT                                                           
*                                                                               
         CLI   0(R6),X'0B'                                                      
         BL    TPD10                                                            
* POL TESTS                                                                     
         ZIC   R0,1(R6)                                                         
         AHI   R0,-10                                                           
         BZ    EQXIT                                                            
         SRL   R0,2                                                             
         LA    R7,10(R6)                                                        
TPD2     OC    2(2,R7),2(R7)                                                    
         BNZ   NEQXIT                                                           
         LA    R7,4(R7)                                                         
         BCT   R0,TPD2                                                          
         B     EQXIT                                                            
*                                                                               
* NON-POL TESTS                                                                 
*                                                                               
TPD10    ZIC   R0,1(R6)                                                         
         AHI   R0,-8                                                            
         SRL   R0,1                                                             
         LA    R7,8(R6)                                                         
TPD12    OC    0(2,R7),0(R7)                                                    
         BNZ   NEQXIT                                                           
         LA    R7,2(R7)                                                         
         BCT   R0,TPD12                                                         
         B     EQXIT                                                            
         SPACE 2                                                                
* TEST FOR AFFIDS FOR ELEM AT 0(R6).                                            
* ASSUME ELCDLO AND ELCDHI CONTAIN REGEL ARGUMENTS                              
* CALLED BY CHKREP, CC=NEQ IF MATCHED                                           
*                                                                               
TESTMTCH NTR1                                                                   
         MVI   ERRCD,REPMTERR                                                   
*                                                                               
         LR    R5,R6               SAVE EL ADDRESS                              
TMTCH2   BAS   RE,NEXTEL                                                        
         BNE   TMTCH4                                                           
         CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    TMTCH2                                                           
TMTCH4   LR    R7,R6               R7 IS END OF SRCH                            
         BCTR  R7,0                                                             
TMTCH6   CLI   0(R5),X'10'         TEST AFFID                                   
         BE    NEQXIT              EXIT WITH CC NOT EQ IF MATCHED               
         ZIC   R6,1(R5)                                                         
         BXLE  R5,R6,TMTCH6                                                     
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO ADD A BUY LINE WITH A PRE-ASSIGNED NUMBER TO FILE              
*                                                                               
LINADD   NTR1                                                                   
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    LINADD1                                                          
         GOTO1 GOGETCTA,DMCB,('CIBADDQ',AREC)                                   
*                                                                               
LINADD1  BRAS  RE,CHKRSN           CHECK RSNCODE PRESENT IF REQ'D               
*                                                                               
         GOTO1 LOOKLIN,1                                                        
         BE    LINADD2             FOUND A LINE ALREADY THERE                   
*                                                                               
         BAS   RE,BLDPST           DELETE OLD PST & ADD FOR NEW STATION         
*                                                                               
         BRAS  RE,CKEXISTS                                                      
         GOTO1 ADDREC                                                           
*                                                                               
         OI    SVUPDATE,X'80'      SET NEW BUY ADDED                            
         MVC   SVKEY+14(4),KEY+14  SAVE D/A                                     
*                                                                               
         BRAS  RE,DARBATCH                                                      
         B     LINADDX                                                          
*                                                                               
LINADD2  SR    RE,RE                                                            
         CLI   BRDLST,0            TEST FOR ANYTHING IN PRODUCT LIST            
         BE    LINADD3                                                          
         LA    RE,PRDLIST                                                       
         XC    PRDLIST,PRDLIST     MOVE LIST TO PRDLIST SO BASE                 
         MVC   PRDLIST(L'BRDLST),BRDLST  WILL BE HAPPY                          
*                                                                               
LINADD3  ST    RE,DMCB+20                                                       
         GOTO1 PUTREC                                                           
*                                                                               
LINADD4  TM    KEY+L'BUYKEY,X'80'  TEST FOR DELETED BUY                         
         BZ    LINADD6             NO                                           
         NI    KEY+L'BUYKEY,X'FF'-X'80' TURN OFF DELETE BIT                     
         MVC   COMMAND(5),=C'DMWRT' WRITE DIRECTORY ENTRY BACK                  
         MVI   GBYACT,C'W'          TELL SPGETBUY IT'S DMWRT                    
         GOTO1 DIR                                                              
*                                                                               
LINADD6  MVC   SVKEY+14(4),KEY+14  SAVE D/A                                     
*                                                                               
LINADDX  B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A PRODUCT LIST FOR PUTREC LOGIC                          
* CALLED BY LINADD                                                              
*                                                                               
SETPRDL  NTR1                                                                   
         LA    R4,BUELPRD                                                       
         BAS   RE,TSTPRD                                                        
         LA    R4,BUELPR2                                                       
         BAS   RE,TSTPRD                                                        
         B     EXIT                                                             
*                                                                               
TSTPRD   NTR1                                                                   
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
TSTPRD2  BAS   RE,NEXTEL                                                        
         BNE   TSTPRD4                                                          
         ZIC   R0,1(R6)                                                         
         AHI   R0,-10                                                           
         BNP   TSTPRD2                                                          
         SRL   R0,2                                                             
         LA    R7,10(R6)                                                        
         CLC   0(1,R7),0(R4)       SAME PRD                                     
         BE    EXIT                                                             
         LA    R7,4(R7)                                                         
         BCT   R0,*-14                                                          
* PRD NOT IN ANY ELEMENT - SEE IF IN LIST YET                                   
TSTPRD4  LA    R7,BRDLST                                                        
*                                                                               
TSTPRD6  CLI   0(R7),0                                                          
         BE    TSTPRD8                                                          
         CLC   0(1,R7),0(R4)                                                    
         BE    EXIT                                                             
         LA    R7,1(R7)                                                         
         B     TSTPRD6                                                          
* ADD PRD TO LIST                                                               
TSTPRD8  MVC   0(1,R7),0(R4)                                                    
         B     EXIT                                                             
         EJECT                                                                  
* COPY A BUY. FORMAT IS COPY,(CLT),(PRD),WABC,EST,LIN,M=PRD,DEM=2/.../3         
* $=0 (NOCOST OPTION SO COSTS ARE NOT DUPLICATEDWHEN COPIED)                    
* STA,EST,LIN MAY BE OMITTED (EQUIVALENT TO REPEAT)                             
* ON CHANGE OF EST, NEW EST DATES BECOME BUY PERIOD DATES                       
         SPACE 2                                                                
B400     TM    SVOPT1,SVOPT1_VNDRLCK  X'10' - VENDOR LOCKED?                    
         BNZ   B202                   YES, NO NEW BUYS                          
         ST    R2,ABUYINPH         SAVE INPUT FLDHDR ADDRESS                    
         MVI   ERRCD,NEWERRS                                                    
*                                                                               
         MVI   COPYOPT,0                                                        
         XC    WORK2,WORK2                                                      
         MVC   FRKEY,SVKEY         SAVE DEFAULT KEY/DA                          
         MVC   FRCLI,QCLT                                                       
         MVC   FRCLITYP,SVCPROF+0                                               
         MVC   FRPRD,QPRD                                                       
         MVC   FRESTST(6),SVSTARTB NEED EST DATES IF TO EST=FR EST              
         MVC   QFRMSTA,QSTA        SET DEFAULT STATION                          
         CLI   FRCLITYP,C'0'       TEST BRAND POOL CLIENT                       
         BE    B401                NO                                           
         MVC   FRPRDNI,SVPRD       YES-SAVE BRAND IN KEY                        
         CLI   SVPOLPRD,0          TEST BRAND POOL BY BRAND                     
         BE    B401                NO                                           
         MVC   FRPRDNI,SVPOLPRD    YES-SAVE ACTUAL BRAND                        
*                                                                               
B401     GOTO1 FLDVAL                                                           
*                                                                               
         CLC   =C'M=',0(R4)                                                     
         BE    B403                                                             
         CLC   =C'P=',0(R4)                                                     
         BE    B403                                                             
         CLC   =C'H=',0(R4)                                                     
         BE    B403                                                             
         CLC   =C'N=',0(R4)                                                     
         BE    B403                                                             
         CLC   =C'COM=',0(R4)                                                   
         BE    B403                                                             
         CLC   =C'DEM=',0(R4)                                                   
         BE    B403                                                             
         CLC   =C'ID=',0(R4)                                                    
         BE    B403                                                             
         CLC   =C'ORB=',0(R4)                                                   
         BE    B403                                                             
         CLC   =C'$0',0(R4)                                                     
         BE    B403                                                             
         LTR   R5,R5                                                            
         BNZ   B404                                                             
         EJECT                                                                  
*========================================================*                      
* STA,EST,LIN NOT INPUT SO MUST HAVE HAD VALID RECALL    *                      
*========================================================*                      
         SPACE 1                                                                
B403     MVI   ERRCD,NORECALL                                                   
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    BUYERR                                                           
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         MVC   FRSTLIN,FRLIN       COPY FROM START=END=PREVIOUSLY               
         MVC   FRENDLIN,FRLIN      RECALLED LINE                                
*                                                                               
         CLC   =C'M=',0(R4)                                                     
         BE    B452                                                             
         CLC   =C'P=',0(R4)                                                     
         BE    B470                                                             
         CLC   =C'H=',0(R4)                                                     
         BE    B454                                                             
         CLC   =C'N=',0(R4)                                                     
         BE    B456                                                             
         CLC   =C'COM=',0(R4)                                                   
         BE    B458                                                             
         CLC   =C'DEM=',0(R4)                                                   
         BE    B460                                                             
         CLC   =C'ID=',0(R4)                                                    
         BE    B458                                                             
         CLC   =C'ORB=',0(R4)                                                   
         BE    B458                                                             
         CLC   =C'$0',0(R4)                                                     
         BE    B476                                                             
         B     B480                                                             
         EJECT                                                                  
* EDIT FOR A CLIENT FIRST - IF FIELD IS NOT A CLIENT, THEN GO                   
* DO STATION EDIT                                                               
*                                                                               
B404     XC    FLEN,FLEN           RE-EDIT FIRST FIELD AFTER ACTION             
         GOTO1 FLDVAL                                                           
*                                                                               
         MVI   ERRCD,INVERR                                                     
         MVC   DUB,SPACES                                                       
         CLI   FLEN+1,2            CLIENT CODE IS 2-3 CHARACTERS                
         BL    BUYERR1                                                          
         MVC   DUB(2),0(R4)        EXTRACT CODE                                 
         BE    B405                                                             
         MVC   DUB(3),0(R4)                                                     
         CLI   FLEN+1,3                                                         
         BNH   B405                                                             
         XC    FLEN,FLEN           NOT A CLIENT, RE-EDIT FIELD                  
         B     B415                FOR A STATION                                
*                                                                               
B405     MVC   FRCLI,DUB           SET CLIENT CODE                              
         OC    VCLPACK,VCLPACK     TEST IF ADDRESS IS THERE                     
         BNZ   B406                YES                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VCLPACK,DMCB                                                     
*                                                                               
B406     GOTO1 VCLPACK,DMCB,FRCLI,HALF                                          
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         XC    FLEN,FLEN           TRY STATION EDIT                             
         B     B415                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   CKEYAM,FRAGYMED                                                  
         MVC   CKEYCLT,HALF                                                     
         GOTO1 HIGH                                                             
         CLC   CKEY,KEYSAVE                                                     
         BE    B406A                                                            
         CLI   FLEN+1,2            TEST 2 CHARACTERS                            
         BE    BUYERR1             YES-MUST BE ERROR                            
         XC    FLEN,FLEN                                                        
         B     B415                NO-TRY STATION EDIT                          
*                                                                               
B406A    MVC   FRCLT,HALF          SET PACKED CLIENT                            
         L     R0,AREC             SAVE IO POINTER                              
         L     R6,AREC4            READ FROM CLIENT INTO IO4                    
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
         MVC   FRCLITYP,CPROF+0                                                 
*&&DO                                                                           
B407     OC    T211FFD+6(2),T211FFD+6 TEST ANY SECURITY                         
         BZ    B412                NO                                           
         CLI   T211FFD+6,C'+'      TEST MARKET LOCKOUT                          
         BE    B412                YES                                          
         MVI   ERRCD,SCRTYERR                                                   
         CLI   T211FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    B408                YES                                          
         CLI   T211FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BE    B410                YES                                          
         CLC   T211FFD+6(2),FRCLT   APPLY CLIENT FILTER                         
         BE    B412                CHECKS OUT                                   
         B     BUYERR1                                                          
*                                                                               
B408     CLC   T211FFD+7(1),COFFICE                                             
         BNE   BUYERR1                                                          
         B     B412                                                             
*&&                                                                             
*                                                                               
*        OFFICE LIST SECURITY                                                   
*                                                                               
B410     XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' OFFICER                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'         TEST ERROR                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T211FFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,FRCLI                                                     
         MVC   OFCSAGMD,FRAGYMED                                                
         MVC   OFCLMT(4),T211FFD+6                                              
         MVC   OFCACCSC(3),CACCESS ACCESS LIST FROM CLTREC                      
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB             RF=A(OFFICER)                                
         GOTO1 (RF),DMCB,(C'N',WORK),VCOMFACS                                   
         CLI   0(R1),0                                                          
         BE    B412                                                             
         MVI   ERRCD,SCRTYERR                                                   
         B     BUYERR1                                                          
         EJECT                                                                  
* EDIT FOR A VALID PRODUCT CODE                                                 
*                                                                               
B412     GOTO1 FLDVAL                                                           
         MVC   FRPRD,SPACES                                                     
         MVI   ERRCD,INVERR                                                     
         CLI   FLEN+1,2            PRODUCT MUST BE 2-3 CHARACTERS               
         BL    BUYERR1                                                          
         MVC   FRPRD(2),0(R4)                                                   
         BE    B413                                                             
         CLI   FLEN+1,3                                                         
         BH    BUYERR1                                                          
         MVC   FRPRD(3),0(R4)                                                   
*                                                                               
B413     MVI   ERRCD,PRDERR                                                     
         CLC   =C'AAA',FRPRD                                                    
         BE    BUYERR1                                                          
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   B414                                                             
         CLI   BUYMD,C'N'          TEST NETWORK                                 
         BNE   B414                                                             
         CLC   =C'POL',FRPRD       MUST BE POOL                                 
         BNE   BUYERR1                                                          
*                                                                               
B414     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         MVC   PKEYAM,FRAGYMED                                                  
         MVC   PKEYCLT,FRCLT                                                    
         MVC   PKEYPRD,FRPRD                                                    
         GOTO1 HIGH                                                             
         CLC   PKEY,KEYSAVE                                                     
         BNE   BUYERR1                                                          
*                                                                               
         L     R0,AREC             SAVE IO AREA POINTER                         
         L     R6,AREC5            READ FROM PRODUCT INTO IO5                   
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC             RESTORE IO POINTER                           
*                                                                               
         MVC   FRPRDN,PCODE+1      EXTRACT PRODUCT NUMBER                       
         MVC   FRPRDNI,PCODE+1     SAVE INPUT PRODUCT NUMBER                    
         CLC   =C'POL',FRPRD       TEST POL INPUT                               
         BE    B415                YES                                          
         CLI   FRCLITYP,C'0'       TEST FROM CLIENT IS BRAND POOL               
         BE    B415                NO                                           
         MVI   FRPRDN,X'FF'        YES-FORCE FROM BRAND TO POOL                 
         EJECT                                                                  
* EDIT STATION                                                                  
*                                                                               
B415     DS    0H                                                               
         GOTO1 FLDVAL              SET FOR LENGTH OF STATION STRING             
* NOW USE STAVAL TO EDIT THAT STRING                                            
         MVI   ERRCD,STAERR                                                     
         L     R5,AREC2                                                         
         USING STABLKD,R5                                                       
         XC    0(STBLNQ,R5),0(R5)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,BUYMD        SET MEDIA                                    
         ST    R4,STBADDR                                                       
         OI    STBADDR,X'80'       SET STRING OPTION                            
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,VCOMFACS                                                 
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A68'   STAVAL                                  
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(R5)                                                   
         CLI   STBERR,0                                                         
         BNE   BUYERR                                                           
         MVC   WORK+4(5),STBSTA      SET OUTPUT STATION                         
         CLI   WORK+8,C' '                                                      
         BH    *+8                                                              
         MVI   WORK+8,C'T'                                                      
         MVC   QFRMSTA,WORK+4       SAVE EBCDIC STATION                         
         MVC   WORK+9(3),STBNET     AND NETWORK                                 
*                                                                               
B418     CLC   WORK+4(5),QSTA      TEST SAME STATION                            
         BNE   B418A                                                            
         CLC   WORK+9(3),QCBLNET   TEST SAME NETWORK                            
         BNE   B418A                                                            
         CLC   FRCLI,QCLT          TEST SAME CLIENT                             
         BE    B420                                                             
         EJECT                                                                  
*                                                                               
* READ STATION MASTER REC FOR MARKET NUM                                        
*                                                                               
B418A    MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(5),WORK+4                                                  
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   KEY+9(3),FRCLI                                                   
         L     R0,AREC                                                          
         L     R6,AREC2                                                         
         USING STARECD,R6                                                       
         ST    R6,AREC                                                          
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   KEY(15),0(R6)                                                    
         BE    B419                                                             
         MVC   KEY+9(3),=C'000'                                                 
         GOTO1 STA                                                              
         CLC   KEY(15),0(R6)                                                    
         BNE   BUYERR1                                                          
B419     MVC   WORK(4),SMKT                                                     
         DROP  R6                                                               
*                                                                               
         ST    R0,AREC             RESTORE I/O ADDRESS                          
         GOTO1 STAPACK,DMCB,(C'P',WORK),WORK+4,FRMSTA                           
         EJECT                                                                  
* EDIT ESTIMATE *                                                               
*                                                                               
B420     MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,ESTERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         CLI   FLEN+1,3                                                         
         BH    BUYERR1                                                          
         TM    FVAL,X'08'                                                       
         BZ    BUYERR1                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4) *EXECUTED*                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR1                                                          
         CHI   R0,255                                                           
         BH    BUYERR1                                                          
         STC   R0,FREST                                                         
*                                                                               
         CLC   SVKEY+1(3),FRCLT    TEST SAME CLIENT/PRODUCT                     
         BNE   *+14                NO                                           
         CLC   SVKEY+9(1),FREST    TEST SAME EST                                
         BE    B425                                                             
         MVI   ERRCD,NOCOPYPB                                                   
* TEST EST ON FILE                                                              
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,FRAGYMED                                                  
         MVC   EKEYCLT,FRCLT                                                    
         MVC   EKEYPRD,FRPRD                                                    
         MVC   EKEYEST,FREST                                                    
         GOTO1 HIGH                                                             
         CLC   EKEY,KEYSAVE                                                     
         BNE   BUYERR1                                                          
*                                                                               
         L     R0,AREC                                                          
         L     R6,AREC2                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
* GET 3 BYTE START/END DATES                                                    
         GOTO1 VDATCON,DMCB,ESTART,(3,FRESTST)                                  
         GOTO1 (RF),(R1),EEND,(3,FRESTEND)                                      
* CHECK ESTIMATE FILTERS AGAINST F0 PROFILE                                     
         MVI   ERRCD,ESTFLTER                                                   
         L     RE,=A(SVF0PROF-BUYSAVE)                                          
         AR    RE,RA               POINT TO SAVED PROFILE                       
*                                                                               
         OC    0(16,RE),0(RE)                                                   
         BZ    B425                                                             
         CLI   SVCXTRA+3,C'N'      TEST FILTERS NOT REQD                        
         BE    B425                                                             
         CLI   2(RE),C'N'          IS THE FILTER REQUIRED                       
         BE    B421                                                             
         CLI   EPROF,C' '                                                       
         BNH   BUYERR1                                                          
*                                                                               
B421     CLI   3(RE),C'N'          IS THE FILTER REQUIRED                       
         BE    B422                                                             
         CLI   EPROF+1,C' '                                                     
         BNH   BUYERR1                                                          
*                                                                               
B422     CLI   4(RE),C'N'          IS THE FILTER REQUIRED                       
         BE    B425                                                             
         CLI   EPROF+2,C' '                                                     
         BNH   BUYERR1                                                          
         DROP  R6                                                               
         EJECT                                                                  
* EDIT LINE NUMBER(S)                                                           
*                                                                               
B425     MVI   FSTOPS+1,C'-'                                                    
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,LINERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         CLI   FSTOP,C'-'          TEST DASH STOPPED EDIT                       
         BE    B427                YES                                          
         TM    FVAL,X'04'          TEST ALPHA FIELD                             
         BZ    B427                NO                                           
*                                                                               
         LA    R0,1                                                             
         STH   R0,FRSTLIN                                                       
         LA    R0,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    R0,255                                                           
         STH   R0,FRENDLIN                                                      
*                                                                               
         CLI   FLEN+1,3                                                         
         BNE   BUYERR1                                                          
         CLC   =C'ALL',0(R4)       TEST COPY ALL LINES                          
         BNE   BUYERR1                                                          
         CLC   SVKEY(10),FRKEY     TEST SAME STA/EST                            
         BE    BUYERR1             YES-DO NOT ALLOW TO STOP LOOP                
         B     B428                                                             
*                                                                               
B427     BRAS  RE,VALLIN                                                        
         STH   R0,FRSTLIN          START LINE NUMBER                            
         STH   R0,FRENDLIN         END LINE NUMBER                              
         CLI   FSTOP,C'-'          TEST DASH FOUND                              
         BNE   B428                NO-ALL DONE WITH EDIT                        
         MVI   FSTOPS+1,0          GET REST OF FIELD                            
         GOTO1 FLDVAL                                                           
         LTR   R5,R5               TEST ANYTHING FOUND                          
         BZ    BUYERR1             NO                                           
         BRAS  RE,VALLIN                                                        
         STH   R0,FRENDLIN                                                      
         CLC   FRSTLIN,FRENDLIN    TEST START LINE L.T. END LINE                
         BNL   BUYERR1                                                          
         CLC   SVKEY(10),FRKEY     TEST SAME STA/EST                            
         BE    BUYERR1             DO NOT ALLOW RANGE COPY                      
*                                                                               
B428     MVI   FSTOPS+1,0                                                       
         B     B450                                                             
         EJECT                                                                  
* EDIT OPTIONAL PARAMS                                                          
*                                                                               
B450     CLI   FSTOP,C','                                                       
         BNE   B480                                                             
         GOTO1 FLDVAL                                                           
*                                                                               
* EDIT M=                                                                       
*                                                                               
B452     CLC   =C'M=',0(R4)                                                     
         BNE   B454                                                             
         MVI   ERRCD,BADMEQL                                                    
* EDIT MASPRD                                                                   
         XC    BUELDATA,BUELDATA                                                
         LA    R4,2(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         MVI   ERRCD,BADMAS                                                     
         TM    BUELPRSW,X'E7'      ANY BITS BUT FREE RIDER                      
         BNZ   BUYERR1                                                          
         CLI   BUELPRD,0           TEST M=UNALL                                 
         BNE   B453                NO-ALLOCATION WAS INPUT                      
         CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BNE   BUYERR1             YES - ERROR                                  
         MVI   UNALL,C'Y'          NOTE UNALLOCATION ENTERED                    
         B     B450                                                             
*                                                                               
B453     CLI   SVCPROF+0,C'0'      TEST BRAND POOL                              
         BE    B450                NO                                           
         CLI   BUELPRD,X'FF'       YES-PREVENT POL FROM BEING                   
         BE    BUYERR1             ENTERED AS MASTER PRD                        
         CLI   BUELPRD+1,X'FF'                                                  
         BE    BUYERR1                                                          
         CLI   SVPOLPRD,0          TEST COPYING TO BRAND POOL BY BRAND          
         BNE   BUYERR1             YES-ALREADY HAVE PRODUCT ALLOCATION          
         B     B450                                                             
*                                                                               
* EDIT H=                                                                       
*                                                                               
B454     CLC   =C'H=',0(R4)                                                     
         BNE   B456                                                             
         MVI   ERRCD,INVERR                                                     
         CLC   FRSTLIN,FRENDLIN    TEST FOR SINGLE LINE COPY                    
         BNE   BUYERR1             NO-HIATUS IS NOT A VALID OPTION              
         LA    R4,2(R4)            POINT BEYOND H=                              
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         BRAS  RE,EDHIAT                                                        
         B     B450                                                             
*                                                                               
* EDIT N=                                                                       
*                                                                               
B456     CLC   =C'N=',0(R4)                                                     
         BNE   B458                                                             
         MVI   ERRCD,INVERR                                                     
         CLC   FRSTLIN,FRENDLIN    TEST FOR SINGLE LINE COPY                    
         BNE   BUYERR1             NO-NOT A VALID OPTION                        
         MVI   ERRCD,BADNEQ                                                     
         LA    R4,2(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         TM    FVAL,X'08'                                                       
         BZ    BUYERR1                                                          
         CLI   FLEN+1,1                                                         
         BH    BUYERR1                                                          
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR1                                                          
         STC   R0,NREPS                                                         
         B     B450                                                             
         EJECT                                                                  
* EDIT COM= *                                                                   
         SPACE 1                                                                
B458     CLC   =C'COM=SAME',0(R4)                                               
         BNE   *+12                                                             
         MVI   COMOPT,C'C'                                                      
         B     B450                                                             
         CLC   =C'ORB=SAME',0(R4)                                               
         BNE   *+12                                                             
         OI    OTHOPT,X'80'                                                     
         B     B450                                                             
         CLC   =C'ID=SAME',0(R4)                                                
         BNE   B460                                                             
         OI    OTHOPT,X'40'                                                     
         B     B450                                                             
*                                                                               
B460     MVI   ERRCD,CPYEXMPL                                                   
         CLC   =C'DEM=',0(R4)                                                   
         BNE   B470                                                             
         LA    R4,4(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         CLC   =C'SAME',0(R4)                                                   
         BNE   B464                                                             
         MVI   DEMOPT,C'S'                                                      
* CHECK CANAD SPOT TV                                                           
*****    CLI   BUYMD,C'T'                                                       
*****    BNE   B450                                                             
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    B462                                                             
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    B462                                                             
         CLI   SVAPROF+7,C'C'                                                   
         BNE   B450                                                             
B462     MVI   ERRCD,CPYSPLL                                                    
         CLC   FRMSTA,SVKEY+4      TEST SAME STATION                            
         BE    B450                                                             
*                                                                               
         CLI   SVCXTRA+5,C'D'      IF DPT SUM SPILL, GIVE THEM ERROR            
         BE    BUYERR1                                                          
* IF NORMAL SPILL, TEST IF SPILL RECORD FOR EITHER STATION                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D13'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(1),SVCPROF+3                                               
         MVC   KEY+5(4),QSTA                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    BUYERR1             'TO' STATION HAS SPILL                       
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+5(4),QFRMSTA                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    BUYERR1                                                          
         B     B450                                                             
         SPACE 1                                                                
* EDIT DEMO OVERRIDES *                                                         
         SPACE 1                                                                
B464     MVI   ERRCD,INVERR                                                     
         CLC   FRSTLIN,FRENDLIN    TEST FOR SINGLE LINE COPY                    
         BNE   BUYERR1             NO-DO NOT ALLOW OVERRIDES                    
         MVI   EDTVAL,DEMEDT                                                    
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         LA    R7,BUDEMS+24                                                     
         MVI   DEMOPT,C'D'         SET DEMOS ENTERED FLAG                       
         SPACE 1                                                                
B465     MVC   SPDEMTYP,1(R7)      SET DEMO TYPE                                
         GOTO1 CALLEDT                                                          
         OC    FLEN,FLEN                                                        
         BZ    B466                                                             
         MVI   3(R7),100           FORCE SVI                                    
         MVC   4(4,R7),BUNEWDEM                                                 
         OI    4(R7),X'80'                                                      
B466     CLI   FSTOP,C'/'                                                       
         BNE   B450                                                             
         LA    R7,8(R7)                                                         
B467     CLI   1(R7),0                                                          
         BNE   B465                                                             
         MVI   ERRCD,MAXDEMS                                                    
         B     BUYERR1                                                          
         EJECT                                                                  
* EDIT BUY LINE PERIOD OVERRIDE (DEFAULT IS 'TO' ESTIMATE PERIOD)               
*                                                                               
* DATE-DATE OR DATE-E                                                           
*                                                                               
B470     CLC   =C'P=',0(R4)                                                     
         BNE   B476                                                             
******   BNE   BUYERR1                                                          
         LA    R4,2(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   FSTOPS,C'-'         LOOK FOR DASH                                
         GOTO1 FLDVAL                                                           
*                                                                               
         MVI   ERRCD,PERERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         GOTO1 VDATVAL,DMCB,(1,(R4)),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR1                                                          
         CLC   FLEN+1(1),3(R1)     TEST DATE MAKES UP WHOLE FIELD               
         BNE   BUYERR1                                                          
         MVC   DUB(2),SVSTART                                                   
         CLC   SVSTART(2),SVEND    TEST ESTIMATE IS IN 1 YEAR                   
         BE    B471                                                             
         CLC   DUB+2(4),SVSTART+2  NO-TEST INPUT MMDD L.T. EST ST.              
         BNL   *+10                                                             
         MVC   DUB(2),SVEND        YES-DATE MUST BE IN END YEAR                 
*                                                                               
B471     GOTO1 VDATCON,DMCB,DUB,(3,PERST)                                       
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         CLI   FLEN+1,1                                                         
         BNE   B472                                                             
         CLI   0(R4),C'E'                                                       
         BNE   BUYERR1                                                          
         GOTO1 VDATCON,DMCB,SVEND,(3,PEREND)                                    
         B     B475                                                             
*                                                                               
B472     GOTO1 VDATVAL,DMCB,(1,(R4)),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR1                                                          
         CLC   3(1,R1),FLEN+1      TEST DATE MAKES UP WHOLE FIELD               
         BNE   BUYERR1                                                          
         MVC   DUB(2),=C'01'                                                    
         GOTO1 VDATCON,DMCB,DUB,(3,PEREND)                                      
         MVC   PEREND(1),PERST     SET END DATE YEAR=START DATE YEAR            
         CLC   PEREND+1(2),PERST+1  TEST END DATE L.T. START DATE               
         BNL   B475                                                             
         ZIC   R1,PEREND           BUMP THE YEAR AHEAD                          
         LA    R1,1(R1)                                                         
         CHI   R1,100                                                           
         BL    *+6                                                              
         SR    R1,R1                                                            
         STC   R1,PEREND                                                        
*                                                                               
B475     MVI   ERRCD,STENDERR                                                   
         CLC   PERST,PEREND         TEST START L.T. END                         
         BH    BUYERR1                                                          
         MVI   ERRCD,ESPERERR                                                   
         CLC   PERST,SVSTARTB        TEST OVERRIDE DATES W/IN                   
         BL    BUYERR1               ESTIMATE                                   
         CLC   PEREND,SVENDB                                                    
         BH    BUYERR1                                                          
         B     B450                                                             
         EJECT                                                                  
* EDIT $0 NOCOST OPTION - SET COST = 0 ON COPIED LINES                          
*                                                                               
B476     CLC   =C'$0',0(R4)                                                     
         BNE   BUYERR1                                                          
         LA    R4,2(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         OI    COPYOPT,CONOCOST    SET NOCOST OPTION ON                         
         B     B450                                                             
         EJECT                                                                  
* FOR MULTIPLE LINE COPY, CHECK THAT THERE ARE NO MORE TRANSACTIONS             
* ON SCREEN                                                                     
*                                                                               
B480     CLC   FRSTLIN,FRENDLIN                                                 
         BE    B482                NO-SINGLE LINE COPY                          
         SR    R0,R0                                                            
         LR    RF,R2               RF=SCREEN POINTER                            
*                                                                               
B481     IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         C     RF,FLAST            TEST IF PAST LAST INPUT LINE                 
         BH    B482                YES                                          
         CLI   5(RF),0             TEST FOR NO INPUT                            
         BE    B482                                                             
         CLI   8(RF),C'*'          TEST FIELD START WITH NO-OP                  
         BE    B482                                                             
*                                                                               
         LR    R2,RF               SET R2=ERROR FIELD HEADER                    
         MVI   ERRAREA,X'FF'       INDICATE CUSTOM ERROR MESSAGE                
         MVC   BUYMSG(L'OTHERMSG),OTHERMSG                                      
         B     BUYERR                                                           
*                                                                               
* REQUIRE MASTER ALLOCATION IF COPYING TO BRAND POOL CLIENT - FORCE             
* ALLOCATION IF COPYING TO BRAND POOL CLIENT BY BRAND                           
*                                                                               
B482     CLI   SVCPROF+0,C'0'      TEST TO CLIENT=BRAND POOL                    
         BE    B485                NO                                           
         CLI   SVPOLPRD,0          TEST BRAND POOL BY BRAND                     
         BE    *+14                NO                                           
         MVC   BUELPRD(1),SVPOLPRD YES-FORCE ALLOCATION TO HEADLINE             
         B     B485                BRAND                                        
*                                                                               
         MVI   ERRCD,NOMASPRD      NO-REQUIRE MASTER ALLOCATION                 
         CLI   BUELPRD,0                                                        
         BE    BUYERR                                                           
*                                                                               
B485     OC    PERST(6),PERST      TEST IF BUY PERIOD OVERRIDE INPUT            
         BNZ   *+10                YES                                          
         MVC   PERST(6),SVSTARTB   NO-DEFAULT IS TO ESTIMATE PERIOD             
*                                                                               
* READ THE FILE FOR RECORDS TO COPY                                             
*                                                                               
B490     XC    KEY,KEY                                                          
         MVC   KEY(10),FRKEY       A-M/CLT/PRD/MKT/STA/EST                      
         MVC   KEY+11(2),FRSTLIN   START LINE NUMBER                            
         MVI   DMINBTS,0           MAKE SURE DO NOT GET DELETES                 
         GOTO1 HIGH                                                             
         B     B500                                                             
*                                                                               
B495     XC    KEY,KEY                                                          
         MVC   KEY(L'BUYKEY),FRKEY                                              
         SR    R1,R1                                                            
         ICM   R1,3,KEY+11                                                      
         LA    R1,1(R1)            INCREMENT LINE NUMBER                        
         LA    R0,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    R0,255                                                           
         CR    R1,R0                                                            
         BH    B600                EXCEEDED MAXIMUM-DONE WITH READ              
         STCM  R1,3,KEY+11                                                      
         MVI   DMINBTS,0           MAKE SURE DO NOT GET DELETES                 
         GOTO1 HIGH                                                             
*                                                                               
B500     CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   B600                        STA/EST                              
         CLC   KEY+11(2),FRENDLIN  TEST LINE NUMBER VS MAXIMUM NUMBER           
         BH    B600                YES-ALL DONE                                 
*                                                                               
         MVC   FRKEY(L'BUYKEY),KEY SAVE LAST KEY                                
         GOTO1 GETREC                                                           
         CLC   SVKEY+3(1),BUYKEY+3 TEST POL TO NON-POL OR VICE-VERSA            
         BE    B501                NO                                           
         MVI   ERRCD,BADCOPY                                                    
         CLI   SVKEY+3,X'FF'       TEST EITHER 'FROM' OR 'TO' IS POL            
         BE    COPYR                                                            
         CLI   BUYKEY+3,X'FF'                                                   
         BE    COPYR                                                            
*                                                                               
B501     CLI   FRCLITYP,C'0'       TEST COPYING FROM BRAND POOL CLT             
         BE    B502                NO                                           
         CLC   =C'POL',FRPRD       TEST COPYING UNDER 'POOL'                    
         BE    B502                YES                                          
         CLC   BDMASPRD(1),FRPRDNI TEST IF MASTER BRAND=FROM BRAND              
         BNE   B495                NO-READ NEXT RECORD                          
*                                                                               
* INITIALIZE THE BUY RECORD FOR COPYING                                         
*                                                                               
B502     TM    SVOPT2,X'40'         TEST NO TAX OPTION                          
         BO    *+10                                                             
         MVC   BDNTAX,SVNTAX       SET TAX FOR NEW STA                          
         CLI   UNALL,C'Y'          TEST FOR UN-ALLOCATION                       
         BE    *+14                YES                                          
         CLC   QCLT,FRCLI          TEST 'TO' CLIENT='FROM' CLIENT               
         BE    B504                YES                                          
         XC    BDMASPRD,BDMASPRD   NO-CLEAR MASTER ALLOCATION                   
*                                                                               
B504     XC    BDMGDATE,BDMGDATE   CLEAR MAKE-GOOD LINE FIELDS                  
         MVI   BDMGSPOT,0                                                       
         MVI   BDCHG,0             RESET CHANGE IND                             
         MVC   BDREP,SVESTREP      TAKE REP FROM TO EST (OR NONE)               
         TM    COPYOPT,CONOCOST    TEST NOCOST OPTION ON                        
         BNO   *+10                                                             
         XC    BDCOST,BDCOST                                                    
*                                                                               
         BRAS  RE,SETRATE          SET RATE TYPES IF NEEDED                     
*                                                                               
         CLI   BDTIME,0            TEST NON-POL P/B                             
         BE    B520                NO                                           
         CLC   FRKEY(4),SVKEY      TEST SAME CLIENT/PRODUCT                     
         BE    *+12                YES                                          
         MVI   ERRCD,NOCOPYPB      STOP CROSS CLIENT COPY                       
         B     COPYR                                                            
*                                                                               
         CLC   FREST,SVKEY+9       TEST NEW EST                                 
         BE    B520                NO                                           
* PARTNER EST MUST HAVE SAME DATES AS COPY TO ESTIMATE                          
* FIRST FIND PBELEM                                                             
         LA    R6,BDELEM                                                        
B512     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),4                                                          
         BNE   B512                                                             
*                                                                               
         MVC   3(1,R6),SVKEY+9     SET NEW EST IN PBELEM                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY      A-M/CLT                                      
         MVC   KEY+4(3),6(R6)      PRD                                          
         MVC   KEY+7(1),SVKEY+9    EST                                          
*                                                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,PTNRDTS                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   COPYR                                                            
         L     R0,AREC                                                          
         L     R6,AREC2                                                         
         ST    R6,AREC                                                          
         USING ESTHDRD,R6                                                       
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
         CLC   SVSTART(12),ESTART                                               
         BNE   COPYR                                                            
         DROP  R6                                                               
*                                                                               
* SET POL ELEMENT LENGTHS                                                       
*                                                                               
B520     CLI   BUYKEY+3,X'FF'                                                   
         BNE   B525                                                             
         OC    BUELPRD,BUELPRD                                                  
         BZ    *+10                                                             
         MVC   BDMASPRD,BUELPRD                                                 
         EJECT                                                                  
* CHECK BRAND ESTIMATES ON FILE                                                 
*                                                                               
B522     CLI   BDMASPRD,0                                                       
         BE    B525                                                             
         LA    R4,BDMASPRD                                                      
         BRAS  RE,CHKEST                                                        
         BNE   COPYR                                                            
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BE    B525                                                             
         BRAS  RE,CHKEST                                                        
         BNE   COPYR                                                            
*                                                                               
* DELETE ALL ELEMENTS EXCEPT BDELEM/DEMEL/PBELEM/DMLKUPEL(X'24')                
*                            PSTELEM,COS2ELEM                                   
*                                                                               
B525     MVI   ELCDLO,5                                                         
         MVI   ELCDHI,X'FF'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
B526     BAS   RE,NEXTEL                                                        
         BNE   B530                                                             
*                                                                               
B527     CLI   0(R6),X'66'         TEST COMMENT                                 
         BNE   *+12                                                             
         CLI   COMOPT,C'C'         TEST COM=SAME                                
         BE    B526                YES - DONT DELETE                            
*                                                                               
         CLI   0(R6),X'67'         TEST ORBIT                                   
         BNE   *+12                                                             
         TM    OTHOPT,X'80'        TEST ORB=SAME                                
         BO    B526                                                             
*                                                                               
         CLI   0(R6),X'70'         TEST ID                                      
         BNE   *+12                                                             
         TM    OTHOPT,X'40'        TEST ID=SAME                                 
         BO    B526                                                             
*                                                                               
         CLI   0(R6),X'71'         TEST COST2 ELEMENT                           
         BNE   B528                                                             
         TM    COPYOPT,CONOCOST    TEST NOCOST OPTION ON                        
         BZ    B526                NO - DO NOT DELETE ELEMENT                   
         BAS   RE,ELEMDEL                                                       
         BAS   RE,NEXTEL2                                                       
         BE    B527                                                             
*                                                                               
B528     CLI   0(R6),X'24'         TEST DEMO LOOKUP OVERRIDE                    
         BE    B526                                                             
*                                                                               
         BAS   RE,ELEMDEL                                                       
         BAS   RE,NEXTEL2                                                       
         BE    B527                                                             
*                                                                               
* ID ELEMENT, SPILL DEMOS                                                       
*                                                                               
B530     DS    0H                                                               
         BAS   RE,TESTID           TEST ID PRESENT IF REQ'D                     
         CLI   DEMOPT,C'S'         TEST IF KEEPING SAME DEMOS                   
         BE    B532                YES                                          
*                                                                               
* DELETE SPILL DEMELS                                                           
*                                                                               
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   B532                                                             
*                                                                               
B531     BAS   RE,ELEMDEL                                                       
         BAS   RE,NEXTEL2                                                       
         BE    B531                                                             
*                                                                               
B532     DS    0H                                                               
         CLI   DEMOPT,C'S'                                                      
         BNE   B534                                                             
         MVI   BUDEMSW,0           FORCE REBUILD                                
         GOTO1 VBLDDEM                                                          
         B     B540                                                             
*                                                                               
B534     CLI   DEMOPT,C'D'         TEST DEMOS INPUT                             
         BE    B536                YES                                          
         CLC   SVKEY(4),FRKEY      TEST SAME CLIENT/PRODUCT                     
         BNE   B536                NO-CHANGE DEMOS                              
         CLC   SVKEY+6(4),FRKEY+6  TEST SAME STA/EST                            
         BE    B540                YES - LEAVE OLD DEMOS                        
                                                                                
* DELETE OLD DEMO EL AND INSERT NEW                                             
B536     MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         GOTO1 (RF),(R1),,BUDEMS,(R6)                                           
         XC    2(2,R6),2(R6)       RESET BOOK IN DEMO EL                        
*                                                                               
         MVI   BUDEMSW,0           FORCE REBUILD                                
         GOTO1 VBLDDEM                                                          
*                                                                               
* CALENDARIZE FOR NEW BUY DESC PERIOD UNLESS SAME EST DATES                     
*                                                                               
B540     MVC   BUYKCLT,SVCLT       SET NEW KEY VALUES                           
         MVC   BUYKPRD,SVPRD                                                    
         MVC   BUYMSTA,SVMKT       MARKET/STATION                               
         MVC   BUYKEST,SVEST                                                    
         CLC   PERST(6),FRESTST    TEST NEW BUY PER=FROM EST DATES              
         BE    B550                                                             
         MVC   BDSTART(6),PERST    NEW BUY LINE DATES                           
         EJECT                                                                  
*========================================================*                      
* NOW CHANGE DATES TO AGREE WITH BUY DESC DAYS           *                      
* FIRST GET START AND END DAY NUMBERS                    *                      
*========================================================*                      
                                                                                
         ZIC   R0,BDSEDAY                                                       
         STC   R0,BUDAYXIN         SET END DATE DAY                             
         NI    BUDAYXIN,X'0F'      DROP START DAY                               
         SRL   R0,4                                                             
         STC   R0,BUDAY1IN         SET START DATE DAY                           
         CLI   BDSEDAY,0           TEST NEW RECORD FORMAT                       
         BNE   B542                YES - CONTINUE                               
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,BDDAY                                                         
         SLL   R1,25                                                            
         SR    RE,RE               RESET DAY NUM COUNTER                        
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RE,*-10                                                          
         BCTR  RE,0                ADD 1                                        
         LPR   R0,RE                                                            
         STC   R0,BUDAY1IN                                                      
* NOW GET END DAY NUMBER                                                        
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         SLL   R1,1                                                             
         BCT   RE,*-10                                                          
         LPR   R0,RE                                                            
         STC   R0,BUDAYXIN                                                      
*                                                                               
B542     DS    0H                  DETERMINE OLD START DATE DAY                 
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
* ADVANCE START DATE TO CORRECT DAY                                             
         ZIC   R0,BUDAY1IN                                                      
         ZIC   RE,0(R1)                                                         
         SR    R0,RE                                                            
         BNM   *+8                                                              
         AHI   R0,7                ADVANCE TO PROPER DAY                        
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         GOTO1 VDATCON,DMCB,WORK+12,(3,BDSTART)                                 
*                                                                               
* END DATE WILL BE CORRECTED AFTER CALENDARIZATION                              
*                                                                               
B550     OC    BUELPRD(2),BUELPRD  TEST ALLOCATIONS ENTERED                     
         BZ    *+10                NO - USE OLD MASPRD (IF ANY)                 
         MVC   BDMASPRD,BUELPRD                                                 
*                                                                               
         MVC   BUELPRD(2),BDMASPRD  BLDEL USES BUELPRD                          
         GOTO1 VBLDEL                                                           
         BAS   RE,CALEND           RE-CALENDARIZE                               
         BAS   RE,CHKMAXEL                                                      
         BE    B550A                                                            
         CLC   =C'COP',BUTRCODE                                                 
         BE    COPYR                                                            
         CLC   =C'MOV',BUTRCODE                                                 
         BE    MOVER                                                            
         B     BUYERR                                                           
* MAKE SURE DATES IN NEW ESTIMATE PERIOD                                        
B550A    MVI   ERRCD,NOTINEST                                                   
         CLC   BDSTART(3),SVSTARTB TEST BUY STARTS IN NEW EST PERIOD            
         BL    BUYERR                                                           
         CLC   BDSTART(3),SVENDB                                                
         BH    BUYERR                                                           
*                                                                               
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    B551                                                             
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    B551                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   B552                                                             
* CANAD SPOT                                                                    
B551     CLI   DEMOPT,C'S'                                                      
         BE    B552                                                             
         LA    R7,SVKEY+4          POINT TO MKT/STA                             
         GOTO1 VGETSPLL                                                         
*                                                                               
B552     CLI   DEMOPT,C'S'                                                      
         BE    *+10                                                             
         L     RF,DEMLKUP                                                       
         BASR  RE,RF                                                            
         EJECT                                                                  
* CORRECT END DATE NOW - FIRST FIND LAST REGEL                                  
*                                                                               
         CLC   FRESTST(6),PERST    TEST IF BUYLINE RE-CALENDARIZED              
         BE    B555                                                             
*                                                                               
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R7,R6                                                            
         BAS   RE,NEXTEL                                                        
         BE    *-6                                                              
* SET BDEND TO LOWER OF LAST DAY OF ROTATOR OR EST END                          
         GOTO1 VDATCON,DMCB,(2,2(R7)),WORK                                      
         ZIC   R0,BUDAYXIN                                                      
         ZIC   RE,BUDAY1IN                                                      
         SR    R0,RE                                                            
         BNM   *+8                                                              
         AHI   R0,7                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)  GET DATE OF LAST ROT DAY          
         GOTO1 VDATCON,DMCB,WORK+12,(3,BDEND)                                   
*                                                                               
         CLC   BDEND,SVENDB       TEST PAST EST END                             
         BNH   *+10                                                             
         MVC   BDEND,SVENDB                                                     
*                                                                               
B555     MVI   BUWHY,X'84'                                                      
         LA    RF,NXTLIN                                                        
         CLI   RDNEXT,C'Y'         TEST FIRST NEW LINE READ FOR                 
         BE    *+12                                                             
         LA    RF,NXTBUYLN                                                      
         MVI   RDNEXT,C'Y'                                                      
         BASR  RE,RF                                                            
         GOTO1 SETCHGDT                                                         
         CLC   FRSTLIN,FRENDLIN    TEST FOR SINGLE LINE COPY                    
         BE    B270                YES-USE SAME EXIT AS NEW BUY                 
*                                                                               
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    B566                                                             
         GOTO1 GOGETCTA,DMCB,('CIBADDQ',AREC)                                   
         B     B567                                                             
*                                                                               
B566     BAS   RE,BLDID                                                         
*                                                                               
         OC    SVCCOST2,SVCCOST2                                                
         BNZ   *+14                                                             
         OC    SVECOST2,SVECOST2                                                
         BZ    B566X                                                            
         MVI   BUCOS2IND,X'80'     SET FLAG COST2 INPUT                         
         BRAS  RE,SETCOST2                                                      
*                                                                               
B566X    CLI   SVAPROF+7,C'C'                                                   
         BNE   B567                                                             
         BRAS  RE,BLDXCH                                                        
*                                                                               
         BAS   RE,BLDPST           DELETE OLD PST & ADD FOR NEW STATION         
*                                                                               
B567     BRAS  RE,CHKRSN           CHECK REASON CODE THERE IF NEEDED            
*                                                                               
         BRAS  RE,CKEXISTS                                                      
         GOTO1 ADDREC                                                           
*                                                                               
         OI    SVUPDATE,X'80'      SET NEW BUY ADDED                            
         MVC   SVKEY+14(4),KEY+14  SET DISK ADDRESS OF BUY JUST ADDED           
         GOTO1 VBLDQLST                                                         
         BRAS  RE,DARBATCH                                                      
         LH    R1,NCOPIES          INCREMENT COPIED RECORDS COUNT               
         LA    R1,1(R1)                                                         
         STH   R1,NCOPIES                                                       
         B     B495                READ NEXT RECORD                             
         EJECT                                                                  
* WRAP-UP CODE - DISPLAY MESSAGE WITH NUMBER COPIED AND ERRORS                  
*                                                                               
B600     LA    R4,BUYMSG                                                        
         MVC   0(16,R4),=C'**COPY COMPLETED'                                    
         LA    R4,17(R4)                                                        
         MVI   0(R4),C'-'                                                       
         LA    R4,2(R4)                                                         
         LH    R0,NCOPIES          GET NUMBER OF BUYS COPIED                    
         EDIT  (R0),(3,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         CLC   NCOPIES,=H'1'       TEST FOR ONE BUY COPIED                      
         BNE   B601                                                             
         MVC   0(15,R4),=C'BUY LINE COPIED'                                     
         LA    R4,15(R4)                                                        
         B     B602                                                             
*                                                                               
B601     MVC   0(16,R4),=C'BUY LINES COPIED'                                    
         LA    R4,16(R4)                                                        
*                                                                               
B602     MVC   0(2,R4),=C'**'                                                   
         OC    NERRS,NERRS         TEST FOR ANY ERRORS                          
         BNZ   B603                NO-ALL DONE                                  
* AFTER LAST COPY DONE, CLEAR THE REASON CODE !                                 
         LHI   RE,SVRSNEL-BUYSAVE                                               
         AR    RE,RA                                                            
         XC    0(69,RE),0(RE)                                                   
         B     B605                                                             
*                                                                               
B603     MVC   0(2,R4),SPACES                                                   
         LA    R4,1(R4)            POSITION OUTPUT POINTER FOR ERRORS           
         MVI   0(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
         LH    R0,NERRS            GET NUMBER OF ERRORS                         
         EDIT  (R0),(3,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(6,R4),=C'ERRORS'                                               
         MVC   6(2,R4),=C'**'                                                   
*                                                                               
B605     L     R2,ABUYINPH         POINT TO INPUT FLDHDR                        
         B     B278                PREPARE TO EXIT                              
         EJECT                                                                  
* ERROR HANDLING FOR ERRORS DURING COPY                                         
*                                                                               
COPYR    CLC   FRSTLIN,FRENDLIN    TEST FOR SINGLE LINE COPY                    
         BE    BUYERR              YES-TAKE REGULAR ERROR EXIT                  
         LH    R1,NERRS                                                         
         LA    R1,1(R1)            INCREMENT ERROR COUNT                        
         STH   R1,NERRS                                                         
         CLC   NERRS,=H'1'         TEST FOR FIRST ERROR                         
         BNE   COPYR2              NO                                           
         XC    BLDLIST,BLDLIST                                                  
         MVC   DSPAREA,SPACES                                                   
         MVC   BLDLIST(4),=X'141C2014'                                          
         LA    R6,DSPAREA                                                       
         ST    R6,BLDLIST+4                                                     
         MVC   DSPAREA(28),=C'**** ERRORS DURING COPY ****'                     
         GOTO1 VBLDFLD                                                          
*                                                                               
* BUILD A FIELD UNDERNEATH BUY LINE FIELDS CONTAINING LINE                      
* AND THE ERROR MESSAGE                                                         
*                                                                               
COPYR2   XC    BLDLIST,BLDLIST                                                  
         MVC   DSPAREA,SPACES                                                   
         MVC   BLDLIST(4),=X'01452001'                                          
         LA    R6,DSPAREA                                                       
         ST    R6,BLDLIST+4                                                     
         MVC   0(4,R6),=C'LINE'                                                 
         LA    R6,5(R6)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BUYKBUY                                                     
         EDIT  (R0),(3,(R6)),ALIGN=LEFT                                         
         AR    R6,R0                                                            
         LA    R6,1(R6)                                                         
         GOTO1 VGETMSG,DMCB+12,(ERRCD,(R6)),(X'FF',DMCB),(3,0)                  
         GOTO1 VBLDFLD                                                          
*                                                                               
         B     B495                RESUME READING NEXT BUY LINE                 
         EJECT                                                                  
*================================================================               
* MOVE A BUY. FORMAT IS MOVE,EST,LINE-LINE                                      
*================================================================               
                                                                                
B700     ST    R2,ABUYINPH         SAVE INPUT FLDHDR ADDRESS                    
         TM    SVOPT1,SVOPT1_VNDRLCK  X'10' - VENDOR LOCKED?                    
         BNZ   B202                   YES, NO NEW BUYS                          
*                                                                               
         XC    WORK2,WORK2                                                      
         MVC   FRKEY,SVKEY         SAVE DEFAULT KEY/DA                          
*                                                                               
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,ESTERR        EDIT ESTIMATE                                
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BUYERR1                                                          
         CP    DUB,=P'255'                                                      
         BH    BUYERR1                                                          
         CVB   R0,DUB                                                           
         STC   R0,FREST                                                         
         MVC   FRKEY+9(1),FREST                                                 
         CLC   FREST,SVKEY+9       MOVE TO SAME ESTIMATE                        
         BE    BUYERR1                                                          
*                                  TEST EST ON FILE                             
         MVC   BYTE,FREST                                                       
         MVC   FULL,=C'POL'                                                     
         BAS   RE,GEST             SEE IF EST ON FILE                           
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVI   ERRCD,MOVESERR                                                   
         CLI   SVPRD,X'FF'         IF 'TO' EST IS NOT POL                       
         BE    B710                                                             
         CLC   EKEY,KEYSAVE        AND POL ESTIMATE FOUND FOR 'FR' EST          
         BE    BUYERR1             THEN IT'S AN ERROR                           
         B     B715                                                             
                                                                                
B710     CLC   EKEY,KEYSAVE        IF 'TO' EST IS POL                           
         BNE   BUYERR1             AND THERE IS NO 'FR' POL EST - ERROR         
                                                                                
B715     MVC   FULL,QPRD           NOW SEE IF ACTUAL ESTIMATE IS FOUND          
         MVI   ERRCD,ESTERR                                                     
         MVC   BYTE,FREST                                                       
         BAS   RE,GEST             SEE IF EST ON FILE                           
         CLC   EKEY,KEYSAVE                                                     
         BNE   BUYERR1                                                          
                                                                                
         L     R6,AREC2                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
* GET 3 BYTE START/END DATES                                                    
         GOTO1 VDATCON,DMCB,ESTART,(3,FRESTST)                                  
         GOTO1 (RF),(R1),EEND,(3,FRESTEND)                                      
         MVI   ERRCD,INVESTDT                                                   
         CLC   FRESTST(6),SVSTARTB    TEST FR EST DATES = TO EST DATES          
         BNE   BUYERR1                                                          
         MVI   ERRCD,DEMOMAT                                                    
         CLC   SVDEMLST(60),EDEMLST   DEMOS HAVE TO BE EQUAL                    
         BNE   BUYERR1                                                          
         CLC   SVWGTLST,EWGTLST                                                 
         BNE   BUYERR1                                                          
         CLC   SVUSRNMS(35),EUSRNMS                                             
         BNE   BUYERR1                                                          
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================              
* EDIT LINE NUMBER(S)                                                           
*=================================================================              
                                                                                
         MVI   FSTOPS+1,C'-'                                                    
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,LINERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR1                                                          
*                                                                               
         BRAS  RE,VALLIN                                                        
         STH   R0,FRSTLIN          START LINE NUMBER                            
         STH   R0,FRENDLIN         END LINE NUMBER                              
         CLI   FSTOP,C'-'          TEST DASH FOUND                              
         BNE   B720                NO-ALL DONE WITH EDIT                        
         MVI   FSTOPS+1,0          GET REST OF FIELD                            
         GOTO1 FLDVAL                                                           
         LTR   R5,R5               TEST ANYTHING FOUND                          
         BZ    BUYERR1             NO                                           
         BRAS  RE,VALLIN                                                        
         STH   R0,FRENDLIN                                                      
         CLC   FRSTLIN,FRENDLIN    TEST START LINE L.T. END LINE                
         BNL   BUYERR1                                                          
*                                                                               
* READ THE FILE FOR RECORDS TO MOVE                                             
*                                                                               
B720     XC    KEY,KEY                                                          
         MVC   KEY(10),FRKEY         BUY KEY EXCEPT LINE NUMBER                 
         MVC   KEY+11(2),FRSTLIN     START LINE NUMBER                          
         B     B750                                                             
*                                                                               
B740     XC    KEY,KEY                                                          
         MVC   KEY(13),FRKEY                                                    
         SR    R1,R1                                                            
         ICM   R1,3,KEY+11         GET LINE NUMBER FROM DIRKEY                  
         LA    R1,1(R1)            INCREMENT LINE NUMBER                        
         SR    R0,R0                                                            
         ICM   R0,3,FRENDLIN                                                    
         CR    R1,R0                                                            
         BH    B1000               EXCEEDED LAST LINE-DONE WITH READ            
         STCM  R1,3,KEY+11                                                      
                                                                                
B750     MVI   DMINBTS,0           MAKE SURE DO NOT GET DELETES                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   B1000                                                            
         CLC   KEY+11(2),FRENDLIN  TEST LINE NUM VS MAX                         
         BH    B1000               YES-ALL DONE                                 
*                                                                               
         MVC   FRKEY(L'BUYKEY),KEY SAVE LAST KEY                                
         GOTO1 GETREC                                                           
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   B810                                                             
         OC    BUELPRD,BUELPRD                                                  
         BZ    *+10                                                             
         MVC   BDMASPRD,BUELPRD                                                 
         EJECT                                                                  
* CHECK BRAND ESTIMATES ON FILE                                                 
*                                                                               
         CLI   BDMASPRD,0                                                       
         BE    B810                                                             
         LA    R4,BDMASPRD                                                      
         BRAS  RE,CHKEST                                                        
         BNE   MOVER                                                            
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BE    B810                                                             
         BRAS  RE,CHKEST                                                        
         BNE   MOVER                                                            
                                                                                
B810     MVI   ELCDLO,X'06'        IF PAID - DON'T MOVE                         
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         MVI   ERRCD,PAIDERR                                                    
                                                                                
B820     BAS   RE,NEXTEL                                                        
         BNE   B840                                                             
         OC    4(2,R6),4(R6)                                                    
         BZ    B820                                                             
         B     MOVER                                                            
                                                                                
B840     MVI   ELCDLO,X'05'        IF PACKAGE - DON'T MOVE                      
         MVI   ELCDHI,X'05'                                                     
         LA    R6,BDELEM                                                        
         MVI   ERRCD,PCKGERR                                                    
                                                                                
B850     BAS   RE,NEXTEL                                                        
         BE    MOVER                                                            
                                                                                
B860     MVC   KEY,SVKEY                                                        
         MVI   BUWHY,X'80'         SET NEW BUY                                  
         MVI   BUWHY3,X'40'        SET MOVED BUY                                
         GOTO1 SETCHGDT                                                         
         BAS   RE,NXTBUYLN         READ FOR NEXT LINE                           
         MVC   SVKEY,KEY                                                        
*                                                                               
         MVC   KEY,FRKEY                                                        
         MVC   BUYKEY(10),KEY      SET KEY IN RECORD                            
         MVC   BUYKEY+10(2),KEY+11                                              
         OI    BUYKEY+15,X'80'     SET RECORD DELETED                           
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 HIGH                REREAD FROM KEY                              
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         MVI   GBYACT,C'W'         SET WRITE TO DIRECTORY                       
         GOTO1 DIR                                                              
**<<>>** GOTO1 VDATAMGR,DMCB,=C'DMDEL',=C'SPTFIL',KEY,AREC,DMWORK               
         OI    SVUPDATE,X'80'      SET BUY CHANGE FLAG                          
         GOTO1 VBLDQLST            BUILD REQ PRD LIST                           
                                                                                
         MVC   KEY,SVKEY           RESTORE KEY WITH NEW LINE NUMBER             
         MVC   BUYKEY(10),KEY      SET KEY IN RECORD                            
         MVC   BUYKEY+10(2),KEY+11                                              
         NI    BUYKEY+15,X'7F'                                                  
*                                                                               
         BRAS  RE,CKEXISTS                                                      
         GOTO1 ADDREC                                                           
*                                                                               
         OI    SVUPDATE,X'80'      SET NEW BUY ADDED                            
         MVC   SVKEY+14(4),KEY+14  SET DISK ADDRESS OF BUY JUST ADDED           
         GOTO1 VBLDQLST                                                         
* SEE IF NEED TO ADD ANY PRODUCT POINTERS ADDREC DOESN'T HANDLE                 
B870     BRAS  RE,ADDPOL                                                        
         BRAS  RE,DARBATCH                                                      
         LH    R1,NCOPIES          INCREMENT COPIED RECORDS COUNT               
         LA    R1,1(R1)                                                         
         STH   R1,NCOPIES                                                       
         B     B740                GO READ NEXT RECORD                          
         EJECT                                                                  
* WRAP-UP CODE - DISPLAY MESSAGE WITH NUMBER MOVED AND ERRORS                   
*                                                                               
B1000    CLC   FRSTLIN,FRENDLIN    TEST START LINE L.T. END LINE                
         BNE   B1005                                                            
         OC    NCOPIES,NCOPIES     ANY BUYS MOVED                               
         BZ    B1005                                                            
         MVC   KEY+14(4),SVKEY+14  RESET DISK ADDR OF BUY JUST ADDED            
         MVI   RCLOPT,0                                                         
         GOTO1 CALLDSP                                                          
                                                                                
B1005    LA    R4,BUYMSG                                                        
         MVC   0(16,R4),=C'**MOVE COMPLETED'                                    
         LA    R4,17(R4)                                                        
         MVI   0(R4),C'-'                                                       
         LA    R4,2(R4)                                                         
         LH    R0,NCOPIES          GET NUMBER OF BUYS MOVED                     
         EDIT  (R0),(3,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         CLC   NCOPIES,=H'1'       TEST FOR ONE BUY COPIED                      
         BNE   B1010                                                            
         MVC   0(15,R4),=C'BUY LINE  MOVED'                                     
         B     B1015                                                            
*                                                                               
B1010    MVC   0(15,R4),=C'BUY LINE  MOVED'                                     
         MVI   8(R4),C'S'                                                       
B1015    LA    R4,15(R4)                                                        
*                                                                               
B1020    MVC   0(2,R4),=C'**'                                                   
         OC    NERRS,NERRS         TEST FOR ANY ERRORS                          
         BNZ   B1022               YES                                          
* CLEAR REASON CODE NOW                                                         
         LHI   RE,SVRSNEL-BUYSAVE                                               
         AR    RE,RA                                                            
         XC    0(69,RE),0(RE)                                                   
         B     B1030               ALL DONE                                     
*                                                                               
B1022    MVC   0(2,R4),SPACES                                                   
         LA    R4,1(R4)            POSITION OUTPUT POINTER FOR ERRORS           
         MVI   0(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
         LH    R0,NERRS            GET NUMBER OF ERRORS                         
         EDIT  (R0),(3,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(6,R4),=C'ERRORS'                                               
         MVC   6(2,R4),=C'**'                                                   
*                                                                               
B1030    L     R2,ABUYINPH         POINT TO INPUT FLDHDR                        
         B     B278                PREPARE TO EXIT                              
         EJECT                                                                  
* ERROR HANDLING FOR ERRORS DURING MOVE                                         
*                                                                               
MOVER    CLC   FRSTLIN,FRENDLIN    TEST FOR SINGLE LINE MOVE                    
         BE    BUYERR              YES-TAKE REGULAR ERROR EXIT                  
         LH    R1,NERRS                                                         
         LA    R1,1(R1)            INCREMENT ERROR COUNT                        
         STH   R1,NERRS                                                         
         CLC   NERRS,=H'1'         TEST FOR FIRST ERROR                         
         BNE   MOVER2              NO                                           
         XC    BLDLIST,BLDLIST                                                  
         MVC   DSPAREA,SPACES                                                   
         MVC   BLDLIST(4),=X'141C2014'                                          
         LA    R6,DSPAREA                                                       
         ST    R6,BLDLIST+4                                                     
         MVC   DSPAREA(28),=C'**** ERRORS DURING MOVE ****'                     
         GOTO1 VBLDFLD                                                          
*                                                                               
* BUILD A FIELD UNDERNEATH BUY LINE FIELDS CONTAINING LINE                      
* AND THE ERROR MESSAGE                                                         
*                                                                               
MOVER2   XC    BLDLIST,BLDLIST                                                  
         MVC   DSPAREA,SPACES                                                   
         MVC   BLDLIST(4),=X'01452001'                                          
         LA    R6,DSPAREA                                                       
         ST    R6,BLDLIST+4                                                     
         MVC   0(4,R6),=C'LINE'                                                 
         LA    R6,5(R6)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BUYKBUY                                                     
         EDIT  (R0),(3,(R6)),ALIGN=LEFT                                         
         AR    R6,R0                                                            
         LA    R6,1(R6)                                                         
         GOTO1 VGETMSG,DMCB+12,(ERRCD,(R6)),(X'FF',DMCB),(3,0)                  
         GOTO1 VBLDFLD                                                          
*                                                                               
         B     B740                RESUME READING NEXT BUY LINE                 
         EJECT                                                                  
*                                                                               
*        GET EST RECORD                                                         
*                                                                               
GEST     LR    R0,RE                                                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,FRAGYMED                                                  
         MVC   EKEYCLT,SVCLT                                                    
         MVC   EKEYPRD,FULL                                                     
         MVC   EKEYEST,BYTE                                                     
         GOTO1 HIGH                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* FIND NEXT AVAILABLE LINE NUMBER.                                              
* SVKEY HAS A-M/CLT/PRD/MKT-STA/EST                                             
* RETURN NEW KEY IN SVKEY AND SET KEY IN BUYREC                                 
*                                                                               
NXTBUYLN NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       MOVE A-M/CLT/PRD/EST/STA                     
         CLI   SV1OR2,2                                                         
         BNE   NXTNOT2                                                          
         CLC   =C'TESTBIG',BUYBU                                                
         BNE   *+8                                                              
         MVI   KEY+12,255          START AT 256                                 
*                                                                               
NXTNOT2  TM    UPSW,UPON+UPCHA     TEST ACTION=CHANGE FOR UPLOAD                
         BNO   NXTBUY1                                                          
         MVC   KEY+11(2),UPBYLINE  MOVE BUYLINE NUMBER                          
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       IN CASE OF DELETED                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NXTBUY0                                                          
         MVI   ERRCD,NOTFOUND      INSTEAD OF DYING WHICH IS NEVER GOOD         
         B     BUYERR                                                           
*                                                                               
NXTBUY0  MVC   AREC,AREC3                                                       
         GOTO1 GETREC                                                           
         MVC   AREC,AREC1                                                       
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         B     NXTBUY10                                                         
*                                                                               
NXTBUY1  OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         B     NXTBUY4                                                          
*                                                                               
NXTBUY2  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
                                                                                
NXTBUY4  CLC   KEY(11),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    NXTBUY2              11 BYTES AS WE DON'T WANT SPILL             
                                                                                
         MVC   KEY,KEYSAVE                                                      
         ICM   RE,3,KEY+11                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+11                                                      
*                                                                               
NXTBUY6  XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
*                                                                               
         MVI   ERRCD,MAXLINES                                                   
         CLC   SVKEY+11(2),=H'255'                                              
         BNH   NXTBUY10                                                         
         CLI   SV1OR2,2                                                         
         BNE   NXTBUY8                                                          
         CLC   SVKEY+11(2),=Y(MAXBUYS)                                          
         BNH   NXTBUY10                                                         
*                                                                               
NXTBUY8  MVC   WORK(L'BUYKEY),KEY                                               
         BAS   RE,FNDPREV          FIND PREVIOUS BUY LINE NOT IN USE            
         BNE   BUYERR                                                           
         MVC   KEY+11(2),HALF                                                   
         MVC   SVKEY(13),KEY       THANKS LISA - $500 TO HAMAS                  
*                                                                               
NXTBUY10 MVC   BUYKEY(10),KEY                                                   
         MVC   BUYKEY+10(2),KEY+11                                              
*                                                                               
**NOP    CLC   BUYKEY+10(2),=H'1'     IF ADDING LINE NUMBER 1                   
**NOP    BNE   NXTBUYX                                                          
         TM    SVOPT2,SVOPT2_MKTOV    CAN'T BE A MARKET OVERRIDE                
         BNO   NXTBUYX                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOMKTOV)                                             
         B     BUYERR                                                           
NXTBUYX  B     EXIT                                                             
         SPACE 2                                                                
* GET NEXT LINE NUMBER                                                          
*                                                                               
NXTLIN   MVI   ERRCD,MAXLINES                                                   
         LA    R0,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    R0,255                                                           
         CLM   R0,3,SVLIN          TEST LAST LINE=MAXLINE                       
         BNE   NXTL20                                                           
         MVC   WORK(L'BUYKEY),BUYKEY                                            
         BAS   RE,FNDPREV          FIND PREVIOUS BUY LINE NOT IN USE            
         BNE   NXTL05                                                           
         MVC   SVLIN,HALF                                                       
         B     NXTL30                                                           
                                                                                
NXTL05   CLC   =C'COP',BUTRCODE    TEST FOR COPY                                
         BE    COPYR               YES-COPY ERROR                               
         B     MOVER               ELSE MUST BE MOVE ERROR                      
                                                                                
NXTL20   SR    R1,R1                                                            
         ICM   R1,3,SVLIN                                                       
         LA    R1,1(R1)            INCREMENT LAST LINE NUMBER                   
         STCM  R1,3,SVLIN                                                       
                                                                                
NXTL30   MVC   BUYKEY+10(2),SVLIN  SET LINE NUMBER IN KEY                       
         BR    RE                                                               
         EJECT                                                                  
*===================================================================            
* FIND A PREVIOUS BUY NUMBER THAT WASN'T USED                                   
* AND RETURN IN HALF                                                            
*===================================================================            
                                                                                
FNDPREV  NTR1                                                                   
         MVC   WORK+20(L'BUYKEY),KEY                                            
         MVC   HALF,=H'1'                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(11),WORK        SET A/M/CLT/PRD/MKSTA/EST                    
         OI    DMINBTS,X'08'       GET DELETED                                  
         GOTO1 HIGH                                                             
         B     FP20                                                             
                                                                                
FP10     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
                                                                                
FP20     CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   FPNO                                                             
         CLC   HALF,KEY+11         NEXT NUMBER ON FILE                          
         BNE   FP30                                                             
                                                                                
FP25     LH    R0,HALF                                                          
         AHI   R0,1                INCREMENT LAST LINE NUMBER                   
         STH   R0,HALF                                                          
         B     FP10                                                             
                                                                                
FP30     OC    HALF,HALF                                                        
         BZ    FP25                                                             
         LA    R0,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    R0,256                                                           
         CH    R0,HALF                                                          
         BE    FPNO                                                             
         MVC   KEY(L'BUYKEY),WORK+20                                            
         B     EQXIT                                                            
                                                                                
FPNO     MVC   KEY(L'BUYKEY),WORK+20                                            
         B     NEQXIT                                                           
         EJECT                                                                  
CHKMAXEL NTR1                                                                   
         MVI   ERRCD,MAXELEMS                                                   
         MVI   ELCDLO,X'0B'                                                     
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         MVI   ELCDLO,X'06'                                                     
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         IC    R7,SVMAXSPT         GET MAX SPOTS                                
         AHI   R7,1                PLUS ONE                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         BCT   R7,*-8                                                           
         B     NEQXIT                                                           
         EJECT                                                                  
TESTID   LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
         CLI   9(RF),C'O'          OPTIONAL PURPOSE CODE?                       
         JE    TESTID2             YES, NOT REQUIRED                            
         CLI   9(RF),C'Y'          TEST PURPOSE CODES REQD                      
         BNE   TESTID2             NO, NOT REQUIRED                             
         CLI   SVID,C' '           TEST PURPOSE CODE ENTERED                    
         BHR   RE                  YES                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPURCOD)                                              
         B     BUYERR                                                           
*                                                                               
TESTID2  CLI   SVCXTRA+2,C'Y'      TEST CLT REQUIRES ID                         
         BE    TESTID4             YES                                          
         CLI   SVCXTRA+2,C'A'      TEST ID=MKTGRP                               
         BL    *+12                                                             
         CLI   SVCXTRA+2,C'Z'                                                   
         BNH   TESTID10                                                         
         CLI   SVAPROF+9,C'Y'      TEST AGY REQUIRES ID                         
         BNER  RE                  NO -RETURN                                   
*                                                                               
TESTID4  OC    SVID,SVID           TEST ID PRESENT                              
         BNZR  RE                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         ST    RE,FULL                                                          
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LHI   R5,UPBY2UID-BUYSAVE                                              
         AR    R5,RA                                                            
         CLC   0(12,R5),SPACES     ANY SBY2 UNIQUE ID?                          
         BE    *+10                YES, BETTER THAN NO ID AT ALL                
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         MVI   ERRCD,NOIDERR                                                    
         CLC   =C'COP',BUTRCODE    TEST FOR COPY                                
         BE    COPYR                                                            
         CLC   =C'MOV',BUTRCODE                                                 
         BE    MOVER                                                            
         B     BUYERR              NOTHING                                      
*                                                                               
* ID=MKTGRP. IF MILLER, MAKE SURE AN IDR IS PRESENT AS WELL                     
*                                                                               
TESTID10 CLC   AGYALPHA,=C'TH'     TEST ZENITH                                  
         BNER  RE                  NO - RETURN                                  
         CLC   SVCLTIFC(2),=C'ML'  TEST THIS IS A MILLER CLIENT CODE            
         BNER  RE                  NO - RETURN                                  
         CLI   SVCLTIFC+2,C' '     TO ALLOW FOR C' ' OR X'00'                   
         BHR   RE                                                               
         CLI   SVID+6,C' '         ANYTHING THEY PUT IN IS VALID                
         BHR   RE                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(BADIDR)                                              
         B     BUYERR                                                           
*                                                                               
BLDID    NTR1                                                                   
         OC    SVID,SVID                                                        
         BZ    EXIT                                                             
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    BLDID2                                                           
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BO    BLDID2                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOCTAID)                                             
         B     BUYERR                                                           
*                                                                               
BLDID2   MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15                                                        
         MVI   ELEM+2,0                                                         
         MVC   ELEM+3(12),SVID                                                  
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    EXIT                IF ID ELEM PRESENT, USE IT                   
         BAS   RE,ELEMADD                                                       
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* BUILD PST ELEMENT                                      *                      
*========================================================*                      
         SPACE 1                                                                
BLDPST   NTR1                                                                   
         OC    SVPST,SVPST                                                      
         BZ    BLDPSTX                                                          
         LA    R0,10                                                            
         LA    R1,SVPST                                                         
BLDPST2  CLI   0(R1),C'H'          TEST FOR HST                                 
         BE    BLDPST4                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,BLDPST2                                                       
         B     BLDPST8                                                          
*                                                                               
* THERE IS HST - BUY PERIOD MUST NOT CROSS 3/31/97                              
*                                                                               
BLDPST4  CLC   BDSTART(3),=X'61031F'  START ON OR AFTER 3/31                    
         BNL   BLDPST6                ON OR AFTER GETS HST                      
         CLC   BDEND(3),=X'61031F'    END BEFORE 3/31                           
         BL    BLDPSTX                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(STANDHST)                                            
         B     BUYERR                                                           
*                                                                               
BLDPST6  XC    BDNTAX,BDNTAX          HST IMPLIES NO SALES TAX                  
*                                                                               
BLDPST8  MVI   ELEM,X'6B'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),SVPST                                                 
         MVI   ELCDLO,X'6B'                                                     
         MVI   ELCDHI,X'6B'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   BLDPST10                                                         
         BAS   RE,ELEMDEL                                                       
*                                                                               
BLDPST10 BAS   RE,ELEMADD                                                       
*                                                                               
BLDPSTX  B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* BUILD UPLOAD ELEMENT                                   *                      
*========================================================*                      
         SPACE 1                                                                
BLDUPL   NTR1                                                                   
         XC    ELEM,ELEM                                                        
E        USING BUPELEM,ELEM                                                     
         MVI   E.BUPCODE,BUPCODEQ                                               
         MVI   E.BUPLEN,BUPLENQ                                                 
         GOTO1 VDATCON,DMCB,(5,0),(3,E.BUPDAT)                                  
         MVC   E.BUPUID,UPUID                                                   
         BRAS  RE,GETTIME                                                       
         MVC   E.BUPTIME(3),FULL                                                
*                                                                               
         MVI   ELCDLO,BUPCODEQ                                                  
         MVI   ELCDHI,BUPCODEQ                                                  
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   BLDUPL2                                                          
         BAS   RE,ELEMDEL                                                       
         DROP  E                                                                
*                                                                               
BLDUPL2  BAS   RE,ELEMADD                                                       
*                                                                               
         LHI   R5,UPBY2UID-BUYSAVE                                              
         AR    R5,RA                                                            
         CLC   0(12,R5),SPACES     ANY SBY2 UNIQUE ID?                          
         BH    BLDUPL7              - WE GOT SOMETHIN'                          
*                                                                               
         CLI   SVCXTRA+2,C'Y'      TEST CLT REQUIRES ID                         
         BE    BLDUPLX             YES                                          
         CLI   SVCXTRA+2,C'A'      TEST ID=MKTGRP                               
         BL    *+12                                                             
         CLI   SVCXTRA+2,C'Z'                                                   
         BNH   BLDUPLX                                                          
*                                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   *+8                 NO PREVIOUS X'70' ELEMENT, NO DELETE         
         BAS   RE,ELEMDEL          WE HAVE A PREVIOUS X'70', DELETE             
         B     BLDUPLX             LET'S GET OUTTA HERE                         
*                                                                               
BLDUPL7  XC    ELEM,ELEM           WE'RE BUILDING THE IDELEM                    
         MVI   ELEM,X'70'          IDELEM FOR SBY2UID                           
         MVI   ELEM+1,15                                                        
         MVI   ELEM+2,0                                                         
*                                                                               
         MVC   ELEM+3(12),0(R5)                                                 
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   BLDUPL9                                                          
         CLI   SVCXTRA+2,C'Y'      WE HAVE AN IDELEM, CLT REQUIRES?             
         BE    BLDUPLX             YES, THEN LEAVE IT ALONE                     
         BAS   RE,ELEMDEL          NO, DELETE IT SO WE GET A NEW ONE            
*                                                                               
BLDUPL9  BAS   RE,ELEMADD                                                       
*                                                                               
BLDUPLX  B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* INSERT NEW DARE TRACE ELEMENT                          *                      
* DARE SPECIFIC DATA IS ADDED ON RETURN TO SPBUY31       *                      
*========================================================*                      
         SPACE 1                                                                
BLDNEWDR NTR1                                                                   
         XC    ELEM,ELEM                                                        
E        USING BDARELEM,ELEM                                                    
         MVI   E.BDARCODE,BDARCODQ                                              
         MVI   E.BDARLEN,BDARLENQ                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,E.BDARDATE)                                
         BRAS  RE,GETTIME                                                       
         MVC   E.BDARTIME,FULL                                                  
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'FF'                                                     
         MVI   ELCDHI,X'FF'                                                     
         BRAS  RE,NEXTEL           GO FIND EOF                                  
         BAS   RE,ELEMADD                                                       
         B     EXIT                                                             
         DROP  E                                                                
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
FNDUF    ZIC   R0,0(R2)                                                         
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
* DELETE AN ELEMENT                                                             
*                                                                               
ELEMDEL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         J     EXIT                                                             
         SPACE 1                                                                
* ADD AN ELEMENT                                                                
*                                                                               
ELEMADD  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         J     EXIT                                                             
         EJECT                                                                  
* ERROR EXIT FOR CURSOR POSITIONING                                             
*                                                                               
BUYERR1  LA    RF,8(R2)            RF=START OF ERROR FIELD                      
         SR    R4,RF               COMPUTE INDEX INTO ERROR FIELD               
         STC   R4,ERRNDX                                                        
         GOTO1 ERROR                                                            
         SPACE 2                                                                
OTHERMSG DC    C'** MULTIPLE BUY LINE COPY MUST BE ONLY TRANSACTION **'         
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*===============================================================                
* BUILD,SCALE AND INSERT SECOND COST ELEMENT IN DOLLARS                         
*===============================================================                
                                                                                
BLDCOST2 NTR1  BASE=*,LABEL=*                                                   
         TM    SVCOPT4,COP4MIDS    TEST MIDAS                                   
         BO    *+12                YES - ALWAYS ADD COST2 ELEMENT               
         TM    BUCOS2IND,X'80'     TEST COST2 INPUT THIS TIME                   
         JZ    EQXIT                                                            
*                                                                               
         XC    WORK,WORK           BUILD A TABLE OF VALUES                      
         MVC   WORK+0(1),BDCIND                                                 
         MVC   WORK+1(1),BDCIND2                                                
         MVC   WORK+5(3),BDCOST                                                 
*                                                                               
         OC    WORK+5(3),WORK+5    TEST COST IS 0                               
         BNZ   *+8                                                              
         NI    WORK+0,X'FF'-X'01'  TURN OFF -COST IND                           
*                                                                               
         CLI   BUCOST2,X'FF'       TEST 'DELETE COST2'                          
         BE    CCOS12                                                           
*                                                                               
         MVC   WORK+8(4),BUCOST2   MOVE COST2 AMOUNT                            
         NI    WORK+2,X'FF'-X'01'  RESET - COST IND                             
         TM    BUCOS2IND,X'01'     TEST COST2 NEGATIVE                          
         BZ    *+8                                                              
         OI    WORK+2,X'01'                                                     
*                                                                               
         MVI   WORK+3,X'40'        SET FLAG FOR COST2 PRESENT                   
         TM    BUCOS2IND,X'10'     TEST COST2 IN DOLLARS                        
         BZ    *+8                                                              
         OI    WORK+3,X'10'        SET FLAG FOR COST2 IN DOLLARS                
*                                                                               
CCOS10   TM    WORK+3,X'40'        TEST ANY COST2                               
         BZ    *+8                 NO - NO SCALING NECESSARY                    
         BRAS  RE,SETSCALE         SCALE COSTS/CHECK SIGNS                      
*                                                                               
         MVC   BDCOST(3),BUCOST    MOVE COST                                    
         MVC   BDCIND,BUCIND                                                    
         MVC   BDCIND2,BUCIND2                                                  
*                                                                               
CCOS12   MVI   BUCOS2IND,X'80'     FORCE ON FOR CAN NET COMPATIBILITY           
         BAS   RE,SETCOST2         YES - SET COST2                              
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* CHECK THAT COST AND COST2 HAVE SAME SIGNS                                     
* IF EITHER COST IS IN DOLLARS, SCALE BOTH TO DOLLARS                           
*===============================================================                
* WORK+0   COST FLAG             X01=MINUS + RATE TYPE                          
* WORK+1   COST FLAG2            X10=DOLLARS, X01=PENNIES IF CANNET             
* WORK+2   COST2 MINUS FLAG      X01                                            
* WORK+3   COST2 IN DOLLARS FLAG X10 + X40=COST2 PRESENT                        
*                                                                               
* WORK+4   BUYREC COST IN PENNIES                                               
* WORK+8   COST2 IN PENNIES                                                     
*===============================================================                
                                                                                
SETSCALE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,WORK+4                                                        
         TM    WORK+1,X'01'        TEST COST IN PENNIES (CANNET)                
         BO    SETSCL2                                                          
         TM    WORK+1,X'10'        TEST COST IN $                               
         BZ    *+8                                                              
         MHI   R0,100              SCALE TO PENNIES                             
         ST    R0,WORK+4                                                        
         NI    WORK+1,X'FF'-X'10'                                               
*                                                                               
SETSCL2  TM    WORK+3,X'01'        TEST COST2 IN PENNIES                        
         BO    SETSCL4                                                          
         L     R0,WORK+8                                                        
         TM    WORK+3,X'10'        TEST COST2 IN $                              
         BZ    *+8                                                              
         MHI   R0,100              SCALE TO PENNIES                             
         ST    R0,WORK+8                                                        
         NI    WORK+3,X'FF'-X'10'                                               
                                                                                
* IF EITHER COST OR COST2 IS 0, NO PROBLEM                                      
                                                                                
SETSCL4  OC    WORK+4(4),WORK+4    TEST COST 0                                  
         BZ    SETSCL10                                                         
         OC    WORK+8(4),WORK+8                                                 
         BZ    SETSCL10                                                         
* BOTH VALUES NOT ZERO - SO BOTH MUST BE + OR BOTH BE -                         
         TM    WORK+0,X'01'        TEST COST IS MINUS                           
         BZ    SETSCL6             NO                                           
         TM    WORK+2,X'01'        YES, TEST COST2 IS MINUS ALSO                
         BO    SETSCL10            YES - ALL IS GOOD                            
         B     SETSCERR                                                         
*                                                                               
SETSCL6  TM    WORK+2,X'01'        COST POS, TEST COST2 IS MINUS                
         BO    SETSCERR                                                         
*                                                                               
SETSCL10 CLI   WORK+4,0            TEST COST FITS IN PENNIES                    
         BNE   SETSCL12            NO - SCALE TO DOLLARS                        
         CLI   WORK+8,0            TEST COST2 FITS IN PENNIES                   
         BE    SETSCL14            YES - ALL IS GOOD                            
*                                                                               
SETSCL12 SR    R0,R0                                                            
         L     R1,WORK+4                                                        
         D     R0,=F'100'                                                       
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BNZ   SETSCER2                                                         
         ST    R1,WORK+4           SET COST IN DOLLARS                          
         OI    WORK+1,X'10'                                                     
*                                                                               
         SR    R0,R0                                                            
         L     R1,WORK+8                                                        
         D     R0,=F'100'                                                       
         ST    R1,WORK+8           SET COST2 IN DOLLARS                         
         OI    WORK+3,X'10'                                                     
*                                                                               
SETSCL14 EQU   *                                                                
SETSCL16 MVC   BUCOST,WORK+5                                                    
         MVC   BUCOST2,WORK+8                                                   
         MVC   BUCIND,WORK+0                                                    
         TM    WORK+2,X'01'        TEST COST 2 IS MINUS                         
         BZ    *+8                                                              
         OI    BUCIND,X'01'        THEN SET - COST FLAG                         
         MVC   BUCIND2,WORK+1                                                   
*                                                                               
SETSCLX  XIT1                                                                   
*                                                                               
SETSCERR MVC   NERRCD,=Y(COSTSIGN)                                              
         B     SETSCERX                                                         
*                                                                               
SETSCER2 MVC   NERRCD,=Y(LOSTCNTS)                                              
*                                                                               
SETSCERX MVI   ERRCD,NEWERRS                                                    
         J     BUYERRX                                                          
         LTORG                                                                  
         EJECT                                                                  
SETCOST2 NTR1  BASE=*,LABEL=*                                                   
         TM    SVCOPT4,COP4MIDS    TEST MIDAS                                   
         BO    *+12                YES - ALWAYS ADD COST2 ELEMENT               
         TM    BUCOS2IND,X'80'     WAS COST 2 INPUT                             
         JZ    EQXIT                                                            
*                                                                               
         MVI   ELCDLO,X'73'                                                     
         OC    SVCCOST2,SVCCOST2   TEST COST2 FACTOR                            
         BNZ   SETCOS2A                                                         
         OC    SVECOST2,SVECOST2                                                
         BNZ   SETCOS2A                                                         
*                                                                               
         MVI   ERRCD,BADKEYWD                                                   
         MVI   ELCDLO,X'71'                                                     
         TM    SVCOPT1,COP1COSQ    TEST COS2 DOLLARS REQD                       
         BO    SETCOS2A                                                         
         TM    SVCOPT3,COP3COSQ    TEST COS2 DOLLARS OR OPTIONAL                
         BO    SETCOS2A                                                         
         TM    SVCOPT4,COP4TRD     TEST TRADE                                   
         BO    SETCOS2A                                                         
         TM    SVCOPT4,COP4MIDS    TEST MIDAS                                   
         BO    SETCOS2A                                                         
         J     EQXIT                                                            
*                                                                               
SETCOS2A MVC   ELCDHI,ELCDLO                                                    
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   SETCOS2B                                                         
         BRAS  RE,ELEMDEL                                                       
*                                                                               
SETCOS2B CLI   BUCOST2,X'FF'       TEST 'DELETE COST 2'                         
         JE    EQXIT                                                            
*                                                                               
SETCOS2C XC    ELEM,ELEM                                                        
         MVC   ELEM(1),ELCDLO                                                   
         MVI   ELEM+1,6                                                         
         ICM   R0,15,SVECOST2                                                   
         BNZ   SETCOS2D                                                         
         ICM   R0,15,SVCCOST2                                                   
         BNZ   SETCOS2D                                                         
         ICM   R0,15,BUCOST2                                                    
*                                                                               
SETCOS2D STCM  R0,15,ELEM+2                                                     
         BRAS  RE,ELEMADD                                                       
*                                                                               
         TM    SVCOPT4,COP4MIDS      TEST MIDAS CLIENT                          
         BZ    SETCOS2X                                                         
         CLI   SVMIDAS,C'M'        TEST MIDAS STATION                           
         BE    *+12                                                             
         CLI   SVMIDAS,C'C'                                                     
         BNE   SETCOS2X                                                         
         OI    BDSTAT3,BDST3_COS2RT  SET COST2 RATE TYPE FLAG                   
SETCOS2X J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
CHKEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY      A-M/CLT                                      
         MVC   KEY+7(1),SVKEY+9    NEW EST                                      
* FIND PRD CODE FOR PRD AT 0(R4)                                                
         L     R6,ASVCLIST                                                      
         CLC   3(1,R6),0(R4)                                                    
         BE    *+12                                                             
         LA    R6,4(R6)                                                         
         B     *-14                                                             
         MVC   KEY+4(3),0(R6)                                                   
* BUT CHECK TABLE FIRST (1 BIT PER BRAND)                                       
         ZIC   R4,3(R6)            PRD NUM                                      
         BCTR  R4,0                SUB 1                                        
         SRDL  R4,3                DIVIDE BY 8                                  
         LA    R4,SVPRDEST(R4)     POINT TO TABLE BYTE (BYTE 0 = 1-8)           
         LA    R6,X'0100'                                                       
         SRL   R5,29               SHIFT REMAINDER FOR BCT                      
         LA    R5,1(R5)            SET FOR BCT                                  
         SRL   R6,1                                                             
         BCT   R5,*-4                                                           
         EX    R6,*+8              TEST BIT ON                                  
         B     *+8                                                              
         TM    0(R4),0 *EXECUTED*                                               
         BO    CHKYES                                                           
*                                                                               
         GOTO1 HIGH                READ FOR ESTHDR                              
         MVI   ERRCD,NOPRDEST                                                   
         CLC   =C'COP',BUTRCODE                                                 
         BE    CKE10                                                            
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CHKNO                                                            
         B     CKE20                                                            
                                                                                
CKE10    CLC   KEY(13),KEYSAVE                                                  
         BNE   CHKNO                                                            
                                                                                
* SET BIT IN PRDEST TABLE                                                       
                                                                                
CKE20    STC   R6,BYTE                                                          
         OC    0(1,R4),BYTE                                                     
         MVI   TWA1FLAG,C'Y'       SET FLAG TO WRITE TWA1                       
*                                                                               
CHKYES   CR    RB,RB                                                            
         B     CHKX                                                             
*                                                                               
CHKNO    LTR   RB,RB                                                            
*                                                                               
CHKX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* OBSERVE CLIENT/ESTIMATE PROFILE VALUES TO SET RATE TYPE          *            
*==================================================================*            
         SPACE 1                                                                
SETRATE  NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCPROF+14,C'*'     TEST NO SPECIAL RATES ALLOWED                
         BE    SETRAT2                                                          
         CLI   SVPRATE,C'*'        TEST OVERRIDE TO NORMAL RATES                
         BE    SETRAT2                                                          
         CLI   SVERATE,C'*'        TEST OVERRIDE TO NORMAL RATES                
         BNE   SETRAT10                                                         
*                                                                               
SETRAT2  MVI   BDCIND,X'20'        SET GROSS                                    
         MVI   BDCIND2,0                                                        
         B     SETRATX                                                          
*                                                                               
SETRAT10 LA    R1,SVERATE                                                       
         CLI   0(R1),C'0'          TEST MANDATORY EST RATE TYPE                 
         BH    SETRAT12                                                         
*                                                                               
         LA    R1,SVPRATE                                                       
         CLI   0(R1),C'0'          TEST MANDATORY PRD RATE TYPE                 
         BH    SETRAT12                                                         
*                                                                               
         LA    R1,SVCPROF+14                                                    
         CLI   0(R1),C'0'          TEST MANDATORY CLT RATE TYPE                 
         BE    SETRATX             NO                                           
*                                                                               
SETRAT12 DS    0H                                                               
         MVC   DUB(1),0(R1)        EXTRACT SETRATE TYPE                         
         NI    DUB,X'0F'                                                        
         ZIC   RE,DUB                                                           
         AR    RE,RE                                                            
         ZIC   RF,COSTAB-2(RE)                                                  
         TM    BDCIND,X'01'                                                     
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,BDCIND                                                        
         IC    RF,COSTAB-1(RE)     GET SECOND BYTE                              
         STC   RF,DUB                                                           
         OC    BDCIND2,DUB                                                      
SETRATX  CR    RB,RB                                                            
         XIT1                                                                   
*                                                                               
COSTAB   DC    X'0400',X'8000',X'1000',X'4000'   S/F/N/Q                        
         DC    X'0800',X'0200',X'0000',X'2080'   V/X/P/C                        
         EJECT                                                                  
*===============================================================                
* SUB-ROUTINE TO VALIDATE LINE NUMBERS                                          
*===============================================================                
                                                                                
VALLIN   NTR1  BASE=*,LABEL=*                                                   
         CLI   FLEN+1,3            TEST NO MORE THAN 3 DIGITS                   
         JH    BUYERR1                                                          
         TM    FVAL,X'08'          TEST NUMERIC FIELD                           
         JZ    BUYERR1                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         GET ADDRESS OF PACK INST                     
         CVB   R0,DUB                                                           
         LTR   R0,R0               TEST VALUE LESS THAN MAX                     
         JZ    BUYERR1                                                          
         CHI   R0,MAXBUYS                                                       
         JH    BUYERR1                                                          
         CLI   SV1OR2,2                                                         
         JE    VALLINX                                                          
         CHI   R0,255                                                           
         JH    BUYERR1                                                          
*                                                                               
VALLINX  XIT1  REGS=(R0)                                                        
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SUB-ROUTINE TO CHECK IF THE KEY FOR THE BUY RECORD IN AREC EXISTS             
* TO AVOID A DUPLICATE SHOWING UP IN THE RECOVERY FILE AS WE NEVER              
* GET THE 'DUPLICATE KEY ON ADD" ERROR FROM DATAMGR  (SEE DMDAPTRUS)            
*===============================================================                
                                                                                
CKEXISTS NTR1  BASE=*,LABEL=*                                                   
         L     R1,AREC             TEST NO MORE THAN 3 DIGITS                   
         XC    KEY,KEY                                                          
         MVC   KEY(BUYKBUY-BUYKEY),0(R1)                                        
         MVC   KEY+BUYKBUY+1-BUYKEY(2),BUYKBUY-BUYKEY(R1)                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
CKXISTX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* EDIT HIATUS WEEK LIST                                         *               
*===============================================================*               
         SPACE 1                                                                
         DS    0D                                                               
EDHIAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK+12,1           SET DEFAULT OCCURRENCES                      
         MVI   WORK+13,7           SET DEFAULT INTERVAL                         
*                                                                               
         MVI   ERRCD,BADWEEK                                                    
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         MVI   FSTOPS+1,C'-'                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    EHERR                                                            
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    EHERR                                                            
*                                                                               
         MVC   WORK(2),SVSTART     MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    EH2                                                              
         CLC   WORK+2(4),SVSTART+2  INPUT MMDD TO ES START MMDD                 
         BNL   *+10                                                             
         MVC   WORK(2),SVEND                                                    
         MVC   WORK+6(6),WORK      SET END=START                                
*                                                                               
EH2      CLI   FSTOP,C','                                                       
         BE    EH10                                                             
         CLI   FSTOP,0             TEST END OF DATA                             
         BE    EH10                                                             
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    EHERR                                                            
*                                                                               
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK+6                                     
         OC    0(4,R1),0(R1)       TEST FOR ANOTHER VALID DATE                  
         BZ    EH4                                                              
         PACK  DUB,WORK(2)                                                      
         CLC   WORK+2(4),WORK+8    START MMDD TO END MMDD                       
         BNH   *+10                                                             
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         B     EH10                                                             
         EJECT                                                                  
*=========================================================*                     
* NOT A VALID DATE - TRY FOR WEEKS                        *                     
*=========================================================*                     
         SPACE 1                                                                
EH4      MVI   FSTOPS,C'W'                                                      
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'W'                                                       
         BNE   EHERR                                                            
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    EHERR                                                            
         LTR   R5,R5                                                            
         BZ    EHERR                                                            
         CVB   R0,DUB                                                           
         STC   R0,WORK+12          SAVE  NUMBER OF WEEKS                        
         SPACE 1                                                                
* CHECK FOR ALT WEEK IND *                                                      
         SPACE 1                                                                
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LA    R1,7                                                             
         LTR   R5,R5                                                            
         BNZ   EH6                                                              
         L     RE,FADDR                                                         
         LA    RE,1(RE)            POINT BEYOND COMMA (IF ANY)                  
         ST    RE,FADDR            AND RESET INPUT POINTER                      
         B     EH8                                                              
*                                                                               
EH6      CLI   FLEN+1,1                                                         
         BNE   EHERR                                                            
         MVC   WORK+13(1),0(R4)                                                 
         LA    R1,14                                                            
         CLI   0(R4),C'A'                                                       
         BE    EH8                                                              
         LA    R1,21                                                            
         CLI   0(R4),C'T'                                                       
         BE    EH8                                                              
         LA    R1,28                                                            
         CLI   0(R4),C'F'                                                       
         BE    EH8                                                              
         B     EHERR                                                            
*                                                                               
EH8      STC   R1,WORK+13          SAVE INTERVAL                                
         ZIC   R0,WORK+12          GET NUMBER OF OCCURRENCES                    
         BCTR  R0,0                                                             
         CLI   WORK+13,7           TEST CONSECUTIVE WEEKS                       
         BE    *+10                                                             
         LTR   R0,R0                                                            
         BZ    EHERR                                                            
         MR    R0,R0                                                            
         LR    R0,R1                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)    SET END DATE                     
         EJECT                                                                  
*=========================================================*                     
* FIND END OF CURRENT HIATUS LIST                         *                     
*=========================================================*                     
         SPACE 1                                                                
EH10     DS    0H                                                               
         LA    R7,BUHIATS                                                       
EH12     OC    0(2,R7),0(R7)                                                    
         BZ    EH14                                                             
         LA    R7,2(R7)                                                         
         B     EH12                                                             
*                                                                               
EH14     LA    R0,BUHIATS+L'BUHIATS-2                                           
         CR    R7,R0               TEST TOO MANY ENTRIES                        
         BH    EHERR                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,(R7))                                       
         LA    R7,2(R7)                                                         
         ZIC   R0,WORK+13          GET INTERVAL                                 
         GOTO1 VADDAY,DMCB,WORK,WORK,(R0)                                       
         CLC   WORK(6),WORK+6                                                   
         BNH   EH14                                                             
*                                                                               
EHX      XIT1                                                                   
*                                                                               
EHERR    GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
* ADDREC DOES NOT ADD TRUE POL POINTERS - SO NEED TO CHECK           *          
* IF THIS IS TRUE POL AND THEN DO GETREC/PUTREC WITH ADDED PRD LIST  *          
*====================================================================*          
         SPACE 1                                                                
         DS    0D                                                               
ADDPOL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PRDLIST,PRDLIST                                                  
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
ADDPOL2  SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    ADDPOL10                                                         
         CLI   0(R6),X'0B'                                                      
         BL    ADDPOL2                                                          
         CLI   0(R6),X'0D'                                                      
         BH    ADDPOL2                                                          
*                                                                               
         LA    R4,10(R6)           POINT TO FIRST ALLOCATION                    
         ZIC   R5,1(R6)                                                         
         AHI   R5,-10                                                           
         BNP   ADDPOL2                                                          
         SRL   R5,2                SET FOR BCT                                  
*                                                                               
ADDPOL4  BAS   RE,ADDSET                                                        
         LA    R4,4(R4)                                                         
         BCT   R5,ADDPOL4                                                       
         B     ADDPOL2                                                          
*                                                                               
ADDPOL10 DS    0H                                                               
         CLI   PRDLIST,0           TEST ANY PRDS TO BE ADDED                    
         BE    ADDPOLX             NO                                           
         GOTO1 GETREC              REBUILD THE GETREC TABLE                     
*                                                                               
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20          NEED TO SET PRDLIST ADDRESS                  
         GOTO1 PUTREC                                                           
*                                                                               
ADDPOLX  XIT1                                                                   
*                                                                               
ADDSET   CLC   0(1,R4),BDMASPRD    TEST MATCHES MASPRD                          
         BER   RE                  YES - RETURN                                 
         CLC   0(1,R4),BDMASPRD+1  OR MASPRD2                                   
         BER   RE                                                               
*                                                                               
         LA    R1,PRDLIST          SEE IF PRD ALREADY IN LIST                   
*                                                                               
ADDSET2  CLI   0(R1),0                                                          
         BE    ADDSET4                                                          
         CLC   0(1,R1),0(R4)                                                    
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     ADDSET2                                                          
*                                                                               
ADDSET4  MVC   0(1,R1),0(R4)       ADD PRD TO LIST                              
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================*                         
* BUILD AND ADD CANADIAN EXCHANGE RATE ELEMENT        *                         
*  AND GST ELEMENT IF NON-STANDARD                    *                         
*=====================================================*                         
         SPACE 1                                                                
BLDXCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    BDCIND2,X'20'                                                    
         BZ    BLDXCH10                                                         
         CLI   SVGSTCOD,C'S'                                                    
         BE    BLDXCH10                                                         
* NEED GST ELEMENT                                                              
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'6A'                                                       
         MVI   ELEM+1,3                                                         
         MVC   ELEM+2(1),SVGSTCOD                                               
*                                                                               
         MVI   ELCDLO,X'FF'                                                     
         MVI   ELCDHI,X'FF'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BRAS  RE,ELEMADD                                                       
*                                                                               
BLDXCH10 CLI   SVCXTRA+9,C'C'      TEST CANADIAN CLIENT                         
         BNE   BLDXCH12            NO - NEED EXCHANGE RATE                      
         CLI   SVSTACNT,C'C'       TEST CANADIAN STATION                        
         BNE   BLDXCH12            NO - NEED EXCHANGE RATE                      
         CLI   SVSTADOL,C'C'       TEST CANADIAN DOLLARS INPUT                  
         BE    BLDXX               YES - ALL CANADIAN - GET OUT                 
*                                                                               
BLDXCH12 XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING XCHELEM,R7                                                       
*                                                                               
         MVI   XCHCODE,XCHCODEQ                                                 
         MVI   XCHLEN,XCHLENQ                                                   
         MVC   XCHCTYP,SVCXTRA+9   CLIENT COUNTRY                               
         MVC   XCHSTYP,SVSTACNT    STATION COUNTRY                              
         MVC   XCHDTYP,SVSTADOL    DOLLAR TYPE                                  
         MVC   XCHC58,SVSTACTX     C58 TAX                                      
*                                                                               
         MVI   ERRCD,XRTRERR                                                    
         OC    SVXRATE,SVXRATE     TEST EXCHANGE RATE EXISTS                    
*=======>                                                                       
         BZ    BLDXX               <========== NOP ERROR FOR NOW                
*=======>                                                                       
***      BZ    BUYERR1             NO-ERROR                                     
         MVC   XCHRATE,SVXRATE     CURRENT EXCHANGE RATE                        
         OI    BDCIND2,X'40'       INDICATE EXCHANGE RATE USED                  
         DROP  R7                                                               
*                                                                               
         MVI   ELCDLO,XCHCODEQ                                                  
         MVI   ELCDHI,XCHCODEQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BLDXCH14                                                         
         BRAS  RE,ELEMDEL                                                       
*                                                                               
BLDXCH14 DS    0H                                                               
         BRAS  RE,ELEMADD                                                       
*                                                                               
BLDXX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================*                         
* ADD DARE BATCH POINTERS IF THE AGENCY DO IT         *                         
*=====================================================*                         
         SPACE 1                                                                
DARBATCH NTR1  BASE=*,LABEL=*,WORK=(R7,DARBATCL)                                
         USING DARBATCD,R7                                                      
         XC    0(DARBATCL,R7),0(R7)   INIT LOCAL STORAGE                        
*                                                                               
         TM    SVSPOMAK,SVSPOMAK_NOBUY TEST DO-NOT-ADD                          
         BO    DBTCHX                                                           
         CLI   SVDARPRF+4,C'Y'     USING DARE BATCH ORDERING?                   
         BNE   DBTCHX              NO, NOTHING TO DO HERE                       
*                                                                               
         CLI   BUYMD,C'T'          BATCH RECORDS FOR ONLY TV & RADIO            
         BE    *+12                                                             
         CLI   BUYMD,C'R'                                                       
         BNE   DBTCHX                                                           
*                                                                               
         OC    BDREP,BDREP                                                      
         BZ    DBTCH005                                                         
*                                                                               
         XC    WORK,WORK                                                        
         LHI   RE,VRCPACK-BUYSAVE  ONLY CHECK THAT FIRST 2 DIGITS MATCH         
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',BDREP),WORK                                      
*                                                                               
         CLI   SVDARPRF+14,C'Y'    MULTIPLE TRADE CODES?                        
         BE    *+14                                                             
         CLC   WORK(3),SVDARPRF+6  DO THEY MATCH FOR 3 DIGITS?                  
         B     *+10                                                             
         CLC   WORK(2),SVDARPRF+6  DO THEY MATCH FOR 2 DIGITS?                  
         BNE   *+8                                                              
         MVI   WORK+3,C'Y'         CHECK THIS FLAG LATER                        
*                                                                               
DBTCH005 L     R4,ASVDARE                                                       
         CLI   0(R4),C'Y'          FLIGHT REQUIRED?                             
         BNE   DBTCH010             NO                                          
         BRAS  RE,CHKFLTS           YES, CHECK SPOTS AGAINST FLT-TBL            
         BNE   DBTCHX              SPOTS OUTSIDE FLTS, EXIT                     
*                                                                               
DBTCH010 XC    KEY,KEY             SEE IF WE HAVE A DARE ORDER ALREADY          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,BUYKAM                                                   
         MVC   DCKCLT,BUYKCLT                                                   
         MVC   DCKPRD,BDMASPRD                                                  
         MVC   DCKEST,BUYKEST                                                   
         MVC   DCKSTA,BUYKSTAC                                                  
         CLI   DCKSTA,X'E8'        IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DCKSTA+2,X'80'      YES, WE ONLY WANT SYSCODE LEVEL              
         MVC   DCKPRD2,BDMASPRD+1                                               
         CLI   DARBFLT,DARBFLTZ    FLIGHT 0?                                    
         BE    *+10                                                             
         MVC   DCKFLTNM,DARBFLT                                                 
         GOTO1 HIGH                                                             
DBTCH020 CLC   KEY(12),KEYSAVE      MATCH KEY UPTO FLT?                         
         BE    DBTCH025              YES, GO CHK TRADE BIT                      
*                                                                               
* IF WE GET HERE, WE NEED TO ADD BATCH                                          
*                                                                               
         CLI   DARBFLT,0           HAVE FLIGHTS?                                
         BE    DBTCH050             NO, DON'T CHECK OTHER FLIGHTS               
         B     DBTCH042             YES, NEED TO CHECK OTHER FLIGHTS            
*                                                                               
DBTCH025 CLI   WORK+3,C'Y'                                                      
         BE    DBTCH030                                                         
         TM    DCKFLAG,DCKFTRDE    BUYLINE IS CASH, HAVE CASH ORDER?            
         BZ    DBTCH041             YES, DON'T ADD BATCH                        
         B     DBTCH040                                                         
DBTCH030 TM    DCKFLAG,DCKFTRDE    BUYLINE IS TRADE, HAVE TRADE ORDER?          
         BO    DBTCH041              YES, DON'T ADD BATCH                       
DBTCH040 GOTO1 SEQ                                                              
         B     DBTCH020                                                         
*                                                                               
* IF WE GET HERE, WE DO NOT NEED TO ADD BATCH                                   
*                                                                               
DBTCH041 CLI   DARBFLT,0           HAVE FLIGHTS?                                
         BE    DBTCH190            NO, RESTORE READ-SEQ AND EXIT                
*                                                                               
* IF WE GET HERE, WE HAVE TO CHECK OTHER FLIGHTS                                
*                                                                               
DBTCH042 CLI   DARBFLT,DARBFLTZ    FLIGHT 0?                                    
         BNE   DBTCH043                                                         
*                                                                               
         LA    RF,DARBFLTL+L'DARBFLTL  POINT RF TO END OF DARBFLTL              
         LA    RE,DARBFLTL         FORCE NEXT FLIGHT TO TO FLT1                 
         CLC   KEY(12),KEYSAVE     DID WE FIND THE FLT ORDER?                   
         BNE   DBTCH046             NO, HAVE TO ADD BATCH FOR THIS FLT          
         MVI   DARBFLT0,0           YES, CLEAR FLT0 SO WE DON'T ADD             
         B     DBTCH046                                                         
*                                                                               
DBTCH043 LLC   RE,DARBFLT                                                       
         LA    RE,DARBFLTL-1(RE)   POINT TO CURRENT FLIGHT ENTRY                
*                                                                               
         CLC   KEY(12),KEYSAVE     DID WE FIND THE FLT ORDER?                   
         BNE   DBTCH044             NO, HAVE TO ADD BATCH FOR THIS FLT          
         MVI   0(RE),0              YES, CLEAR THE FLIGHT ENTRY                 
*                                                                               
DBTCH044 LA    RF,DARBFLTL+L'DARBFLTL  FIND THE NEXT FLIGHT IN DARBFLTL         
DBTCH045 LA    RE,1(RE)            BUMP RE TO GET NEXT FLT IN DARBFLTL          
         CR    RE,RF               END OF FLIGHT TABLE?                         
         BNL   DBTCH047                                                         
DBTCH046 CLI   0(RE),0             HAVE FLIGHT?                                 
         BZ    DBTCH045             NO, CHECK NEXT                              
         MVC   DARBFLT,0(RE)                                                    
         B     DBTCH010                                                         
*                                                                               
DBTCH047 OC    DARBFLTA,DARBFLTA   ANY BATCH-FLTS TO ADD?                       
         BZ    DBTCH190             NO, RESTORE READ-SEQ AND EXIT               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DARBFLT0       HAVE FLIGHT0?                                
         BNZ   DBTCH049             YES                                         
*                                                                               
         LA    RF,DARBFLTL         POINT TO START OF FLT1-FLT16 TABLE           
DBTCH048 ICM   RE,1,0(RF)          ENTRY HAS FLIGHT?                            
         BNZ   DBTCH049                                                         
         AHI   RF,1                                                             
         B     DBTCH048                                                         
DBTCH049 STC   RE,DARBFLT          SET THE FIRST FLIGHT TO ADD                  
*                                                                               
DBTCH050 XC    KEY,KEY             SEE IF WE HAVE A BATCH ORDER ALREADY         
         LA    R6,KEY                                                           
         USING DBTKEY,R6                                                        
*                                                                               
         MVI   DBTKTYP,DBTKTYPQ                                                 
         MVI   DBTKSTYP,DBTKSTYQ                                                
         MVC   DBTKAGMD,BUYKAM                                                  
         MVC   DBTKMKT(L'BUYMSTA),BUYMSTA                                       
***                                                                             
         CLI   DBTKSTA,X'E8'       IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DBTKSTA+2,X'80'     YES, WE ONLY WANT SYSCODE LEVEL              
***                                                                             
         MVC   DBTKCLT,BUYKCLT                                                  
         MVC   DBTKEST,BUYKEST                                                  
         CLI   BUYKPRD,X'FF'       POL BUY?                                     
         BNE   DBTCH060                                                         
         MVC   DBTKPRD,BDMASPRD    YES, PRODUCT CODES STORED HERE               
         MVC   DBTKPRD2,BDMASPRD+1                                              
         B     DBTCH090                                                         
*                                                                               
DBTCH060 MVC   DBTKPRD,BUYKPRD     NON-POL, PRODUCT CODES STORED HERE           
         LA    RE,BDELEM                                                        
         XR    RF,RF                                                            
DBTCH070 CLI   0(RE),0                                                          
         BE    DBTCH090                                                         
         CLI   0(RE),X'04'         LOOK FOR PIGGYBACK ELEM                      
         BE    DBTCH080                                                         
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     DBTCH070                                                         
*                                                                               
DBTCH080 MVC   DBTKPRD2,PBPROD-PBELEM(RE)                                       
*                                                                               
DBTCH090 CLI   DBTKPRD,X'00'       IF NO PRODUCT ALLOCATED                      
         BNE   *+8                                                              
         MVI   DBTKPRD,X'FF'       THEN POL                                     
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   AREC,AREC3          READ BATCH RECORD INTO AIO3                  
         NI    DARFLG1,X'FF'-DARF1RNF                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DBTCH100                                                         
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
         CLI   24(R6),DBINFELQ     X'01' - INFO ELEMENT?                        
         BNE   DBTCH101                                                         
         MVC   ELEM(DBINFLNQ),24(R6)         SAVE THE OLD ELEM                  
         MVC   WORK+4(DBINFLNQ),ELEM                                            
         GOTO1 VRECUP,DMCB,(R6),24(R6),24(R6)  DELETE INFO ELEM AND             
         B     DBTCH101                                                         
*                                                                               
DBTCH100 OI    DARFLG1,DARF1RNF    DIDN'T FIND IT!!                             
         L     R6,AREC                                                          
         XC    0(256,R6),0(R6)                                                  
         MVC   KEY,KEYSAVE                                                      
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   13(2,R6),=AL2(DBTRFRST-DBTKEY)                                   
*                                                                               
DBTCH101 CLI   DARBFLT,0           HAVE FLIGHTS?                                
         BE    DBTCH110            NO                                           
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
DBTCH102 CLI   0(R6),0             END-OF-REC?                                  
         BE    DBTCH106            GO ADD A FLIGHT ELEM                         
         CLI   0(R6),DBFLTELQ      X'20'-FLIGHT ELEM?                           
         BE    DBTCH104                                                         
DBTCH103 LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DBTCH102                                                         
         USING DBFLTELD,R6                                                      
DBTCH104 SR    RF,RF                                                            
         ICM   RF,1,DBFLTFLT       FLIGHT0?                                     
         BNZ   *+8                                                              
         LHI   RF,C'0'                                                          
         STC   RF,BYTE                                                          
                                                                                
         CLC   BYTE,DARBFLT                                                     
         BL    DBTCH103            NOT THERE YET, LOOK FOR NXT FLT ELEM         
         BH    DBTCH106            WE PASSED IT, LETS GO ADD IT                 
         CLI   WORK+3,C'Y'                                                      
         BE    DBTCH105                                                         
         TM    DBFLTFL1,DBFLTTRD   HAVE CASH(NOT TRADE) FLIGHT ALREADY?         
         BNZ   DBTCH106             NO, THEN GO ADD IT                          
         B     DBTCH107             YES, SKIP IT                                
*                                                                               
DBTCH105 TM    DBFLTFL1,DBFLTTRD   HAVE TRADE FLIGHT ALREADY?                   
         BZ    DBTCH103             NO, GO CHK THE NEXT ONE                     
         B     DBTCH107             YES, SKIP IT                                
         DROP  R6                                                               
*                                                                               
DBTCH106 OI    DARFLG1,DARF1UPF+DARF1UPD  UPD FLT ELEM & WRITE REC              
         LA    R2,SVDBFLTEL        LETS ADD OUR FLT ELEM                        
         USING DBFLTELD,R2                                                      
         XC    SVDBFLTEL,SVDBFLTEL INIT SV-ELEM                                 
         MVI   DBFLTEL,DBFLTELQ    X'20'-FLIGHT ELEM                            
         MVI   DBFLTLEN,DBFLTOVH                                                
         CLI   DARBFLT,DARBFLTZ    ZERO FLIGHT?                                 
         BE    *+10                 YES, LEAVE AS NULL                          
         MVC   DBFLTFLT,DARBFLT                                                 
         CLI   WORK+3,C'Y'         ARE WE UPDATE TRADE?                         
         BNE   *+8                                                              
         OI    DBFLTFL1,DBFLTTRD   SET TRADE BIT                                
         GOTO1 VRECUP,DMCB,AREC,SVDBFLTEL,(R6)                                  
         CLI   DARBFLT,DARBFLTZ    JUST ADD FLIGHT0?                            
         BNE   DBTCH107                                                         
         LLC   R0,1(R6)            YES, KEEP FLT-0 ELEM AT START OF REC         
         AR    R6,R0                                                            
*                                                                               
DBTCH107 LA    RF,DARBFLTL+L'DARBFLTL  RF=A(END OF DARBFLTL)                    
*                                                                               
         LA    RE,DARBFLTL         IN CASE WE JUST PROCESSED FLT0               
*                                   POINT RE=A(START OF DARBFLTL)               
         CLI   DARBFLT,DARBFLTZ    DID WE JUST PROCESS FLIGHT0?                 
         BE    DBTCH109             YES                                         
*                                                                               
         LLC   RE,DARBFLT          PROCESS THE NEXT FLT                         
         LA    RE,DARBFLTL-1(RE)   POINT TO CURRENT FLT ENTRY                   
         SR    R1,R1                                                            
DBTCH108 LA    RE,1(RE)            BUMP TO NEXT FLT ENTRY                       
DBTCH109 CR    RE,RF               END OF FLIGHTS?                              
         BNL   DBTCH110             YES                                         
         ICM   R1,1,0(RE)          ENTRY HAS FLIGHT?                            
         BZ    DBTCH108             NO                                          
         STC   R1,DARBFLT          SET THE FLT TO PROCESS                       
         B     DBTCH102                                                         
*                                                                               
* NOTE : ELEM WILL HAVE COPY OF DBINFEL IF IT WAS PRESENT IN ORIG-REC           
*                                                                               
DBTCH110 L     R6,AREC                                                          
         LA    RE,ELEM             WHEN WE CREATED THIS BATCH REC               
         USING DBINFELD,RE                                                      
         MVI   DBINFEL,DBINFELQ                                                 
         MVI   DBINFLEN,DBINFLNQ                                                
         LA    R1,DBINFDTC         POINT TO CASH (PWOS JULIAN)                  
         LA    R2,DBINFTMC         POINT TO CASH (TIME TU)                      
         CLI   WORK+3,C'Y'         ARE WE UPDATE TRADE?                         
         BNE   DBTCH120                                                         
         LA    R1,DBINFDTT         POINT TO TRADE (PWOS JULIAN)                 
         LA    R2,DBINFTMT         POINT TO TRADE (TIME TU)                     
         DROP  RE                                                               
*                                                                               
DBTCH120 OC    0(3,R1),0(R1)       DO WE ALREADY HAVE A DATE?                   
         BZ    DBTCH125             NO                                          
         TM    DARFLG1,DARF1UPF    DID I UPDATE FLIGHT?                         
         BO    DBTCH130             YES, MUST PUT INFO ELEM BACK                
         B     DBTCH190             NOTHING TO DO, EXIT                         
*                                                                               
DBTCH125 OI    DARFLG1,DARF1UPP    UPDATE PASSIVE                               
         ST    R1,DMCB+4                                                        
         MVI   DMCB+4,19                                                        
*                                                                               
         TIME  TU                                                               
         STCM  R0,15,0(R2)         R2=A(TIME ADDED) SAVE FOR LATER              
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),,0  FOR TODAY                                 
DBTCH130 GOTO1 VRECUP,DMCB,AREC,ELEM,24(R6)                                     
         OI    DARFLG1,DARF1UPD    WRITE RECORD BACK                            
*                                                                               
DBTCH135 TM    DARFLG1,DARF1UPD    WRITE RECORD BACK?                           
         BZ    DBTCH150                                                         
         TM    DARFLG1,DARF1RNF    DID WE FIND A BATCH REC?                     
         BZ    DBTCH140                                                         
         GOTO1 ADDREC              NO                                           
         B     DBTCH150                                                         
DBTCH140 GOTO1 PUTREC              YES                                          
*                                                                               
DBTCH150 TM    DARFLG1,DARF1UPP    UPDATE PASSIVE                               
         BZ    DBTCH190             NO                                          
         LA    R2,ELEM                                                          
         USING DBINFELD,R2                                                      
         LA    R6,KEY                                                           
         USING DBTKEY,R6                                                        
         MVI   DDTKTYP,DDTKTYPQ                                                 
         MVI   DDTKSTYP,DDTKSTYQ                                                
         MVC   DDTKCLT,DBTKCLT                                                  
         TM    DARFLG1,DARF1RNF    DID WE FIND A BATCH REC?                     
         BNZ   DBTCH180            NO, THEN NO CLEANUP, GO ADD PASSIVE          
         LA    R1,DBINFDTC                                                      
         MVC   DDTKTMTU,DBINFTMC                                                
         CLI   WORK+3,C'Y'         TRADE BUYLINE ADDED?                         
         BE    DBTCH160            YES, THEN DELETE CASH PASSIVE                
         LA    R1,DBINFDTT         NO, THEN DELETE TRADE PASSIVE                
         MVC   DDTKTMTU,DBINFTMT                                                
*                                                                               
DBTCH160 ST    R1,DMCB                                                          
         MVI   DMCB,8                                                           
         GOTO1 VDATCON,DMCB,,(2,DDTKDATE),0                                     
         XC    DDTKDATE,=X'FFFF'                                                
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BNE   DBTCH170                                                         
         OI    KEY+13,X'80'        DELETE IT                                    
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
DBTCH170 MVC   KEY,KEYSAVE                                                      
*                                                                               
DBTCH180 GOTO1 VDATCON,DMCB,(5,0),(2,DDTKDATE),0  TODAY                         
         XC    DDTKDATE,=X'FFFF'                                                
         MVC   DDTKTMTU,DBINFTMC                                                
         CLI   WORK+3,C'Y'         TRADE BUYLINE ADDED?                         
         BNE   *+10                                                             
         MVC   DDTKTMTU,DBINFTMT                                                
         GOTO1 ADD                                                              
         DROP  R2,R6                                                            
*                                                                               
DBTCH190 XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                YES, RESTORE READ SEQUENCE                   
*                                                                               
DBTCH200 MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
DBTCHX   XIT1                                                                   
                                                                                
**********************************************************************          
* IF FLIGHT IS REQUIRED, THEN CHECK EACH BUY ELEMENT AGAINST THE                
*  FLIGHT START/END DATE TABLE IN ASVDARE AND BUILD LIST OF EFFECTIVE           
*  FLIGHTS IN DARBFLTL. ALSO SET THE FIRST FLIGHT IN DARBFLT                    
**********************************************************************          
CHKFLTS  NTR1                                                                   
         MVI   ELCDLO,X'0B'        MUST USE BUY ELEMENTS                        
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
CHKFLT10 BRAS  RE,NEXTEL                                                        
         BNE   CHKFLTX                                                          
*                                                                               
         BAS   RE,FLNUM            FIND FLIGHT SEQ NUM                          
         B     CHKFLT10                                                         
*                                                                               
CHKFLTX  CLI   DARBFLT,0           HAVE ATLEAST 1 FLIGHT SET?                   
         BE    *+6                  NO, EXIT CC=NO                              
CHKFLTYS SR    RE,RE                                                            
         LTR   RE,RE                                                            
         XIT1                                                                   
*                                                                               
**********************************************************************          
*  FIND FLIGHT SEQ NUMBER IN FLIGHT TABLE                                       
*                                                                               
*  ON ENTRY:R6 POINTS TO BUY ELEMENT                                            
*                                                                               
*  ON EXIT :DARBFLTL  CONTAINS LIST OF ALL FLIGHTS                              
*          :DARBFLT   CONTAINS FIRST FLIGHT#                                    
*          :DARBFLT0  CONTAINS FLIGHT-0 C'0'                                    
**********************************************************************          
FLNUM    NTR1                                                                   
*                                                                               
         L     R4,ASVDARE                                                       
         USING SVDARED,R4                                                       
         OC    SVDRFLT0,SVDRFLT0   HAVE FLIGHT0?                                
         BZ    FLNUM05                                                          
         CLI   DARBFLT0,0          ALREADY FOUND SPOTS FOR FLT0?                
         BNE   FLNUM05              YES, DON'T BOTHER                           
         CLC   2(2,R3),SVDRFLT0                                                 
         BH    FLNUM05                                                          
         MVI   DARBFLT0,DARBFLTZ                                                
         MVI   DARBFLT,DARBFLTZ                                                 
*                                                                               
FLNUM05  LA    R4,8(R4)            POINT R4 TO START/END DATES                  
         LA    R2,0                FLIGHT SEQ NUM                               
         LA    R5,16               MAX NUM OF FLIGHTS                           
*                                                                               
* HOLES IN THE TABLE ARE IDENTIFIED BY X'FF' IN THE FIRST BYTE                  
* END OF THE TABLE IS IDENTIFIED BY NULLS IN THE FIRST TWO BYTES                
*                                                                               
FLNUM10  CLC   0(2,R4),=X'0000'    NO MORE FLIGHTS?                             
         BE    FLNUMEX                                                          
         LLC   RE,DARBFLTL(R2)                                                  
         LTR   RE,RE               ALREADY HAVE SOMETHING FOR THIS FLT?         
         BNZ   FLNUM20               YES, SKIP IT THEN                          
         CLC   2(2,R6),0(R4)       ELEM DATE TO FLIGHT START                    
         BL    FLNUM20                                                          
         CLC   2(2,R6),2(R4)                                                    
         BH    FLNUM20                                                          
         LA    RE,1(R2)            ADD 1                                        
         STC   RE,DARBFLTL(R2)     AND SET FLIGHT ON IN LIST                    
         CLI   DARBFLT,0           FIRST FLIGHT NUM SET?                        
         BNE   FLNUMEX                                                          
         STC   RE,DARBFLT          FIRST TIME, LETS SET IT                      
         B     FLNUMEX                                                          
*                                                                               
FLNUM20  LA    R4,4(R4)            NEXT FLIGHT                                  
         LA    R2,1(R2)            NEXT SEQ NUM                                 
         BCT   R5,FLNUM10          END OF FLIGHT TABLE?                         
*                                                                               
FLNUMEX  XIT1                                                                   
         DROP  R7                                                               
*                                                                               
DARBATCD DSECT                                                                  
DARBFLTA DS    0XL17                                                            
DARBFLT0 DS    C                   FLIGHT 0                                     
DARBFLTZ EQU   C'0'                FLIGHT 0 EQUATE                              
DARBFLTL DS    XL16                FLIGHT LIST, FOR 16 FLIGHTS                  
DARBFLT  DS    X                                                                
SVDBFLTEL DS   XL(DBFLTOVH)                                                     
DARFLG1  DS    X                                                                
DARF1RNF EQU   X'80'               RECORD NOT FOUND                             
DARF1UPF EQU   X'40'               FLIGHT ELEM UPDATED                          
DARF1UPD EQU   X'20'               WRITE RECORD BACK                            
DARF1UPP EQU   X'10'               UPDATE PASSIVE                               
DARF1SOF EQU   X'08'               HAVE SPOTS OUTSIDE FLIGHT                    
DARBATCL EQU   *-DARBATCD                                                       
T21104   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* RETURN BINARY TIME IN FULL                                                    
*==============================================================                 
         LTORG                                                                  
GETTIME  NTR1  BASE=*,LABEL=*                                                   
         XC    FULL,FULL                                                        
         THMS                      TIME R1=0HHMMSS+                             
         LR    R0,R1               CONVERT TO BINARY HMS                        
         SRDL  R0,12                                                            
         SRL   R1,20                                                            
         XC    DUB,DUB                                                          
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+2                                                        
         SRDL  R0,8                                                             
         SRL   R1,20                                                            
         LA    RF,X'0C'                                                         
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+1                                                        
         SRDL  R0,28                                                            
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL                                                          
         XIT1                                                                   
         LTORG                                                                  
*==============================================================                 
* CHECK IF RSNCODE REQUIRED AND MAKE SURE THERE IS ONE                          
*==============================================================                 
         SPACE 1                                                                
CHKRSN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   RE,SVB0PROF-BUYSAVE                                              
         AR    RE,RA                                                            
         CLI   7(RE),C'1'          TEST REASON CODE SCHEME 1                    
         BNE   CHKRSNX             NO                                           
         CLI   SVANYLOK,C'Y'       TEST LOCKED ORDER                            
         BNE   CHKRSNX             NO                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NORSNCOD)                                              
         LHI   RE,SVRSNEL-BUYSAVE                                               
         AR    RE,RA                                                            
         CLI   1(RE),0             TEST RSNCOD IS THERE                         
         BE    CHKRSNER                                                         
         LHI   RE,SVRSNFLD-BUYSAVE                                              
         AR    RE,RA                                                            
         MVI   0(RE),RCID_NEW      SET NEW BUY                                  
*                                                                               
         CLI   BUTRCODE,C'B'       TEST SINGLE BUY ACTION                       
         BNE   CHKRSNX             NO - LEAVE REASON CODE AROUND                
* CLEAR REASON CODE SO MUST BE RE-ENTERED EVERY TIME !                          
         LHI   RE,SVRSNEL-BUYSAVE                                               
         AR    RE,RA                                                            
         XC    0(69,RE),0(RE)                                                   
CHKRSNX  XIT1                                                                   
*                                                                               
CHKRSNER GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* SET FLAG FOR NO LOOKUP FOR COMSCORE DEMOS WITH OVERRIDES                      
*==================================================================             
                                                                                
SET50EL  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         LR    R7,R6               SAVE A(50EL)                                 
*                                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
*                                                                               
SET50EL2 LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         SRL   R0,3                DIVIDE BY 8                                  
         LA    R1,24(R6)                                                        
*                                                                               
SET50EL4 CLI   2(R1),0             TEST COMSCORE DEMO                           
         JNE   SET50EL6            NO                                           
         TM    4(R1),X'80'         TEST HAS OVRD                                
         JZ    SET50EL6            NO - LEAVE FLAG OFF                          
*                                                                               
         LLC   RE,1(R1)            GET INDEX                                    
         BCTR  RE,0                                                             
         MHI   RE,9                                                             
         LA    RE,2(RE,R7)         POINT TO DEMO IN 50 EL                       
         MVI   BYTE,X'80'          FLAG TO SET IF ORIG LKUP NOT REQD            
         CLI   0(R6),2                                                          
         JE    *+8                                                              
         MVI   BYTE,X'40'          FLAG TO SET IF SPL LKUP NOT REQD             
         OC    8(1,RE),BYTE        TURN OFF APPROPRIATE FLAG                    
*                                                                               
SET50EL6 LA    R1,8(R1)            NEXT DEMO                                    
         JCT   R0,SET50EL4                                                      
*                                                                               
         BRAS  RE,NEXTEL           NEXT DEMO ELEMENT                            
         JE    SET50EL2                                                         
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
* SPGENCLT                                                                      
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
* SPGENPRD                                                                      
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
* SPGENEST                                                                      
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPDBLBOOKD                                                     
         EJECT                                                                  
* BUY PROGRAM WORKING STORAGE DSECT AND DDOFFICED                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
* DSECT TO COVER ADD CONTROL VALUES (OVERLAYS WORK2)                            
*                                                                               
SPBUYWKD DSECT                                                                  
         ORG   WORK2                                                            
NEWLINE  DS    H                   PRE-ASSIGNED LINE NUMBER                     
IOAREASW DS    X                                                                
REPLSW   DS    C                   Y=REPLACING EXISTING BUY LINE                
BRDLST   DS    XL3                 BRAND LIST AREA FOR PUTREC                   
         SPACE 2                                                                
* DSECT TO COVER COPY CONTROL VALUES (OVERLAYS WORK2)                           
*                                                                               
SPBUYWKD DSECT                                                                  
         ORG   WORK2                                                            
FRKEY    DS    0CL(L'BUYKEY)                                                    
FRAGYMED DS    X                   FROM AGENCY/MEDIA                            
FRCLT    DS    XL2                 FROM CLIENT (PACKED)                         
FRPRDN   DS    X                   FROM PRODUCT NUMBER                          
FRMSTA   DS    CL5                 FROM MARKET/STATION (PACKED)                 
FREST    DS    X                   FROM ESTIMATE                                
         DS    X                                                                
FRLIN    DS    XL2                 FROM BUY LINE                                
*                                                                               
DEMOPT   DS    C                   C'S'=KEEP SAME DEMOS                         
COMOPT   DS    C                   C'C'=PRESERVE COMMENTS                       
OTHOPT   DS    X                   X'80'=KEEP ORBITS                            
*                                  X'40'=KEEP IDS                               
NREPS    DS    X                   N'REPITITIONS (SINGLE LINE COPY)             
*                                                                               
FRSTLIN  DS    H                   START BUY LINE FOR COPY                      
FRENDLIN DS    H                   END BUY LINE FOR COPY                        
*                                                                               
NCOPIES  DS    H                   N'COPIED BUY LINES                           
NERRS    DS    H                   N'ERRORS IN COPY                             
*                                                                               
FRCLI    DS    CL3                 FROM CLIENT (EBCDIC)                         
FRCLITYP DS    C                   FROM CLIENT TYPE (CPROF+0 VALUE)             
FRPRD    DS    CL3                 FROM PRODUCT (EBCDIC)                        
FRPRDNI  DS    X                   ACTUAL FROM PRODUCT NUMBER                   
FRESTST  DS    XL3                 FROM ESTIMATE START (YMD)                    
FRESTEND DS    XL3                 FROM ESTIMATE END (YMD)                      
*                                                                               
PERST    DS    XL3                 BUY LINE PERIOD START (YMD)                  
PEREND   DS    XL3                 BUY LINE PERIOD END (YMD)                    
RDNEXT   DS    C                   C'Y'=READ FOR NEXT BUY LINE                  
UNALL    DS    C                   C'Y'=UNALLOCATE MASTER ALLOCATION            
COPYOPT  DS    X                   COPY OPTIONS                                 
CONOCOST EQU   X'80'               NOCOST OPTION                                
*                                                                               
         DS    CL(L'WORK2-(*-FRAGYMED)) SPARE                                   
         SPACE 2                                                                
       ++INCLUDE SPGENDRBTC                                                     
       ++INCLUDE SPGENDRORD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPBUY04   11/19/19'                                      
         END                                                                    

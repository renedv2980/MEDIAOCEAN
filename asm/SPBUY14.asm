*          DATA SET SPBUY14    AT LEVEL 053 AS OF 11/02/16                      
*PHASE T21114B  <====                                                           
                                                                                
*==============================================================                 
*   JUN16 MHER CALL DEMO LOOKUPS FOR MARKET 0                                   
* 25MAY05 MHER FIX NXPLOD                                                       
* 27APR05 MHER SUPPORT FOR SPGENDOV LOOKUP VS. ZERO OVERRIDES                   
*              + NO COST OVRD BITS + GLOBAL IMPS IN NTWK 02 ELEM                
* 01OCT03 MHER SUPPORT FOR SOFT DEMO LOOKUPS                                    
* 27MAY03 MHER ALLOW CANADIAN EXPLODED BUYS IN DOLLARS                          
*==============================================================                 
                                                                                
         TITLE 'T21114 - SPOTPAK BUY - CANADIAN NTWK BUYS'                      
T21114   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21114                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21114+4096,R9                                                   
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         CLC   =C'NSET',8(R2)      TEST SET NETWORK PCTGS                       
         BE    NSET                                                             
         CLC   =C'CSET',8(R2)      TEST SET NETWORK CUT-IN LIST                 
         BE    CSET                                                             
         CLC   =C'CCLR',8(R2)      TEST CLEAR CUT-IN LIST                       
         BE    CCLR                                                             
         CLC   =C'NXPLOD',8(R2)                                                 
         BE    B900                                                             
*                                                                               
         MVI   BUDEMSW,0           RESET DEMO RESEQ SWITCH                      
         MVI   BUSTAT,X'20'        INDICATE CANADIAN NETWORK LEVEL BUY          
         XC    SVSPLMKT,SVSPLMKT                                                
* BUILD SKELETON DEMO ELEM                                                      
         XC    BUDEMS,BUDEMS                                                    
         LA    R6,SVDEMOS                                                       
         CLI   SVBRDEMS,0                                                       
         BZ    *+8                                                              
         LA    R6,SVBRDEMS                                                      
         LA    R1,BUDEMS+24                                                     
B10      CLI   0(R6),0                                                          
         BE    B12                                                              
         MVC   0(1,R1),0(R6)                                                    
         LA    R1,4(R1)                                                         
         LA    R6,9(R6)                                                         
         B     B10                                                              
*                                                                               
B12      LA    R0,BUDEMS                                                        
         SR    R1,R0                                                            
         STC   R1,BUDEMS+1         SET ELEM LENGTH                              
         MVI   BUDEMS,2            AND ELEM CODE                                
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)            ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         CLI   BUTRCODE,C'B'       BUY                                          
         BE    B200                                                             
         MVI   ERRCD,TRCDERR                                                    
         B     BUYERR                                                           
         EJECT                                                                  
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
**************************************************************                  
*       SUBR TO EXTRACT DEMO VALUES FROM SAVED OVRD DATA     *                  
*                                                            *                  
*       OVERRIDE RECORD (IN REC4) ELEMENT LENGTHS ARE        *                  
*                                                            *                  
*                 ....NEW.....                               *                  
*                                                            *                  
*        01      12 + 3*N'DEMOS                              *                  
*        02       2 + 5*N'DEMOS                              *                  
*        05       5 + 2*N'DEMOS                              *                  
**************************************************************                  
         SPACE 1                                                                
GETDEMOS NTR1                                                                   
*                                                                               
         L     R6,AREC4                                                         
         CLI   0(R6),0             TEST OVRD REC SAVED                          
         BE    GETDEMX             NO                                           
         ST    R6,FULL             SAVE CURRENT REC ADDRESS                     
*                                                                               
         XC    ELEM,ELEM           CLEAR WORK AREA (5 BYTES/DEMO)               
         LA    R2,ELEM             SET WORK AREA POINTER                        
*                                                                               
GETDEM2  L     R6,FULL             POINT TO CURRENT REC                         
         LA    R6,24(R6)           POINT TO FIRST ELEM                          
*                                                                               
         MVI   ELCDLO,2            TEST 02 (IMP OVRD ELEM)                      
         MVI   ELCDHI,2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   GETDEM18                                                         
*                                                                               
GETDEM4  SR    RF,RF                                                            
         IC    RF,1(R6)            GET ELEMENT LEN                              
         AHI   RF,-2                                                            
         BNP   GETDEM18                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)            GET ELEM LEN AGAIN                           
         SRDL  R0,32                                                            
         D     R0,=F'5'            NOTE REMAINDER IS DROPPED                    
         LR    RF,R1               SET FOR BCT                                  
*                                                                               
GETDEM6  LA    R6,2(R6)            POINT TO FIRST IMP (IN 02 EL)                
         EJECT                                                                  
GETDEM8  DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,3(R6)          GET VALUE                                    
         STH   R1,HALF             SAVE FOR MKT 0                               
         OC    BUYKMKT,BUYKMKT     TEST NETWORK BUY                             
         BZ    GETDEM12            YES                                          
         AR    R1,R1               X 2                                          
         M     R0,SVNPCTG          X RGN SHARE OF NTWK                          
         D     R0,=F'100000'       RESULT IN R1                                 
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         OC    BUCOST(3),BUCOST    TEST NTWK COST = 0                           
         BNZ   GETDEM10            NO                                           
* ZERO - RECALCULATE PERCENTAGE                                                 
         ICM   R0,15,10(R7)                                                     
         AR    R0,R0               X 2                                          
         MR    R0,R0               X STA SHARE OF RGN                           
         D     R0,=F'100000'                                                    
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STH   R1,HALF                                                          
         B     GETDEM12                                                         
         SPACE 1                                                                
* COST NOT 0 - MULT BY STA COST / NTWK COST *                                   
         SPACE 1                                                                
GETDEM10 L     R0,BDCOST           STATION COST (IN PENNIES)                    
         SRL   R0,8                                                             
         AR    R0,R0               X 2                                          
         MR    R0,R0                                                            
         L     R5,BUCOST                                                        
         SRL   R5,8                                                             
         TM    BUCIND2,X'01'       TEST NTWK COST ALREADY IN PENNIES            
         BO    *+8                 YES                                          
         MH    R5,=H'100'          CONVERT NTWK COST TO PENNIES                 
         DR    R0,R5                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STH   R1,HALF                                                          
*                                                                               
GETDEM12 MVC   0(3,R2),0(R6)       MOVE DEMO NUMBER                             
*                                                                               
         NI    0(R2),X'7F'         CLEAR 'INPUT' FLAG                           
         MVC   3(2,R2),HALF        MOVE VALUE                                   
         OC    HALF,HALF                                                        
         BNZ   *+8                                                              
         OI    3(R2),X'80'         SET ZERO OVERRIDE FLAG                       
         LA    R2,5(R2)            NEXT SLOT                                    
         LA    R6,5(R6)            NEXT DEMO VALUE                              
         BCT   RF,GETDEM8                                                       
         SPACE 1                                                                
GETDEM18 CLC   FULL,AREC4          TEST FIRST DEMO OVRD REC                     
         BNE   GETDEM20            NO                                           
         L     R6,AREC5                                                         
         ST    R6,FULL                                                          
         CLI   0(R6),0             TEST FOR SECOND RECORD                       
         BNE   GETDEM2             YES - GO PROCESS                             
*                                                                               
         EJECT                                                                  
GETDEM20 DS    0H                                                               
         MVC   FULL,AREC4          RESET REC ADDRESS=AREC4                      
*                                                                               
         MVI   ELCDLO,2            FIND O2 DEMO ELEM                            
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*==================================================================             
* NOW FIND 05 ELEM FOR THIS STATION (R7 POINTS TO STA SAVE AREA)                
*              AND REC4/REC5 HAVE DEMO OVERRIDE RECS                            
*==================================================================             
                                                                                
GETDEM21 OC    BUYKMKT,BUYKMKT     TEST NETWORK BUY                             
         BZ    GETDEM37            YES - NO 05 ELEMS                            
         L     R5,FULL             POINT TO DEMO OVRD REC                       
         LA    R5,24(R5)                                                        
         SR    RE,RE                                                            
         ICM   RE,7,2(R7)          GET PACKED CALL LETTERS (FROM LIST)          
         CLI   4(R7),X'B0'         TEST CANADIAN CABLE STATION >=B0             
         BNL   GETDEM24            YES - NO SHIFT                               
         SRL   RE,8                DROP MEDIA BITS                              
         B     GETDEM24                                                         
*                                                                               
GETDEM22 SR    RF,RF                                                            
         ICM   RF,7,2(R5)                                                       
         CLI   4(R5),X'B0'         TEST CANADIAN CABLE STATION >=B0             
         BNL   GETDEM23            YES- NO SHIFT                                
         SRL   RF,8                                                             
GETDEM23 CR    RE,RF               TEST RIGHT STATION                           
         BE    GETDEM26                                                         
*                                                                               
GETDEM24 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    GETDERR1                                                         
         CLI   0(R5),5                                                          
         BE    GETDEM22                                                         
         B     GETDEM24                                                         
*                                                                               
GETDEM26 CLI   0(R6),2             TEST SPILL MKT DEMOS                         
         BE    GETDEM30            NO                                           
         EJECT                                                                  
* FIND SPILL MKT OVRD *                                                         
         SPACE 1                                                                
GETDEM28 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),5                                                          
         BNE   GETDERR2                                                         
         CLI   2(R5),0             TEST SPILL OVRD                              
         BNE   GETDERR2                                                         
         CLC   3(2,R5),4(R6)       RIGHT MKT                                    
         BNE   GETDEM28                                                         
         SPACE 1                                                                
* NOW EXTRACT VALUES FROM 05 ELEMENT *                                          
         SPACE 1                                                                
GETDEM30 L     R4,FULL             FULL HAS DEMO OVRD REC ADDRESS               
         LA    RE,24(R4)                                                        
         SR    RF,RF                                                            
         IC    RF,1(RE)                                                         
         AHI   RF,-12                                                           
         BNP   GETDEM56                                                         
         SR    RE,RE                                                            
         D     RE,=F'3'            RF NOW SET FOR BCT                           
         L     RE,FULL POINT TO REC                                             
         LA    RE,24+12(RE)        OVRD LIST (IN O1 EL)                         
         LA    R4,5(R5)            POINT TO DEMO OVRD VAL (IN 05 EL)            
*                                                                               
GETDEM32 MVC   0(3,R2),0(RE)       MOVE DEMO NUM TO WORK AREA                   
*                                                                               
GETDEM36 MVC   3(2,R2),0(R4)       MOVE DEMO VALUE                              
         NI    0(R2),X'7F'         DROP 'INPUT' IND                             
         LA    R2,5(R2)            NEXT WORK AREA SLOT                          
         LA    R4,2(R4)            NEXT DEMO VALUE                              
         LA    RE,3(RE)            NEXT DEMO NUMBER                             
         BCT   RF,GETDEM32                                                      
*                                                                               
GETDEM37 CLC   FULL,AREC4          TEST POINTING TO FIRST OVRD REC              
         BNE   GETDEM40                                                         
         L     RE,AREC5            POINT TO SECOND                              
         CLI   0(RE),0             TEST IT IS THERE                             
         BE    GETDEM40            NO                                           
         ST    RE,FULL             ELSE SET CURRENT OVRD REC ADDR               
* BUT NEED TO MAKE SURE THERE ARE 05 ELEMENTS OR IGNORE IT                      
         LA    RE,24(RE)                                                        
         SR    R0,R0                                                            
GETDEM38 CLI   0(RE),0                                                          
         BE    GETDEM40            NONE                                         
         CLI   0(RE),5                                                          
         BE    GETDEM21            YES - GO AND PROCESS                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     GETDEM38                                                         
         EJECT                                                                  
* NOW MOVE VALUES TO DEMO EL *                                                  
         SPACE 1                                                                
GETDEM40 SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AHI   RF,-24                                                           
         BNP   EXIT                                                             
         LA    RE,24(R6)                                                        
         SRL   RF,3                SET FOR BCT                                  
*                                                                               
GETDEM51 LA    R2,ELEM             POINT TO WORK AREA                           
*                                                                               
GETDEM52 CLC   0(3,R2),0(RE)       MATCH DEMO NUM                               
         BE    GETDEM54                                                         
         LA    R2,5(R2)                                                         
         CLI   1(R2),0                                                          
         BNE   GETDEM52                                                         
         B     GETDEM56                                                         
*                                                                               
GETDEM54 MVC   6(2,RE),3(R2)       MOVE DEMO VALUE                              
         OC    6(2,RE),6(RE)       TEST VALUE = LOOKUP                          
         BZ    GETDEM56                                                         
         CLC   6(2,RE),=X'8000'    TEST VALUE = ZERO                            
         BNE   *+10                                                             
         XC    6(2,RE),6(RE)       THEN MAKE IT ZERO                            
         OI    4(RE),X'80'         SET OVRD IND                                 
         MVI   3(RE),100           SET HUT                                      
*                                                                               
GETDEM56 LA    RE,8(RE)                                                         
         BCT   RF,GETDEM51                                                      
         EJECT                                                                  
GETDEM57 DS    0H                                                               
         L     R5,AREC4                                                         
         USING DOVRECD,R5                                                       
         MVC   2(2,R6),DOVBBK      SET BASE BOOK                                
*                                                                               
GETDEM58 XC    ELEM,ELEM           CLEAR                                        
         LA    R2,ELEM              AND RESET POINTER                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),3                                                          
         BNE   GETDEMX                                                          
         CLI   1(R6),24            TEST ANY DEMOS IN ELEMENT                    
         BNH   GETDEM58                                                         
         MVC   FULL,AREC4          RESTORE FIRST REC ADDRESS                    
         B     GETDEM21                                                         
                                                                                
*==========================================================                     
* CALL DEMO LOOK-UPS TO SUPPLY MISSING OVERRIDES                                
*==========================================================                     
GETDEMX  DS    0H                                                               
         OC    BUYKMKT,BUYKMKT     TEST MARKET 0 CALL                           
         JNZ   GETDEMX2            NO - GO DO LOOKUP                            
*                                                                               
         MVI   SVNDINDX,X'FF'      SET FLAG FOR DMLKUP IN BUY00                 
         LHI   RE,SVD0PROF-BUYSAVE ELSE CHECK D0 PROF                           
         AR    RE,RA                                                            
         CLI   13(RE),C'Y'                                                      
         JE    GETDEMX2                                                         
         CLI   13(RE),C'B'         TEST LOOK UP NETWORK DEMOS                   
         JNE   EXIT                                                             
*                                                                               
GETDEMX2 GOTO1 DEMLKUP                                                          
         B     EXIT                                                             
         SPACE 2                                                                
         USING SVNPGMD,R7                                                       
GETDERR1 OI    SVNPSHR,X'80'       SET STATION DEMOS MISSING FLAG               
         B     GETDEMX                                                          
GETDERR2 OI    SVNPSHR,X'40'       SET SPILL DEMOS MISSING FLAG                 
         B     GETDEMX                                                          
         DROP  R7                                                               
         EJECT                                                                  
* NEW BUY                                                                       
*                                                                               
B200     DS    0H                                                               
         TM    SVOPT1,SVOPT1_VNDRLCK  X'10' - VENDOR LOCKED?                    
         BNZ   B215                   YES, NO NEW BUYS                          
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
         MVI   EDTVAL,ADJEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,COSTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         CLI   FSTOP,C','                                                       
         BNE   B250                                                             
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'='                                                       
         BNE   B250                                                             
*                                                                               
* CHECK FOR MASTER ALLOC OR PARTNER ENTRY                                       
B213     CLC   =C'M=',0(R4)                                                     
         BE    B220                                                             
         MVI   ERRCD,BADCOMMA                                                   
         B     BUYERR                                                           
*                                                                               
B215     LA    R2,BUYSTH           POINT TO THE STATION                         
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(STALCKED)  STA LOCKED - CANNOT TRANSFER..            
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
         EJECT                                                                  
B250     MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,C','                                                       
         BE    BUYERR                                                           
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
B252     LA    R0,BUYREC           UPDATE FOR 4K IDIOT                          
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
* FIND LINE NUMBER                                                              
         BAS   RE,NXTBUYLN                                                      
*                                                                               
         MVC   BUYRLEN,=H'94'                                                   
         MVC   BUYALPHA,AGYALPHA                                                
*                                                                               
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,70                                                         
*                                                                               
         MVC   BDSTART(6),BUSTARTB                                              
*                                                                               
* NOTE BDWKS SET IN CALEND                                                      
         MVC   BDINPUT,BUPERIND                                                 
         MVC   BDWKIND,BUWKIND                                                  
         MVC   BDDAY,BUDAYS                                                     
         MVC   BDSEDAY,BUDAYNUM                                                 
         MVC   BDNOWK,BUNPW                                                     
         MVC   BDSEC,BUSLN                                                      
         MVC   BDDAYPT,BUDPT                                                    
         MVC   BDTIMST(4),BUTIME                                                
         BAS   RE,BLDPRG           BUILD PROGRAM NAME                           
         MVC   BDPROGT,BUADJ                                                    
         MVC   BDCOST(4),BUCOST                                                 
         MVC   BDREP,SVESTREP                                                   
         OC    SVOPTREP,SVOPTREP                                                
         BZ    *+10                                                             
         MVC   BDREP,SVOPTREP                                                   
         OC    BDREP,BDREP                                                      
         BNZ   *+10                                                             
         MVC   BDREP,BUREP                                                      
         MVC   BDSTAT,BUSTAT       X'20'                                        
         MVC   BDCIND2,BUCIND2     X'01'=RATE IN PENNIES                        
         OI    BDCIND2,X'20'       SET CANADIAN FLAG                            
         OI    BDSTAT3,BDST3_CNNEW  SET COST OVRDS ALLOWED FLAG                 
*                                                                               
         CLI   BUXSW,C'N'           TEST SUPPRESS STA EXCEPTIONS                
         BNE   *+8                                                              
         OI    BDSTAT2,X'40'                                                    
         MVI   BUWHY,X'80'          SET NEW BUY                                 
         GOTO1 SETCHGDT                                                         
*                                                                               
         MVC   BDADVAGY,SVADVAGY                                                
* SET MASPRDS AND WORK OUT ELEM LENS                                            
         MVC   BDMASPRD,BUELPRD                                                 
*                                                                               
B260     B     B280                                                             
         EJECT                                                                  
B272     DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         TM    SVSPOMAK,SVSPOMAK_NOBUY TEST DO-NOT-ADD                          
         BO    B272X                                                            
         OC    KEY+14(4),KEY+14    HOPEFULLY DOING U=N                          
         BZ    B272X                                                            
         GOTO1 GETREC              REREAD NETWORK BUY                           
*                                                                               
B272X    MVI   RCLOPT,RCLSTA       FORCE STATION LIST DISPLAY                   
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BZ    B272X2              NO                                           
         MVI   RCLOPT,RCLROT       FORCE ROTATION DISPLAY                       
         B     B273X                                                            
*                                                                               
B272X2   SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    RE,R2               HAVE A LOOK AT NEXT INPUT LINE               
         CLC   =C'C,MG=',8(RE)     IS IT A MAKEGOOD                             
         BNE   *+8                                                              
         MVI   RCLOPT,RCLROT       SET FOR ROTATION DISPLAY                     
* TEST FOR DEMO OVRD ERRORS                                                     
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
B273A    TM    SVNPSHR,X'C0'                                                    
         BNZ   B273X                                                            
         LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B273A                                                            
         TM    SVOPTS,X'04'        ROTATION REQ                                 
         BZ    *+8                 NO                                           
         MVI   RCLOPT,RCLROT                                                    
         DROP  R7                                                               
*                                                                               
B273X    MVC   BUYMSG(33),=C'** BUY ADDED. NOTE LINE NUMBER **'                 
         GOTO1 CALLDSP                                                          
*                                                                               
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BO    EXIT                YES                                          
*                                                                               
* MOVE '*' TO CHAR 1  OF INPUT LINE                                             
*                                                                               
B274     DS    0H                                                               
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BO    EXIT                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   =C'C,MG=',8(R2)     ALLOW MG AFTER NEW BUY                       
         BE    *+8                                                              
         MVI   5(R2),0             SUPPRESS FURTHER INPUT                       
*                                                                               
B278     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    EXIT                                                             
         C     R2,FLAST                                                         
         BH    EXIT                                                             
         CLI   8(R2),C'C'          IS LINE 3 A CHANGE TRANSACTION               
         BE    B278                                                             
* KILL ALL REMAINING LINES                                                      
B278A    MVI   5(R2),0                                                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    EXIT                                                             
         C     R2,FLAST                                                         
         BH    EXIT                                                             
         B     B278A                                                            
         EJECT                                                                  
B280     DS    0H                                                               
         CLI   BUORB,C'='          TEST SIMULCAST                               
         BE    B290                YES                                          
*                                                                               
* MAKE SURE ALL BUYS LIE IN EST PERIOD                                          
* FIRST FIND LARGEST NEGATIVE REL DAY                                           
*                                                                               
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
         SR    R0,R0                                                            
B282     TM    SVNPDAY,X'80'       TEST NEG REL DAY                             
         BZ    B283                                                             
         MVC   HALF,SVNPDAY                                                     
         LH    R1,HALF                                                          
         CR    R0,R1                                                            
         BL    *+6                                                              
         LR    R0,R1                                                            
B283     LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B282                                                             
         LTR   R0,R0                                                            
         BZ    B284                                                             
         LA    R4,BDSTART                                                       
         BAS   RE,GETDATE                                                       
         MVI   ERRCD,ESPERERR                                                   
         CLC   BDSTART,SVSTARTB                                                 
         BL    BUYERR                                                           
*                                                                               
* FIND LARGEST POSITIVE VALUE                                                   
*                                                                               
B284     L     R7,AREC3                                                         
         SR    R0,R0                                                            
B286     TM    SVNPDAY,X'80'                                                    
         BZ    B287                                                             
         MVC   HALF,SVNPDAY                                                     
         CH    R0,HALF                                                          
         BH    *+8                                                              
         LH    R0,HALF                                                          
B287     LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BZ    B286                                                             
         LTR   R0,R0                                                            
         BZ    B288                                                             
         LA    R4,BDEND                                                         
         BAS   RE,GETDATE                                                       
         MVI   ERRCD,ESPERERR                                                   
         CLC   BDEND,SVENDB                                                     
         BH    BUYERR                                                           
*                                                                               
B288     MVC   BDSTART(6),BUSTARTB RESTORE CORRECT DATES                        
         DROP  R7                                                               
         EJECT                                                                  
* CREATE NTWK AND EXPLODED BUYS                                                 
*                                                                               
B290     GOTO1 VBLDEL                                                           
         BAS   RE,CALEND                                                        
         BAS   RE,CHKMAXEL                                                      
*                                                                               
         OC    BUYKMKT,BUYKMKT     TEST MARKET 0 BUY                            
         JNZ   *+8                                                              
         MVI   SVNDINDX,X'FF'                                                   
         GOTO1 VBLDDEM                                                          
         BRAS  RE,GETDEMOS         GET GLOBAL IMPS FOR NTWK REC                 
*                                                                               
         CLC   BDSTART,=X'6E061C'  TEST BUY STARTS BEFORE JUL/10                
         BL    B291B               YES                                          
         MVI   ELEM,X'6B'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),SVPST    JUL/10+ GETS NETWORK VALUES                  
*                                                                               
         LA    R6,BDELEM                                                        
B291A    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   B291A                                                            
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
B291B    LA    R6,BDELEM                                                        
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   20(4,R6),SVNPCTG    SAVE REGION PCTG IN DEMO ELEMENT             
*                                                                               
         MVI   BDCANAD,X'80'       SET TO GEN COMBINED POINTERS                 
*                                                                               
         BAS   RE,PCTADJ           RE-ALLOCATE PERCENTAGES                      
* SET UP TO ADD X'68' STATION ELEMENTS                                          
         MVC   BDNRGN,SVNRGN       SET NTWK RGN CODE IN BDELEM                  
         MVI   ELEM,X'68'                                                       
         MVI   ELEM+1,11                                                        
         LA    R6,BDELEM           FIND E-O-R                                   
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    *+8                                                              
         B     *-14                                                             
         EJECT                                                                  
*============================================================*                  
* ADD ELEMENT FOR EACH STATION IN BUY                        *                  
*============================================================*                  
                                                                                
         MVC   AREC,AREC1          SET I/O AREA FOR RECUP                       
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
B292     MVC   ELEM+2(5),SVNPMKST  MOVE MKT-STA                                 
         MVC   ELEM+7(4),SVNPSHR   SHARE                                        
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B292                                                             
         DROP  R7                                                               
*                                                                               
         CLI   BUORB,C'='          TEST SIMULCAST                               
         BNE   B306                NO                                           
*                                  ADD NETWORK BUY                              
         BAS   RE,BLDID            ADD ID ELEMENT IF NEEDED                     
         TM    SVSPOMAK,SVSPOMAK_NOBUY TEST DO-NOT-ADD                          
         BO    B272X                                                            
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14        SAVE DISK ADDR                         
*                                                                               
         MVC   BUYRLEN,=H'94'                                                   
         LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         XC    0(2,R6),0(R6)       CLEAR ELEMS                                  
*                                                                               
         XC    SVSPLMKT,SVSPLMKT                                                
         MVI   BUDEMSW,0                                                        
         GOTO1 VBLDDEM                                                          
         GOTO1 VBLDEL                                                           
         BAS   RE,CALEND                                                        
*                                                                               
         EJECT                                                                  
* EXPLODE SIMULCAST                                                             
*                                                                               
B293     DS    0H                                                               
         BAS   RE,ALLODOL                                                       
         MVI   BDCANAD,X'80'       GEN COMBINED PTRS                            
*                                                                               
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
B294     MVC   REC+4(5),SVNPMKST   SET MKT/STA                                  
         MVC   SVNDINDX,SVNPSEQ    SET SEQNUM FOR STA OVRD                      
         MVC   BDCIND2,BUCIND2                                                  
         OI    BDCIND2,X'01'       SET RATE IN PENNIES                          
         OC    BUCOST(3),BUCOST    TEST NTWK COST 0                             
         BZ    *+10                                                             
         MVC   BDCOST(3),SVNPSHR+1 MOVE COST TO BUY                             
* NOT SO FAST - IF IT'S TOO BIG, MAKE IT IN DOLLARS FOR ANNIE                   
         CLI   SVNPSHR,0           ANY BITS ON                                  
         BE    B294A               NO - IT BE FINE                              
                                                                                
         ICM   R1,15,SVNPSHR                                                    
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,7,BDCOST                                                      
         NI    BDCIND2,X'FF'-X'01' TURN OFF 'RATE IN PENNIES'                   
         OI    BDCIND2,X'10'       SET RATE IN DOLLARS                          
*                                                                               
B294A    TM    SVOPT2,X'40'        TEST NO TAX OPTION                           
         BO    *+10                                                             
         MVC   BDNTAX,SVNPTAX      MOVE TAX RATE TO BUY                         
*                                                                               
         MVI   BUDEMSW,0                                                        
         GOTO1 VBLDDEM             CREATE NEW DEMO ELEMENT                      
         GOTO1 VGETSPLL            ADD SPILL DEMO ELEMENTS                      
* CALCULATE TIME FOR THIS STA                                                   
         MVC   FULL(2),BUTIME                                                   
         MVC   FULL+2(2),SVNPTIM                                                
         BAS   RE,ADDTIME                                                       
         MVC   BDTIMST,FULL+2                                                   
*                                                                               
         MVC   FULL(2),BUTIME+2                                                 
         OC    FULL(2),FULL                                                     
         BZ    B294X                                                            
         MVC   FULL+2(2),SVNPTIM                                                
         BAS   RE,ADDTIME                                                       
         MVC   BDTIMEND,FULL+2                                                  
*                                                                               
B294X    BAS   RE,GETDEMOS                                                      
         BAS   RE,ADDNETEL                                                      
         BAS   RE,BLDPST                                                        
         BAS   RE,BLDID                                                         
*                                                                               
         GOTO1 ADDREC                                                           
* DELETE ALL BUT BDELEM AND REGELS                                              
         BRAS  RE,DELELS                                                        
*                                                                               
B296X    LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B294                                                             
         DROP  R7                                                               
*                                                                               
         B     B272                                                             
         EJECT                                                                  
*================================================================               
* SUM PCTS - REDISTRIBUTE IF NEEDED - THEN ALLOCATE COST                        
* PROGRAM EDIT ROUTINE BUILT STATION LIST IN REC3                               
*================================================================               
                                                                                
PCTADJ   NTR1                                                                   
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
         SR    R8,R8                                                            
PCTADJ2  ICM   R0,15,SVNPSHR        SUM PCTS                                    
         AR    R8,R0                                                            
         LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   PCTADJ2                                                          
         C     R8,=F'100000'                                                    
         BE    PCTADJX                                                          
         MVI   ERRCD,NOSTATNS                                                   
         LTR   R8,R8                                                            
         BZ    BUYERR                                                           
* DISTRIBUTE PCTS                                                               
         L     R7,AREC3                                                         
PCTADJ4  ICM   R1,15,SVNPSHR                                                    
         M     R0,=F'200000'                                                    
         DR    R0,R8                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         STCM  R1,15,SVNPSHR                                                    
         LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   PCTADJ4                                                          
* CHECK SUM AND ADJUST LARGEST                                                  
         SR    R8,R8                                                            
         L     R7,AREC3                                                         
         LR    RE,R7                                                            
*                                                                               
PCTADJ6  ICM   R0,15,10(R7)                                                     
         AR    R8,R0                                                            
         CLC   SVNPSHR-SVNPGMD(4,RE),SVNPSHR                                    
         BH    *+6                                                              
         LR    RE,R7               SAVE A(LARGEST)                              
         LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   PCTADJ6                                                          
*                                                                               
         S     R8,=F'100000'       GET ERROR AMOUNT                             
         ICM   R0,15,SVNPSHR-SVNPGMD(RE)                                        
         SR    R0,R8                                                            
         STCM  R0,15,SVNPSHR-SVNPGMD(RE)                                        
PCTADJX  B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
B306     DS    0H                                                               
         BAS   RE,BLDPRG                                                        
*                                                                               
B308     BAS   RE,BLDID            ADD ID ELEMENT IF NEEDED                     
         TM    SVSPOMAK,SVSPOMAK_NOBUY TEST DO-NOT-ADD                          
         BO    B272X                                                            
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDR                               
* DELETE ALL BUT BDELEM                                                         
         MVC   BUYRLEN,=H'94'                                                   
         LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         XC    0(2,R6),0(R6)       CLEAR ELEMS                                  
                                                                                
*                                                                               
         MVI   BUDEMSW,0                                                        
         GOTO1 VBLDDEM                                                          
*                                                                               
         BAS   RE,ALLODOL          ALLOCATE DOLLARS                             
         EJECT                                                                  
* EXPLODE BUY                                                                   
*                                                                               
B318     MVI   BDCANAD,X'80'       GEN CMBD PTRS                                
         MVC   BDPROGRM,BUPROG     SET ACTUAL PROGRAM NAME                      
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
B320     MVC   BUYREC+4(5),SVNPMKST   SET MKT/STA                               
         MVC   SVNDINDX,SVNPSEQ    SET SEQNUM FOR STA OVRD                      
*                                                                               
         MVI   BUDEMSW,0                                                        
         GOTO1 VBLDDEM             CREATE NEW DEMO ELEMENT                      
         GOTO1 VGETSPLL                                                         
* CALCULATE DAY                                                                 
         MVC   BDSTART(6),BUSTARTB  RESET START/END DATES                       
         MVC   BDDAY,BUDAYS                                                     
         MVC   BDSEDAY,BUDAYNUM                                                 
         SR    R0,R0                                                            
         ICM   R0,12,SVNPDAY        REL DAY                                     
         BZ    B324                                                             
         SRA   R0,16                PROPAGATE SIGN BIT                          
* CALCULATE DAYS                                                                
         LA    R4,BDSTART                                                       
         BAS   RE,GETDATE          ON RETN, WORK+6 HAS EBCDIC DATE              
         GOTO1 VGETDAY,DMCB,WORK+6,DUB                                          
         LA    R1,DAYTAB                                                        
         CLC   0(2,R1),DUB                                                      
         BE    B322                                                             
         LA    R1,3(R1)                                                         
         B     *-14                                                             
DAYTAB   DC    C'MO',X'40'                                                      
         DC    C'TU',X'20'                                                      
         DC    C'WE',X'10'                                                      
         DC    C'TH',X'08'                                                      
         DC    C'FR',X'04'                                                      
         DC    C'SA',X'02'                                                      
         DC    C'SU',X'01'                                                      
*                                                                               
B322     MVC   BDDAY,2(R1)                                                      
*                                                                               
         LA    R4,BDEND                                                         
         BAS   RE,GETDATE                                                       
         EJECT                                                                  
B324     DS    0H                                                               
         GOTO1 VBLDEL              REBUILD PROTOTYPE                            
         BAS   RE,CALEND            AND RE-CALENDARIZE                          
*                                                                               
         MVC   BDTIMST(4),BUTIME   SET DFLT TIME                                
         OC    SVNPTIM,SVNPTIM     TEST OVRD TIME                               
         BZ    B326                NO                                           
         MVC   BDTIMST,SVNPTIM     SET OVRD TIME                                
* CONVERT TIMES TO MINUTES TO CALC DURATION                                     
         MVC   FULL,BUTIME         ALIGN                                        
         LH    R0,FULL+2           END TIME                                     
         LTR   R0,R0                                                            
         BZ    B326                                                             
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         MH    R1,=H'60'                                                        
         AR    R0,R1                                                            
         LR    RE,R0               SAVE END TIME                                
*                                                                               
         LH    R0,FULL             START TIME                                   
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         MH    R1,=H'60'                                                        
         AR    R0,R1                                                            
*                                                                               
         SR    RE,R0               GIVES DURATION IN MINUTES                    
         STH   RE,FULL+2                                                        
         MVC   FULL(2),BDTIMST     ADD IT TO OVRD START TIME                    
         BAS   RE,ADDTIME          ADD DURATION TO OVRD START                   
         MVC   BDTIMEND,FULL+2      AND SET AS END TIME                         
*                                                                               
B326     MVC   BDDAYPT,BUDPT       USE INPUT DAYPART                            
         CLI   SVNPDPT,C' '        TEST OVERRIDE                                
         BNH   *+10                                                             
         MVC   BDDAYPT,SVNPDPT     SET DAYPART                                  
         OC    BUCOST(3),BUCOST    TEST NTWK COST = 0                           
         BZ    *+10                                                             
         MVC   BDCOST(3),SVNPSHR+1  SET COST SHARE                              
         MVC   BDCIND2,BUCIND2                                                  
         OI    BDCIND2,X'01'        STATION SHARE ALWAYS IN PENNIES !           
* NOT SO FAST - IF IT'S TOO BIG, MAKE IT IN DOLLARS FOR ANNIE                   
         CLI   SVNPSHR,0           ANY BITS ON                                  
         BE    B327                NO - IT BE FINE                              
*                                                                               
         ICM   R1,15,SVNPSHR                                                    
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,7,BDCOST                                                      
         NI    BDCIND2,X'FF'-X'01' TURN OFF 'RATE IN PENNIES'                   
         OI    BDCIND2,X'10'       SET RATE IN DOLLARS                          
*                                                                               
B327     TM    SVOPT2,X'40'         TEST NO TAX OPTION                          
         BO    *+10                                                             
         MVC   BDNTAX,SVNPTAX      SET TAX RATE                                 
         SPACE 1                                                                
         BAS   RE,GETDEMOS                                                      
         BAS   RE,ADDNETEL                                                      
         BAS   RE,BLDPST              BUILD PST ELEMENT                         
         BAS   RE,BLDID               BUILD ID ELEMENT                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT BEYOND BDELEM                          
         XC    0(256,R6),0(R6)     AND CLEAR                                    
         LA    R0,BUYREC                                                        
         SR    R6,R0                                                            
         STCM  R6,3,BUYREC+13      SET RECORD LENGTH                            
*                                                                               
B330     LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B320                                                             
         B     B272                                                             
         DROP  R7                                                               
         EJECT                                                                  
* ADD SIGNED TIME IN FULL+2(2) TO TIME IN FULL(2)                               
*                                                                               
ADDTIME  NTR1                                                                   
*                                                                               
         LH    R0,FULL             CONVERT TIME TO MINUTES                      
         SRDA  R0,32                                                            
         D     R0,=F'100'          MINUTES IN R0/ HOURS IN R1                   
         MH    R1,=H'60'                                                        
         AR    R0,R1                                                            
         AH    R0,FULL+2           ADD OFFSET IN MINUTES                        
*                                                                               
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         AH    R0,=H'1440'                                                      
*                                                                               
         CH    R0,=H'1440'                                                      
         BNH   *+8                                                              
         SH    R0,=H'1440'                                                      
*                                                                               
         SRDA  R0,32                                                            
         D     R0,=F'60'           MINUTES IN R0/ HOURS IN R1                   
         MH    R1,=H'100'                                                       
         AR    R0,R1                                                            
         STH   R0,FULL+2                                                        
         B     EXIT                                                             
         SPACE 2                                                                
* R4 POINTS TO 3 BYTE DATE                                                      
* R0 HAS REL DAY                                                                
*                                                                               
GETDATE  NTR1                                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,(R4)),WORK                                       
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(3,(R4))                                     
         B     EXIT                                                             
         EJECT                                                                  
* ALLOCATE DOLLARS USING PCTGS IN REC3                                          
*                                                                               
ALLODOL  NTR1                                                                   
*                                                                               
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
ALLOD2   ICM   R0,15,SVNPSHR       GET SHARE                                    
         BZ    ALLOD4                                                           
         SR    R1,R1                                                            
         ICM   R1,7,BUCOST         GET COST                                     
         BZ    EXIT                                                             
         AR    R1,R1               X 2                                          
         MR    R0,R0               X SHARE                                      
         LA    RE,1000                                                          
         TM    BUCIND2,X'01'       TEST COST IN PENNIES                         
         BZ    *+8                 NO                                           
         MHI   RE,100              YES                                          
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,15,SVNPSHR       GIVES COST IN CENTS                          
*                                                                               
ALLOD4   LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   ALLOD2                                                           
*                                                                               
* TEST SUM = BUCOST. ALL COSTS ARE IN PENNIES                                   
*                                                                               
         L     R7,AREC3                                                         
         SR    R8,R8                                                            
         LR    RE,R7                                                            
*                                                                               
ALLOD10  ICM   R1,15,SVNPSHR         GET COST IN PENNIES                        
         CLC   SVNPSHR,=X'00FFFFFF'  TEST AMOUNT FITS IN 3 BYTES                
         BL    ALLOD12               YES DO NOT ADJUST                          
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                GIVES COST IN DOLLARS                        
         MHI   R1,100              NOW CONVERT BACK TO PENNIES                  
         AR    R8,R1               AND ADD TO TOTAL                             
         B     ALLOD14             AND DO NOT CONSIDER ADJUSTING                
*                                                                               
ALLOD12  AR    R8,R1               SUM COSTS                                    
*                                                                               
         CLC   SVNPSHR-SVNPGMD(4,RE),SVNPSHR                                    
         BH    ALLOD14                                                          
         LR    RE,R7                 SAVE A(LARGEST)                            
*                                                                               
ALLOD14  LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   ALLOD10                                                          
*                                                                               
* ADJUST LARGEST AMOUNT THAT IS NOT IN DOLLARS, UNLESS THEY ARE                 
* ALL IN DOLLARS                                                                
*                                                                               
         L     R1,BUCOST                                                        
         SRL   R1,8                                                             
         TM    BUCIND2,X'01'       TEST COST IN PENNIES                         
         BO    *+8                                                              
         M     R0,=F'100'                                                       
         SR    R8,R1                                                            
         ICM   R0,15,SVNPSHR-SVNPGMD(RE)                                        
         SR    R0,R8                                                            
         STCM  R0,15,SVNPSHR-SVNPGMD(RE)                                        
*                                                                               
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
* CREATE REGELEMS FOR ALL WEEKS IN BUY PERIOD                                   
*                                                                               
CALEND   NTR1                                                                   
* FIND LAST ELEM                                                                
         LA    R6,BDELEM                                                        
         SR    R7,R7               CLEAR COUNTER                                
CAL4     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         BNE   CAL4                                                             
* GET EBCDIC START/END DATES                                                    
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 (RF),(R1),(3,BDEND),WORK+12                                      
*                                                                               
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
*                                                                               
CAL8     GOTO1 VDATCON,DMCB,WORK,(2,ELEM+2)                                     
         LA    R7,1(R7)                                                         
*                                                                               
         LA    R0,1                NON-POL GETS 1 ELEM/WEEK                     
         CLI   ELEM,X'06'                                                       
         BE    *+8                                                              
         IC    R0,BDNOWK           POL GETS 1 ELEM PER SPOT                     
*                                                                               
CAL10    GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         BCT   R0,CAL10            LOOP FOR NUMBER OF ELEMENTS                  
*                                                                               
         L     R0,FULL                                                          
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         CLC   WORK+6(6),WORK+12   TEST PAST END                                
         BH    CAL12                                                            
         MVC   WORK(6),WORK+6                                                   
         B     CAL8                                                             
*                                                                               
CAL12    STC   R7,BDWKS            SET NUMBER OF WEEKS IN BDELEM                
         B     EXIT                                                             
         EJECT                                                                  
* FIND NEXT AVAILABLE LINE NUMBER.                                              
* SVKEY HAS A-M/CLT/PRD/MKT-STA/EST                                             
* RETURN NEW KEY IN SVKEY AND SET KEY IN BUYREC                                 
*                                                                               
NXTBUYLN NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       MOVE A-M/CLT/PRD/EST/STA                     
         CLI   SV1OR2,2                                                         
         BNE   NXTBUY1                                                          
         CLC  =C'TESTBIG',BUYBU                                                 
         BNE   *+8                                                              
         MVI   KEY+12,255          START AT 256                                 
         B     NXTBUY1X                                                         
*                                                                               
NXTBUY1  XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       USE HIGHEST LINE SO FAR IF ANY               
*                                                                               
NXTBUY1X OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         B     NXTBUY4                                                          
*                                                                               
NXTBUY2  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
NXTBUY4  TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    NXTBUY2                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         ICM   RE,3,KEY+11                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+11                                                      
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
*                                                                               
         MVI   ERRCD,MAXLINES                                                   
         CLC   KEYSAVE+11(2),=H'255'   DID WE GET TO HIGHEST LINE?              
         BL    NXTBUY10                                                         
         CLI   SV1OR2,2                                                         
         BNE   NXTBUY8                                                          
         CLC   KEYSAVE+11(2),=Y(MAXBUYS)                                        
         BL    NXTBUY10                                                         
*                                                                               
NXTBUY8  MVC   WORK(L'BUYKEY),KEY                                               
         BAS   RE,FNDPREV          FIND PREVIOUS BUY LINE NOT IN USE            
         BNE   BUYERR                                                           
         MVC   KEY+11(2),HALF                                                   
         MVC   SVKEY(13),KEY       THANKS LISA - $500 TO HAMAS                  
*                                                                               
NXTBUY10 MVC   BUYKEY(10),KEY                                                   
         MVC   BUYKEY+10(2),KEY+11                                              
         B     EXIT                                                             
*                                                                               
FNDPREV  NTR1                                                                   
         MVC   WORK+20(L'BUYKEY),KEY                                            
         MVC   HALF,=H'1'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(10),WORK        SET A/M/CLT/PRD/MKSTA/EST                    
         OI    DMINBTS,X'08'       GET DELETED                                  
         GOTO1 HIGH                                                             
         B     FP20                                                             
                                                                                
FP10     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
                                                                                
FP20     TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   FPNO                                                             
         CLC   HALF,KEY+11         NEXT NUMERICAL NUMBER ON FILE                
         BNE   FP30                                                             
                                                                                
FP25     LH    R0,HALF                                                          
         AHI   R0,1                INCREMENT LAST LINE NUMBER                   
         STH   R0,HALF                                                          
         B     FP10                                                             
                                                                                
FP30     OC    HALF,HALF                                                        
         BZ    FP25                                                             
         LA    R0,999                                                           
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    R0,255                                                           
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
         B     BUYERR                                                           
         SPACE 2                                                                
ADDNETEL NTR1                                                                   
         XC    DUB,DUB                                                          
         MVI   DUB,X'68'                                                        
         MVI   DUB+1,6                                                          
         MVC   DUB+2(4),QSTA                                                    
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *-10                                                             
         GOTO1 VRECUP,DMCB,BUYREC,DUB,(R6)                                      
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* NSET COMMAND  -- OVERRIDE NETWORK DEFINITION PERCENTAGES    *                 
*=============================================================*                 
         SPACE 1                                                                
NSET     LA    R2,BUYINP1H         POINT TO FIRST/ONLY INPUT LINE               
*                                                                               
         LA    R7,SVNDEF           POINT TO NETWORK DEFINITION                  
         USING SVNDEFD,R7                                                       
*                                                                               
         MVI   ERRCD,NONETDIS                                                   
         CLI   SVRCLOPT,RCLNET     TEST LAST RECALL ACTION NDIS                 
         BNE   BUYERR                                                           
*                                                                               
NSET2    ZIC   R0,0(R2)            POINT TO NEXT UNP FIELD                      
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST E-O-S                                   
         BNH   NSET10                                                           
         TM    1(R2),X'20'         SKIP PROT FIELD                              
         BO    NSET2                                                            
* EDIT NEW PERCENTAGES                                                          
         MVI   ERRCD,BADPCTG                                                    
         CLI   5(R2),2                                                          
         BNE   NSET4                                                            
         CLC   =C'NB',8(R2)        TEST NOT BOUGHT                              
         BNE   NSET4                                                            
         MVC   SVNDPCT,=F'-1'                                                   
         B     NSET6                                                            
*                                                                               
NSET4    ZIC   R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(3,8(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         MVC   SVNDPCT,4(R1)                                                    
*                                                                               
NSET6    LA    R7,L'SVNDEF(R7)     NEXT TABLE ENTRY                             
         B     NSET2                                                            
         EJECT                                                                  
*=================================================================*             
* ALL STATIONS EDITED - NOW DISTRIBUTE PERCENTAGES OVER STATIONS  *             
* SO THAT SUM OF ACTUAL PERCENTAGES COMES TO 100                  *             
*=================================================================*             
                                                                                
NSET10   LA    R7,SVNDEF                                                        
         SR    R8,R8                                                            
*                                                                               
NSET12   ICM   R0,15,SVNDPCT                                                    
         C     R0,=F'-1'           TEST NOT BOUGHT                              
         BE    *+6                 YES  - SKIP                                  
         AR    R8,R0                                                            
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   NSET12                                                           
*                                                                               
         ST    R8,SVNPCTG                                                       
*                                                                               
*============================================================*                  
*                                                                               
         LA    R2,BUYINP1H         FIND 'TOT=' AND SHOW CURRENT TOTAL           
NSET13   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BZ    NSET13                                                           
         CLC   =C'TOT=',8(R2)                                                   
         BNE   NSET13                                                           
*                                                                               
         LA    R4,12(R2)                                                        
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
         EDIT  (R8),(7,(R4)),3,ALIGN=LEFT                                       
         OI    6(R2),X'80'         SET XMT FLAG                                 
*                                                                               
         C     R8,=F'100000'                                                    
         BE    NSET14                                                           
*                                                                               
         MVI   ERRCD,NOSTATNS                                                   
         LTR   R8,R8                                                            
         BZ    BUYERR                                                           
*                                                                               
NSET14   LA    R7,SVNDEF                                                        
*                                                                               
NSET16   ICM   R1,15,SVNDPCT                                                    
         C     R1,=F'-1'           TEST NOT BOUGHT                              
         BE    NSET18                                                           
         M     R0,=F'200000'                                                    
         DR    R0,R8                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         STCM  R1,15,SVNDPCT                                                    
*                                                                               
NSET18   LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   NSET16                                                           
*                                                                               
         LA    R7,SVNDEF           CHECK NOW SUMS TO 100                        
         SR    R8,R8                                                            
*                                                                               
NSET20   CLC   SVNDPCT,=F'-1'      TEST NOT BOUGHT                              
         BNE   NSET22              NO - FOUND ONE TO USE                        
         LA    R7,L'SVNDEF(R7)                                                  
         B     NSET20                                                           
*                                                                               
NSET22   LR    RE,R7               SAVE A(FIRST)                                
*                                                                               
NSET24   ICM   R0,15,SVNDPCT                                                    
         C     R0,=F'-1'           TEST NOT BOUGHT                              
         BE    NSET26                                                           
         C     R0,SVNDPCT-SVNDEFD(RE)  COMPARE TO LARGEST                       
         BL    *+6                                                              
         LR    RE,R7                   SAVE ADDRESS OF THIS ONE                 
         AR    R8,R0                                                            
*                                                                               
NSET26   LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   NSET24                                                           
*                                                                               
         S     R8,=F'100000'                                                    
         ICM   R0,15,SVNDPCT-SVNDEFD(RE)  ADJUST LARGEST PCTG                   
         SR    R0,R8                                                            
         STCM  R0,15,SVNDPCT-SVNDEFD(RE)                                        
         B     CSETX                                                            
         EJECT                                                                  
*=======================================================*                       
* EDIT NETWORK CUT-IN LIST                              *                       
*=======================================================*                       
         SPACE 1                                                                
CSET     DS    0H                                                               
         LA    R2,BUYINP1H         POINT TO FIRST/ONLY INPUT LINE               
*                                                                               
         LA    R7,SVNDEF           POINT TO NETWORK DEFINITION                  
         USING SVNDEFD,R7                                                       
*                                                                               
         MVI   ERRCD,NOCLSTDS                                                   
         CLI   SVRCLOPT,RCLCLST    TEST LAST RECALL ACTION CDIS                 
         BNE   BUYERR                                                           
*                                                                               
CSET2    DS    0H                                                               
         ZIC   R0,0(R2)            POINT TO NEXT UNP FIELD                      
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST E-O-S                                   
         BNH   CSETX                                                            
         TM    1(R2),X'20'         SKIP PROT FIELD                              
         BO    CSET2                                                            
*                                                                               
         NI    SVNDSTAT,X'FF'-SVNDCUTQ    RESET FLAG                            
         CLI   5(R2),0                                                          
         BE    CSET4                                                            
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),1                    ANY 1 CHAR WILL DO                    
         BH    BUYERR                                                           
         OI    SVNDSTAT,SVNDCUTQ   SET FLAG                                     
*                                                                               
CSET4    LA    R7,L'SVNDEF(R7)     NEXT TABLE ENTRY                             
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   CSET2                                                            
*                                                                               
CSETX    DS   0H                                                                
         L     R0,ASVNOLD                                                       
         LA    R1,SVNDEFX-SVNDEF                                                
         LA    RE,SVNDEF                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE CURRENT NETWORK DEF                     
         MVI   TWA1FLAG,C'Y'                                                    
         MVC   BUYMSG(16),=C'ACTION COMPLETED'                                  
         B     EXIT                                                             
         SPACE 2                                                                
CCLR     LA    R7,SVNDEF                                                        
*                                                                               
CCLR2    NI    SVNDSTAT,X'FF'-SVNDCUTQ                                          
         LA    R7,L'SVNDEF(R7)                                                  
         OC    0(5,R7),0(R7)                                                    
         BNZ   CCLR2                                                            
         MVI   RCLOPT,RCLCLST                                                   
         GOTO1 CALLDSP                                                          
         B     CSETX                                                            
         EJECT                                                                  
         DROP  R7                                                               
*===========================================================                    
*        NXPLOD=START,END                                                       
*===========================================================                    
                                                                                
B900     XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
         LA    R4,8(R2)            ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL              GET PAST NXPLOD=                             
*                                                                               
         MVC   BUYLOW,=H'1'                                                     
         MVC   BUYHI,=Y(MAXBUYS)                                                
         CLI   SV1OR2,2                                                         
         BE    *+10                                                             
         MVC   BUYHI,=H'255'                                                    
*                                                                               
         CLI   FSTOP,C','                                                       
         BE    *+12                                                             
         CLI   FSTOP,C'='                                                       
         BNE   B904                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NXPLERR)                                               
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         STH   R0,BUYLOW           SET START LINE NUMBER                        
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         STH   R0,BUYHI                                                         
         CHI   R0,999                                                           
         BH    BUYERR                                                           
         CLI   SV1OR2,2                                                         
         BE    *+12                                                             
         CHI   R0,255                                                           
         BH    BUYERR                                                           
         CLC   BUYLOW,BUYHI                                                     
         BH    BUYERR                                                           
*                                                                               
         USING VTIAD,R5                                                         
B904     L     R5,VTIA               CLEAR STATION, BUYLINE & STATION           
         LA    RE,16                 DEFINITION TABLES                          
B905     XC    0(178,R5),0(R5)                                                  
         LA    R5,178(R5)                                                       
         BCT   RE,B905                                                          
*                                                                               
         L     R5,VTIA               INITIALIZE END OF STATION AND              
         MVI   STATAB,X'FF'          BUYLINE TABLES                             
         MVI   BUYLNTAB,X'FF'                                                   
         MVI   ELEM,0                INITIALIZE BUYLINE COUNTER                 
*                                                                               
         LA    RE,SVNDEF                                                        
         LA    RF,DEFTAB                                                        
B906     OC    0(5,RE),0(RE)         BUILD DEFTAB WITH ALL STATIONS             
         BZ    B907                  FROM SVNDEF                                
         MVC   0(5,RF),0(RE)                                                    
         LA    RE,26(RE)                                                        
         LA    RF,6(RF)                                                         
         B     B906                                                             
B907     MVI   0(RF),X'FF'                                                      
*                                                                               
**********************************************************************          
*                                                                               
         LA    R7,BUYLNTAB                                                      
         XC    KEY,KEY               READ ALL BUYLINES FOR NETWORK              
         MVC   KEY(10),SVKEY         (UNTIL CHANGE OF ESTIMATE) FOR             
         MVC   KEY+11(2),BUYLOW      SET STARTING LINE NUMBER                   
         GOTO1 HIGH                  SELECTED REGION INTO AREC5                 
         B     B915                                                             
*                                                                               
B910     LA    RE,DEFTAB             CLEAR OUT DEFTAB'S X'S FOR EACH            
*                                                                               
B911     CLI   0(RE),X'FF'           NEW NETWORK BUYLINE                        
         BE    B914                                                             
         MVI   5(RE),C' '                                                       
         LA    RE,6(RE)                                                         
         B     B911                                                             
*                                                                               
B914     GOTO1 SEQ                                                              
*                                                                               
B915     CLC   KEY(10),KEYSAVE                                                  
         BNE   B991                                                             
         CLC   KEY+11(2),BUYHI                                                  
         BH    B991                                                             
         MVC   AREC,AREC5                                                       
         GOTO1 GETREC                                                           
*                                                                               
         USING BUYREC,R6             BUYLINE MUST BE IN SAME REGION             
         L     R6,AREC5                                                         
         CLC   BDNRGN,SVNRGN                                                    
         BNE   B910                                                             
         DROP  R6                                                               
*                                                                               
         LA    R6,BDELEM-BUYREC(R6)                                             
         MVI   ELCDLO,X'0B'          BUYLINE MUST BE UNPAID                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
B920     BAS   RE,NEXTEL                                                        
         BNE   B925                                                             
         USING REGELEM,R6                                                       
         OC    RPAY,RPAY                                                        
         BNZ   B910                                                             
         TM    RSTATUS,X'20'       TEST COST OVRD                               
         BZ    B920                NO                                           
         OC    RPCOST,RPCOST       TEST ZERO-DOLLARS                            
         BNZ   B910                                                             
         B     B920                                                             
         DROP  R6                    SET 'BUYTAB MUST BE UPDATED' FLAG          
*                                                                               
B925     MVI   BYTE,C'N'             TO NO                                      
*                                                                               
**********************************************************************          
*                                                                               
         L     R6,AREC5                                                         
         LA    R6,BDELEM-BUYREC(R6)  READ NETWORK BUYLINE'S 68 ELEMENTS         
         MVI   ELCDLO,X'68'          ...                                        
         MVI   ELCDHI,X'68'                                                     
B930     BAS   RE,NEXTEL                                                        
         BNE   B970                                                             
*                                                                               
         LA    RE,DEFTAB                                                        
B931     CLI   0(RE),X'FF'           X ALL THE 68 ELEMENT'S AFFILIATES          
         BE    B910                  IN DEFTAB                                  
         CLC   0(5,RE),2(R6)                                                    
         BE    B932                                                             
         LA    RE,6(RE)             IF MARKET-STATION FROM 68 ELEMENT           
         B     B931                 NOT FOUND IN DEFTAB ... IGNORE THIS         
B932     MVI   5(RE),C'X'           BUYLINE                                     
*                                                                               
         LA    RE,SVNDEF                                                        
         USING SVNDEFD,RE           MATCH UP MARKET-STATION FROM 68             
B935     CLC   SVNDMKST,2(R6)       ELEMENT TO SVNDEF ...                       
         BE    B938                                                             
         LA    RE,26(RE)            IF MARKET-STATION NOT FOUND IN              
         CLC   SVNDMKST,=5X'00'     SVNDEF ... ERROR MADE BUILDING              
         BNE   B935                 DEFTAB                                      
         DC    H'00'                                                            
*                                                                               
B938     MVI   BYTE,C'Y'            ELEMENT MATCH ... GET NEXT 68 ELEM          
         LA    R4,STATAB                                                        
*                                                                               
B940     CLI   0(R4),X'FF'                                                      
         BE    B950                                                             
         CLC   0(5,R4),2(R6)                                                    
         BE    B960                 IF PERCENTAGES FROM SVNDEF AND 68           
         LA    R4,L'STATAB(R4)      ELEMENT DON'T MATCH, SET 'BUYTAB            
         B     B940                 MUST BE UPDATED' FLAG TO YES ...            
*                                                                               
B950     MVC   0(5,R4),2(R6)        IF STATION NOT ALREADY IN STATAB            
         MVC   5(4,R4),7(R6)        ... STORE MKT/STA AND OLD COST%             
         MVI   L'STATAB(R4),X'FF'   (FROM 68 ELEMENT) INTO STATAB ...           
         IC    R0,ELEM              MARK END OF STATION TABLE AND BUMP          
         AHI   R0,1                 STATION COUNTER                             
         STC   R0,ELEM                                                          
         B     B930                                                             
*                                                                               
B960     CLC   5(4,R4),7(R6)        IF STATION ALREADY IN STATAB ...            
         BE    B930                 IF PERCENTAGE MATCHES GET NEXT 68           
         B     B910                 ELEM ... IF NOT, SKIP TO NEXT               
*                                   NETWORK BUYLINE                             
*                                                                               
***********************************************************************         
*                                                                               
*                                   WHEN NO MORE 68 ELEMENTS IN NETWORK         
*                                   BUYLINE RECORD ...                          
*                                                                               
*                                   IF ALL PERCENTAGES FROM 68 ELEMENTS         
B970     CLI   BYTE,C'N'            MATCH PERCENTAGES FROM SVNDEF ...           
         BE    B910                 DO NOT ADD BUYLINE TO TABLE ... GET         
*                                   NEXT BUYLINE                                
         LA    RE,DEFTAB                                                        
B971     CLI   0(RE),X'FF'          IF ANY AFFILIATES FROM SVNDEF ARE           
         BE    B972                 NOT PRESENT IN NETWORK BUYLINE ...          
         CLI   5(RE),C'X'           DO NOT ADD BUYLINE TO TABLE ... GET         
         BNE   B910                 NEXT BUYLINE                                
         LA    RE,6(RE)                                                         
         B     B971                                                             
*                                                                               
B972     MVC   0(2,R7),KEY+11       IF ANY PERCENTAGES FROM 68 ELEMENTS         
         USING BUYREC,R6            DO NOT MATCH PERCENTAGES FROM               
         L     R6,AREC5             SVNDEF ...                                  
         MVC   2(3,R7),BDCOST       PUT BUYLINE AND OLD SPOT COST INTO          
         LA    R7,L'BUYLNTAB(R7)    BUYTABLE ...                                
         MVI   0(R7),X'FF'                                                      
*                                                                               
         LA    R6,BDELEM-BUYREC(R6)                                             
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'         CHANGE THE PERCENTAGE IN ALL OF THE         
B980     BAS   RE,NEXTEL            NETWORK BUYLINE'S 68 ELEMENTS TO            
         BNE   B990                 THE NEW VALUE                               
         LA    RE,SVNDEF                                                        
B981     CLC   SVNDMKST,2(R6)                                                   
         BE    B985                                                             
         LA    RE,26(RE)                                                        
         CLC   SVNDMKST,=5X'00'     IF MARKET-STATION NOT FOUND IN              
         BNE   B981                 SVNDEF ... SKIP THIS 68 ELEMENT             
         B     B980                                                             
B985     MVC   7(4,R6),SVNDPCT                                                  
         B     B980                                                             
B990     GOTO1 PUTREC               PUT NETWORK BUYLINE                         
         B     B910                                                             
         DROP  RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
B991     CLI   BUYLNTAB,X'FF'       IF NO BUYLINES TO UPDATE ... EXIT           
         BNE   B991A                                                            
         MVC   BUYMSG(21),=C'NO BUYLINES TO UPDATE'                             
         B     EXIT                                                             
*                                                                               
B991A    CLI   STATAB,X'FF'         IF NO STATIONS TO UPDATE ... EXIT           
         BNE   B991B                                                            
         MVC   BUYMSG(19),=C'NO STATIONS CHANGED'                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
B991B    ZIC   R0,ELEM              SORT STATAB BY ASCENDING OLD COST %         
         L     RF,VCOMFACS                                                      
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(0,STATAB),(R0),L'STATAB,4,5                           
*                                                                               
***********************************************************************         
*                                                                               
         LA    R4,STATAB                                                        
B992     CLI   0(R4),X'FF'                                                      
         BE    B993                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+4(5),0(R4)                                                   
         MVC   KEY+11(2),BUYLOW                                                 
*                                                                               
B992A    GOTO1 HIGH                 FIND THE LAST BUYLINE FROM BUYTAB           
         B     B992C                ASSOCIATED WITH EACH AFFILIATE ...          
*                                                                               
B992B    GOTO1 SEQ                  SAVE THAT LAST BUYLINE FOR EACH             
*                                                                               
B992C    CLC   KEY(10),KEYSAVE      AFFILIATE INTO BUYTAB                       
         BNE   B992D                                                            
         CLC   KEY+11(2),BUYHI                                                  
         BNH   B992E                                                            
*                                                                               
B992D    LA    R4,L'STATAB(R4)     NEXT AFFILIATE                               
         B     B992                                                             
*                                                                               
B992E    LA    R7,BUYLNTAB                                                      
         USING BUYLNTABD,R7                                                     
*                                                                               
B992F    CLI   0(R7),X'FF'                                                      
         BE    B992B                                                            
         CLC   BUYLNTAB_LIN,KEY+11                                              
         BE    B992G                                                            
         LA    R7,L'BUYLNTAB(R7)                                                
         B     B992F                                                            
*                                                                               
B992G    MVC   9(2,R4),KEY+11                                                   
         B     B992B                                                            
*                                                                               
***********************************************************************         
*                                                                               
B993     LA    R4,STATAB                                                        
         USING STATABD,R4                                                       
*                                                                               
B994     LA    RE,SVNDEF            FOR EACH AFFILIATE, MOVE THE NEW            
         USING SVNDEFD,RE           COST % (FROM SVNDEF) INTO ELEM              
B994A    CLC   SVNDMKST,STATAB_MS                                               
         BE    B994B                                                            
         LA    RE,L'SVNDEF(RE)                                                  
         B     B994A                                                            
B994B    MVC   ELEM(4),SVNDPCT                                                  
         DROP  RE                                                               
*                                                                               
         LA    R7,BUYLNTAB          READ INTO AREC5 ALL THE BUYLINES            
         USING BUYLNTABD,R7                                                     
*                                                                               
B995     XC    KEY,KEY              FOR EACH AFFILIATE                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+4(5),STATAB_MS                                               
         MVC   KEY+11(2),BUYLNTAB_LIN                                           
         GOTO1 HIGH                                                             
         B     B997                                                             
*                                                                               
B996     GOTO1 SEQ                                                              
*                                                                               
B997     CLC   KEY(10),KEYSAVE                                                  
         BNE   B1010                                                            
         CLC   KEY+11(2),BUYLNTAB_LIN                                           
         BNE   B996                                                             
         GOTO1 GETREC                                                           
*                                                                               
         USING BUYREC,R6                                                        
         L     R6,AREC5                                                         
         SR    R1,R1                                                            
         ICM   R1,7,BUYLNTAB_DOL    R1 <--- COST FOR ENTIRE BUYLINE             
         AR    R1,R1                R1 <--- R1 X 2 (FOR ROUNDING)               
         M     R0,ELEM              R1 <--- NEW COST% * COST ENT BUYL           
         D     R0,=F'100000'        R1 <--- NEW COST                            
         AHI   R1,1                 TAKES CARE OF ROUNDING NUMBERS              
         SRL   R1,1                                                             
         LR    R0,R1                R0 <--- NEW COST                            
*                                                                               
         CLI   L'STATAB(R4),X'FF'   CHECK LAST STATION FOR BUYLINE              
         BE    B999B                YES - DON'T ADD TO TOTAL                    
         MVC   FULL,BUYLNTAB_PCT                                                
         A     R1,FULL                                                          
*                                                                               
         CLM   R1,7,BUYLNTAB_DOL    WE HAVE MORE THAN ORIGINAL COST??           
         BNH   B999A                NO, WE'RE OKAY                              
         SR    R1,R1                ADJUST COST FOR 2ND TO LAST STATION         
         ICM   R1,7,BUYLNTAB_DOL    GET TOTAL COST FOR BUYLINE                  
         S     R1,FULL              LESS COST USED SO FAR                       
         LR    R0,R1                                                            
         A     R1,FULL              NOW 5(R7) SHOULD MATCH 1(R7)                
*                                                                               
B999A    STCM  R1,15,BUYLNTAB_PCT   ACCUMULATE TOTAL COST USED                  
*                                                                               
B999B    CLI   L'STATAB(R4),X'FF'   TEST LAST STATION THIS BUYLINE              
         BNE   B1000                NO                                          
         SR    R0,R0                                                            
         ICM   R0,7,BUYLNTAB_DOL    GET TOTAL COST FOR BUYLINE                  
         MVC   FULL,BUYLNTAB_PCT                                                
         S     R0,FULL              LESS COST USED SO FAR                       
*                                                                               
B1000    STCM  R0,7,BDCOST          MOVE NEW COST INTO AFFILIATE RECORD         
         GOTO1 PUTREC               AND PUT IT                                  
*                                                                               
***********************************************************************         
*                                                                               
B1010    LA    R7,L'BUYLNTAB(R7)    IF NOT AT END OF BUYLINES, READ             
         CLI   0(R7),X'FF'          NEXT BUYLINE FOR CURRENT AFFILIATE          
         BNE   B996                                                             
*                                                                               
*                                   IF AT END OF BUYLINES, BUMP TO NEXT         
         LA    R4,L'STATAB(R4)      AFFILIATE                                   
         CLI   0(R4),X'FF'          WHEN NO MORE AFFILIATES ... DONE            
         BNE   B994                                                             
         MVC   BUYMSG(16),=C'ACTION COMPLETED'                                  
         B     EXIT                                                             
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
*======================================================                         
* BUILD PROGRAM NAME                                                            
*======================================================                         
                                                                                
BLDPRG   NTR1                                                                   
         MVC   BDPROGRM(4),BUORB   MOVE PGMDEF KEY TO PROGRAM                   
         MVI   BDPROGRM+4,C'-'                                                  
         MVC   BDPROGRM+5(12),BUPROG                                            
         CLI   BUPROG,C'='         TEST LIVE SHOW                               
         BNE   BLDPRG2                                                          
         CLC   BUPROG(4),BUORB     TEST INPUT AND SHOW REC THE SAME             
         BNE   BLDPRG2             NO                                           
         MVC   BDPROGRM(17),BUPROG YES - DON'T NEED CODE TWICE !                
BLDPRG2  DS    0H                                                               
         CLI   BUPROG+17,0         TEST '-S'                                    
         BNE   BLDPRGX                                                          
         MVI   BDPROGRM+17,0       SET -S IND                                   
         LA    RE,BDPROGRM+16                                                   
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         BCTR  RE,0                                                             
         CLC   0(2,RE),=C'-S'                                                   
         BE    BLDPRGX                                                          
         LA    RE,2(RE)                                                         
         LA    R0,BDPROGRM+15                                                   
         CR    RE,R0                                                            
         BNH   *+6                                                              
         LR    RE,R0                                                            
         MVC   0(2,RE),=C'-S'                                                   
BLDPRGX  XIT1                                                                   
         EJECT                                                                  
*========================================================*                      
* BUILD ID  ELEMENT                                      *                      
*========================================================*                      
         SPACE 1                                                                
BLDID    NTR1                                                                   
         OC    SVID,SVID           TEST ID PRESENT                              
         BNZ   BLDID10                                                          
         MVI   ERRCD,NOIDERR                                                    
         CLI   SVAPROF+9,C'Y'      TEST AGY REQUIRES ID                         
         BE    BLDIDERR            YES                                          
         CLI   SVCXTRA+2,C'Y'      TEST CLT REQUIRES ID                         
         BE    BLDIDERR            YES                                          
         B     EXIT                NOT REQD AND NOT PRESENT !                   
*                                                                               
BLDIDERR GOTO1 ERROR                                                            
*                                                                               
BLDID10  MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15                                                        
         MVI   ELEM+2,0                                                         
         MVC   ELEM+3(12),SVID                                                  
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    EXIT                IF ID ELEM PRESENT, USE IT                   
* ADD ELEMENT AT END OF RECORD                                                  
         LA    R6,BDELEM           FIND E-O-R                                   
         SR    R0,R0                                                            
BLDID20  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   BLDID20                                                          
BLDID22  DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* BUILD PST ELEMENT AND GST ELEMENT                      *                      
*       R7 - MKT/STA                                     *                      
*========================================================*                      
         SPACE 1                                                                
BLDPST   NTR1                                                                   
*                                                                               
         MVI   ELEM,X'6B'                                                       
         MVI   ELEM+1,12                                                        
         MVI   ELCDLO,X'6B'                                                     
         MVI   ELCDHI,X'6B'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   BLDPST10                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  DELETE ELEM IF PRESENT                  
*                                                                               
BLDPST10 CLI   SVNETBTS,X'01'      TEST CBLDEF                                  
         BE    BLDPST50                                                         
*                                                                               
         LA    R7,SVNDEF           POINT TO NETWORK DEFINITION                  
         USING SVNDEFD,R7                                                       
*                                                                               
BLDPST20 OC    0(L'SVNDEF,R7),0(R7)                                             
         BZ    BLDPSTX                                                          
         CLC   SVNDMKST,BUYREC+4   FIND MATCHING MKT/STA                        
         BE    BLDPST30                                                         
         LA    R7,L'SVNDEF(R7)                                                  
         B     BLDPST20                                                         
*                                                                               
BLDPST30 XC    BDNTAX,BDNTAX          HST IMPLIES NO SALES TAX                  
         MVI   ELEM,X'6B'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),SVNDPST                                               
*                                                                               
         CLC   BDSTART,=X'6E061B'  TEST BUY STARTS BEFORE JUL/10                
         BH    BLDPST38                                                         
         LA    R0,10                                                            
         LA    R1,ELEM+2                                                        
*                                                                               
BLDPST32 CLI   0(R1),C'H'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'X'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,BLDPST32                                                      
                                                                                
* NETWORK VALUE WILL NOW OVERRIDE STATION IF THERE IS A PST CODE THERE          
                                                                                
         LA    R1,SVPST            POINT TO NETWORK PST VALUES                  
         LA    R0,10                                                            
         LA    RE,ELEM+2                                                        
*                                                                               
BLDPST34 CLI   0(R1),C' '          TEST NETWORK HAS PST CODE                    
         BNH   BLDPST36            NO - IGNORE                                  
         CLI   0(RE),C' '          TEST STATION HAS A PST CODE                  
         BNH   BLDPST36            NO - IGNORE                                  
         MVC   0(1,RE),0(R1)       ELSE SET NTWK VALUE FOR STATION              
*                                                                               
BLDPST36 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BLDPST34                                                      
                                                                                
*                                                                               
BLDPST38 CLC   BDSTART,=X'6E061C'  TEST BUY STARTS BEFORE JUL/10                
         BL    *+10                YES                                          
         MVC   ELEM+2(10),SVPST    JUL/10+ GETS NETWORK VALUES                  
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
BLDPST40 CLI   SVNDGST,C'S'           TEST GST ON THIS STATION                  
         BE    BLDPSTX                                                          
* NEED GST ELEMENT                                                              
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'6A'                                                       
         MVI   ELEM+1,3                                                         
         MVC   ELEM+2(1),SVNDGST                                                
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         B     BLDPSTX                                                          
         SPACE 1                                                                
*====================================================================           
* FOR CANADIAN CBLDEF, EVERY STATION GETS NETWORK PST VALUES                    
*====================================================================           
         SPACE 1                                                                
BLDPST50 OC    SVPST,SVPST         TEST ANY PST AT NTWK LEVEL                   
         BZ    BLDPST60                                                         
         XC    BDNTAX,BDNTAX          HST IMPLIES NO SALES TAX                  
         MVI   ELEM,X'6B'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),SVPST                                                 
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
BLDPST60 CLI   SVGSTCOD,C'S'       TEST ANY SPECIAL GST ON NTWK                 
         BE    BLDPSTX                                                          
* NEED GST ELEMENT                                                              
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'6A'                                                       
         MVI   ELEM+1,3                                                         
         MVC   ELEM+2(1),SVGSTCOD                                               
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
BLDPSTX  B     EXIT                                                             
         EJECT                                                                  
DELELS   NTR1                                                                   
         LA    R6,BDELEM                                                        
*                                                                               
DELELS2  SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
DELELS4  CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'0B'                                                      
         BL    DELELS6                                                          
         CLI   0(R6),X'0D'                                                      
         BNH   DELELS2                                                          
*                                                                               
DELELS6  GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     DELELS4                                                          
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
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
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPGENSDEF                                                                     
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
* SPGENDOV                                                                      
       ++INCLUDE SPGENDOV                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
SPBUYWKD DSECT                                                                  
         ORG   WORK2                                                            
TBYTE    DS    X                                                                
         ORG                                                                    
 END                                                                            

*          DATA SET SPREPK302  AT LEVEL 087 AS OF 05/01/02                      
*PHASE SPK302A                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'SPK302 - UNGOAL REPORT'                                         
SPK302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPK302                                                         
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         STM   R9,RB,REGSTR                                                     
         LA    R1,HEADHK                                                        
         ST    R1,HEADHOOK                                                      
         SPACE 3                                                                
***********************************                                             
*                                 *                                             
* THIS PROGRAM READS GOAL RECORDS *                                             
*    AND DELETES THE '21' ELEMS   *                                             
*     WHICH FALL BETWEEN THE      *                                             
*        REQUESTED DATES          *                                             
*                                 *                                             
*      QOPT1   = DAYPART          *                                             
*      QOPT2-4 = SPTLEN           *                                             
*      QOPT5   = N (DOES NOT      *                                             
*                  MARK FILE)     *                                             
*                                 *                                             
***********************************                                             
         EJECT                                                                  
*                                                                               
MODE00   DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BE    M10                                                              
         CLI   MODE,MGR1FRST                                                    
         BE    M20                                                              
         CLI   MODE,PRDLAST                                                     
         BE    M30                                                              
         CLI   MODE,MKTFRST                                                     
         BE    M40                                                              
         CLI   MODE,MKTLAST                                                     
         BE    M50                                                              
         CLI   MODE,PROCGOAL                                                    
         BE    PROCGL                                                           
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   CR    RB,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
M10      DS    0H                 *ESFRST - SET QSTART TO PREV MON              
         SPACE                                                                  
         GOTO1 GETDAY,DMCB,QSTART,WORK                                          
         ZIC   R0,0(R1)                     NUM OF DAY OF WEEK TO R0            
         LA    R3,1                                                             
         SR    R3,R0                    NEG NUM TO ADD TO QSTART IN R3          
         GOTO1 ADDAY,DMCB,QSTART,WORK,(R3)             WORK = PREV MON          
         SPACE                                                                  
M11      GOTO1 DATCON,DMCB,(0,WORK),(2,BQSTARTP)                                
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
         SPACE                                                                  
         CLC   =C'POL',QPRD                       SET PRODUCT                   
         BNE   M12                                                              
         MVC   P(80),QAREA                                                      
         MVC   P+82(32),=C'** PRD=POL IS INVALID REQUEST **'                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE                                                                  
M12      CLC   =CL3' ',QOPT2       SPOTLEN(QOPT2,3,4)                           
         BE    M16                                                              
         LA    R5,3                                                             
         LA    R6,QOPT2                                                         
M14      CLI   0(R6),C'0'                                                       
         BL    SPERR                                                            
         CLI   0(R6),C'9'                                                       
         BH    SPERR                                                            
         LA    R6,1(R6)                                                         
         BCT   R5,M14                                                           
         PACK  DUB,QOPT2(3)                                                     
         CVB   R0,DUB                                                           
         STC   R0,BSLN                                                          
         SPACE                                                                  
M16      CLC   =C'ALL',QMKT        MKT=ALL                                      
         BNE   M18                                                              
         MVI   MKTOTSW,C'Y'                                                     
M18      MVI   FORCEHED,C'Y'                                                    
         CLC   =CL3'NO',QEST       DOES EST=NO                                  
         BE    M19                                                              
         CLI   QESTEND,C' '        DOES EST=A RANGE                             
         BE    M19A                NO.                                          
M19      MVI   ESTOTSW,C'Y'                                                     
         SPACE                                                                  
M19A     XC    TOTALS,TOTALS       CLEAR TOTALS                                 
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
M20      DS    0H                 *MGR1FRST                                     
         CLI   QMGR,C' '                                                        
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   MKTOTSW,C'Y'                                                     
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
M30      DS    0H                 *PRDLAST                                      
         CP    RECCNT,=P'0'                                                     
         BE    EXIT                                                             
         BAS   RE,FNLTOTS                                                       
         B     EXIT                                                             
*                                                                               
M40      DS    0H                 *MKTFRST                                      
*                                                                               
         BAS   RE,REQPARSE                                                      
*                                                                               
         MVI   FRSTSW,C'Y'         SET FIRST TIME SWITCH                        
         ZAP   RECCNT,=P'0'                                                     
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
M50      DS    0H                 *MKTLAST                                      
         CLI   ESTOTSW,C'Y'        ARE THERE ESTOTS                             
         BNE   M59A                                                             
         MVI   BESTSV,0            YES. FORCE EST UNEQUAL                       
         BAS   RE,ESTOTS                                                        
M59A     BAS   RE,MKTOTS                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PROCGOAL                                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCGL   DS    0H                  *PROCGL                                      
*                                                                               
         BRAS  RE,FILTGOAL                                                      
         BNE   EQXIT                                                            
*                                                                               
         L     R7,ADGOAL                                                        
         USING GKEY,R7                                                          
         MVC   GKEYSAV,0(R7)                                                    
*                                                                               
PROC20   LA    R6,24(R7)           GET '21' ELEMENT                             
         USING GLEMENT,R6                                                       
         MVI   ELCODE,X'21'                                                     
         CLI   0(R6),X'21'                                                      
         BE    PROC25                                                           
         BAS   RE,NXTEL                                                         
         BNE   EXIT                                                             
*                                                                               
PROC25   MVI   DELSW,C'N'          SET TO PROCESS ELEMS                         
         CLI   FRSTSW,C'Y'         FIRST TIME                                   
         BNE   *+10                                                             
         MVC   BESTSV,GKEYEST                                                   
         MVI   FRSTSW,C'N'                                                      
         BAS   RE,ESTOTS           EST TOT BREAK                                
*                                                                               
PROC30   CLC   GLWEEK,BQSTARTP     CHECK DATE SPREAD                            
         BL    PROC50                                                           
         CLC   GLWEEK,BQENDP                                                    
         BH    PROC50                                                           
         CLI   DELSW,C'Y'                                                       
         BE    PROC35                                                           
         MVC   STDTSV,GLWEEK       SAVE BEG/END DATES                           
PROC35   MVC   ENDTSV,GLWEEK                                                    
         SPACE                                                                  
PROC40   L     R0,DOLTOT           ADD UP DOLLAR/POINT TOTS                     
         ICM   R1,15,GLBUDGET                                                   
         AR    R0,R1                                                            
         ST    R0,DOLTOT                                                        
         L     R0,PTOT                                                          
         ICM   R1,15,GLGRP                                                      
         AR    R0,R1                                                            
         ST    R0,PTOT                                                          
         SPACE                                                                  
***                                                                             
         GOTO1 RECUP,DMCB,(0,(R7)),(R6),0         DELETE ELEMENT                
         MVI   DELSW,C'Y'                                                       
         BAS   RE,NXT10                                                         
         B     *+8                                                              
         SPACE                                                                  
PROC50   BAS   RE,NXTEL                                                         
         BE    PROC30                                                           
         CLI   DELSW,C'Y'          END OF ELEMS, ANY DELETED                    
         BNE   EXIT                NO. EXIT                                     
         SPACE                                                                  
         CLI   QOPT5,C'N'          WRITE='NO' OPTION                            
         BNE   PROC50A                                                          
         MVI   RCSUBPRG,2          NO-DOES NOT MARK FILE                        
         MVI   RCWRITE,C'N'                                                     
         B     PROC50B                                                          
PROC50A  MVC   AREC,ADGOAL                                                      
         GOTO1 PUT                 YES.WRITE RECORD                             
PROC50B  DS    0H                                                               
         DROP  R6                                                               
         SPACE 2                                                                
         MVC   PMKT,MKT                                                         
         MVC   PMKNM,MKTNM                                                      
         EDIT  (1,GKEYEST),(3,PEST)                                             
         MVC   PDPT,GKEYDPT                                                     
         EDIT  (1,GKEYSLN),(3,PSLN),ZERO=BLANK                                  
         GOTO1 DATCON,DMCB,(2,STDTSV),(8,PSDTE)                                 
         GOTO1 DATCON,DMCB,(2,ENDTSV),(8,PEDTE)                                 
         EDIT  (4,DOLTOT),(12,PDOLS),2,ZERO=BLANK                               
         EDIT  (4,PTOT),(10,PPTS),1,ZERO=BLANK                                  
         SPACE                                                                  
         GOTO1 REPORT                                                           
         AP    RECCNT,=P'1'                                                     
         AP    ESTCNT,=P'1'                                                     
         SPACE                                                                  
         CLI   ESTOTSW,C'Y'                                                     
         BNE   PROC60                                                           
         L     R0,ESTDOLS          ADD UP EST TOTALS                            
         A     R0,DOLTOT                                                        
         ST    R0,ESTDOLS                                                       
         L     R0,ESTPTS                                                        
         A     R0,PTOT                                                          
         ST    R0,ESTPTS                                                        
         SPACE                                                                  
PROC60   DS    0H                                                               
         L     R0,MKTDOLS          ADD UP MKT TOTALS                            
         A     R0,DOLTOT                                                        
         ST    R0,MKTDOLS                                                       
         SPACE                                                                  
PROC70   XC    DOLTOT,DOLTOT                                                    
         XC    PTOT,PTOT                                                        
         SPACE                                                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * *                                                               
* MARKET TOTALS *                                                               
* * * * * * * * *                                                               
         DS    F                                                                
MKTOTS   ST    RE,*-4                                                           
         CP    RECCNT,=P'1'                                                     
         BNH   MKT10                                                            
         XC    P,P                                                              
         GOTO1 REPORT                                                           
         EDIT  (4,MKTDOLS),(12,PDOLS),2,ZERO=BLANK                              
         MVC   P+25(10),=C'** MARKET '                                          
         MVC   P+36(4),MKT                                                      
         MVC   P+41(8),=C'TOTAL **'                                             
         MVI   SPACING,X'03'                                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
MKT10    L     R0,MKTDOLF                                                       
         A     R0,MKTDOLS                                                       
         ST    R0,MKTDOLF                                                       
         XC    MKTDOLS,MKTDOLS                                                  
MKTX     L     RE,MKTOTS-4                                                      
         BR    RE                                                               
         SPACE 2                                                                
* EST TOTALS *                                                                  
         DS    F                                                                
ESTOTS   ST    RE,*-4                                                           
         CLC   BESTSV,GKEYEST      EST=EST PREV                                 
         BE    ESTX                                                             
         MVC   BESTSV,GKEYEST      NO.SET NEW EST NUM                           
         CP    ESTCNT,=P'0'                                                     
         BE    ESTX                                                             
         CP    ESTCNT,=P'1'                                                     
         BNH   ESTXA                                                            
         SPACE                                                                  
         XC    P,P                                                              
         GOTO1 REPORT                                                           
         MVI   SPACING,2                                                        
         EDIT  (4,ESTDOLS),(12,PDOLS),2,ZERO=BLANK                              
         EDIT  (4,ESTPTS),(10,PPTS),1,ZERO=BLANK                                
         MVC   P+25(20),=C'** ESTIMATE TOTAL **'                                
         GOTO1 REPORT                                                           
ESTXA    XC    ESTDOLS,ESTDOLS                                                  
         XC    ESTPTS,ESTPTS                                                    
         ZAP   ESTCNT,=P'0'                                                     
         XC    P,P                                                              
         GOTO1 REPORT             TO GET BLNK LINE BETWEEN DIFF ESTS            
ESTX     L     RE,ESTOTS-4                                                      
         BR    RE                                                               
         SPACE                                                                  
* PRODUCT TOTALS *                                                              
         DS    F                                                                
FNLTOTS  ST    RE,*-4                                                           
         CLI   MKTOTSW,C'Y'                                                     
         BNE   FNLX                                                             
         EDIT  (4,MKTDOLF),(12,PDOLS),2,ZERO=BLANK                              
         MVC   P+25(19),=C'** PRODUCT TOTAL **'                                 
         GOTO1 REPORT                                                           
         ZAP   RECCNT,=P'0'                                                     
         ZAP   ESTCNT,=P'0'                                                     
FNLX     L     RE,FNLTOTS-4                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
NXTEL    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    NXTELX                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DS    H'0'                                                             
         AR    R6,R0                                                            
NXT10    CLC   ELCODE,0(R6)                                                     
         BNE   NXTEL                                                            
         BR    RE                  EXIT WITH CC =                               
NXTELX   LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT =                           
         SPACE                                                                  
*                                                                               
SPERR    DS    0H                                                               
         MVC   P(80),QAREA                                                      
         MVC   P+82(23),=C'** SPTLEN NOT NUMERIC **'                            
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE                                                                  
*                                                                               
HEADHK   DS    0D                    TO DELETE MKT NAME PRINTED BY              
         USING *,RF                       MARKET GROUP ROUTINE                  
         NTR1  BASE=REGSTR+8                                                    
         DROP  RF                                                               
         LM    R9,RB,REGSTR                                                     
         SPACE                                                                  
         CLC   QOPT1(4),SPACES     TEST FILTERS                                 
         BE    HD02                                                             
         MVC   H6+69(4),QOPT1                                                   
         MVC   H6+74(12),=C'RECORDS ONLY'                                       
HD02     LA    R1,H8+30                                                         
         CLI   QMGR,C' '                                                        
         BE    HD07                                                             
         LA    R0,4                                                             
         LA    R1,H9+30                                                         
HD05     CLC   =C'MARKET',0(R1)                                                 
         BE    HD07                                                             
         SH    R1,=H'132'                                                       
         BCT   R0,HD05                                                          
         B     EXIT                                                             
HD07     MVC   0(37,R1),SPACES                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* - * - * - * - * - * - * - * - * - * - * - * - *                               
* REQPARSE- GET BINARY CODES FOR PRODUCTS QPRD AND QPRD2                        
* ANALYZE PRODUCTS IN REQ CARD AND SET REQFLAG MASKS                            
* - * - * - * - * - * - * - * - * - * - * - * - *                               
REQPARSE NTR1                                                                   
         MVI   PRD1BIN,0                                                        
         MVI   PRD2BIN,0                                                        
         MVI   REQFLAG,0                                                        
*                                                                               
         CLC   QPRD,=C'ALL'                                                     
         BNE   *+12                                                             
         OI    REQFLAG,REQP1ALQ       P1=ALL                                    
         B     REQP10                                                           
         CLC   QPRD,=C'   '                                                     
         BNE   *+12                                                             
         OI    REQFLAG,REQP1NOQ       P1 NOT GIVEN                              
         B     REQP10                                                           
         OI    REQFLAG,REQP1Q         P1 SPECIFICALLY REQUESTED                 
REQP10   DS    0H                                                               
*                                                                               
         CLC   QPRD2,=C'ALL'                                                    
         BNE   *+12                                                             
         OI    REQFLAG,REQP2ALQ       P2=ALL                                    
         B     REQP15                                                           
         CLC   QPRD2,=C'   '                                                    
         BNE   *+12                                                             
         OI    REQFLAG,REQP2NOQ       P2 NOT GIVEN                              
         B     REQP15                                                           
         OI    REQFLAG,REQP2Q         P2 SPECIFICALLY REQUESTED                 
*                                                                               
REQP15   DS    0H                                                               
         L     R7,ADGOAL                                                        
         USING GOALREC,R7                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),GKEYAM                                                  
         MVC   KEY+2(2),GKEYCLT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                NO SUCH CLIENT RECORD                        
         LA    R0,IO                                                            
         ST    R0,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         TM    REQFLAG,REQP1ALQ         IF P1 = ALL                             
         BO    REQP20                   DON'T GET BINARY VALUE                  
         LA    R2,QPRD                                                          
         LA    R3,PRD1BIN                                                       
         BAS   RE,GETPROD               GET P1'S BINARY VALUE                   
*                                                                               
REQP20   DS    0H                                                               
         TM    REQFLAG,REQP2NOQ         IF P2 IS BLANK                          
         BO    EQXIT                    EXIT                                    
         TM    REQFLAG,REQP2ALQ         IF P2 IS ALL                            
         BO    EQXIT                    EXIT                                    
         LA    R2,QPRD2                                                         
         LA    R3,PRD2BIN                                                       
         BAS   RE,GETPROD                                                       
*                                                                               
         B     EQXIT                                                            
         DROP  R7                                                               
*                                                                               
*                                                                               
*                                                                               
********************************************************************            
* GETPROD FINDS 1-BYTE EQUIVALENT OF 3-BYTE PRODUCT                             
* AREC IS EXPECTED TO HAVE ADDRESS OF CLIENT RECORD                             
* R2 IS EXPECTED TO ADDRESS 3-CHAR PRODUCT CODE                                 
* R3 IS EXPECTED TO ADDRESS 1-BYTE PRODUCT SAVE AREA                            
********************************************************************            
GETPROD  NTR1                                                                   
*                                                                               
         L     R4,AREC                                                          
         USING CLTHDR,R4                                                        
         LA    R6,CLIST+880                                                     
         LA    R4,CLIST            LIST OF PRODUCTS                             
GETP10   DS    0H                                                               
         CLC   0(3,R4),=AL3(0)     PAST THE LAST PRODUCT?                       
         BNE   *+6                                                              
         DC    H'0'                PRODUCT NOT FOUND                            
         CR    R4,R6               END OF LIST?                                 
         BL    *+6                                                              
         DC    H'0'                PRODUCT NOT FOUND                            
*                                                                               
         CLC   0(3,R2),0(R4)                                                    
         BNE   GETP20              NOT THIS ONE, GET NEXT                       
*                                                                               
         MVC   0(1,R3),3(R4)       SAVE 1-BYTE PRODUCT CODE                     
         B     EQXIT               GET OUT                                      
*                                                                               
GETP20   DS    0H                                                               
         LA    R4,4(R4)            ADVANCE TO NEXT PRODUCT IN LIST              
         B     GETP10                                                           
         DROP  R4                                                               
*                                                                               
********************************************************************            
* FILTGOAL - CHECKS REQUEST PARAMETERS AND GOAL RECORD                          
* ADGOAL EXPECTED TO CONTAIN ADDRESS OF THE GOAL RECORD                         
* EQUAL/UNEQUAL EXIT CONDITION IF RECORD FITS/DOESN'T FIT REQ. PARAMS           
********************************************************************            
FILTGOAL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ADGOAL                                                        
         USING GKEY,R2                                                          
*                                                                               
         TM    REQFLAG,REQP1Q              IS P1 NAME GIVEN?                    
         BNO   FILT10                                                           
         CLC   GKEYPRD,PRD1BIN                                                  
         JNE   NEQXIT                                                           
*                                                                               
FILT10   DS    0H                                                               
         TM    REQFLAG,REQP2ALQ       P2 = ALL?                                 
         BNO   FILT40                                                           
*                                                                               
         CLI   QGRP,C'Y'              IF 'Y' - ANY GOAL IS OK                   
         BE    FILT40                 IF 'N' DON'T ACCEPT SOLO GOALS            
         BAS   RE,ISSOLO                                                        
         JE    NEQXIT                                                           
*                                                                               
FILT40   DS    0H                                                               
         TM    REQFLAG,REQP2Q         P2 REQUESTED BY NAME?                     
         BNO   FILT50                                                           
*                                                                               
         CLC   GKEYPRD2,PRD2BIN       P2 FROM SPONSOR = P2 IN REQUEST?          
         JE    EQXIT                                                            
         BAS   RE,ISSOLO              NO OTHER PIGGYBACKS                       
         JNE   NEQXIT                                                           
         CLI   QGRP,C'Y'                                                        
         JNE   NEQXIT                                                           
*                                                                               
FILT50   DS    0H                                                               
         TM    REQFLAG,REQP2NOQ       P2 BLANK ?                                
         BNO   FILT60                                                           
         BAS   RE,ISSOLO                                                        
         JNE   NEQXIT                                                           
*                                                                               
FILT60   DS    0H                                                               
         CLI   QOPT1,C' '          DAYPART (QOPT1)                              
         BE    FILT70                                                           
         CLC   QOPT1,GKEYDPT                                                    
         JNE   NEQXIT                                                           
*                                                                               
FILT70   CLC   =CL3'   ',QOPT2       SPOTLEN (QOPT2,3,4)                        
         BE    FILTX                                                            
         CLC   BSLN,GKEYSLN                                                     
         JNE   NEQXIT                                                           
*                                                                               
FILTX    DS    0H                                                               
         J     EQXIT                                                            
         DROP  R2                                                               
                                                                                
*                                                                               
* R2 IS EXPECTED TO ADDRESS GOAL RECORD                                         
* EQUAL EXIT CONDITION IF SOLO RECORD, UNEQUAL OTHERWISE                        
ISSOLO   NTR1                                                                   
         USING GKEY,R2                                                          
         TM    GKEYAGY,X'40'          GKEYPRD2 IS NOT PROD NUMBER?              
         JO    EQXIT                                                            
         CLI   GKEYPRD2,0             PROD2 PRESENT IN THIS GOAL REC?           
         JE    EQXIT                                                            
         J     NEQXIT                                                           
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
         DS    0F                                                               
TOTALS   DS    0XL24                                                            
DOLTOT   DS    F                                                                
ESTDOLS  DS    F                                                                
PTOT     DS    F                                                                
ESTPTS   DS    F                                                                
MKTDOLS  DS    F                                                                
MKTDOLF  DS    F                                                                
*                                                                               
REGSTR   DS    4F                                                               
*                                                                               
PRD1BIN  DS    X                                                                
PRD2BIN  DS    X                                                                
*                                                                               
REQFLAG  DS    X                                                                
REQP1Q   EQU   X'01'                                                            
REQP1ALQ EQU   X'02'                                                            
REQP1NOQ EQU   X'04'                                                            
REQP2Q   EQU   X'10'                                                            
REQP2ALQ EQU   X'20'                                                            
REQP2NOQ EQU   X'40'                                                            
*                                                                               
*                                                                               
DELSW    DS    CL1                                                              
FRSTSW   DS    CL1                                                              
ELCODE   DS    CL1                                                              
BESTSV   DS    CL1                                                              
ESTOTSW  DS    CL1                                                              
MKTOTSW  DS    CL1                                                              
STDTSV   DS    CL2                                                              
ENDTSV   DS    CL2                                                              
BMKTSV   DS    CL2                                                              
BSLN     DS    CL1                                                              
*                                                                               
RECCNT   DC    PL4'0'                                                           
ESTCNT   DC    PL4'0'                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
GKEYSAV  DS    XL13                                                             
IO       DS    4096C                                                            
*                                                                               
PLINED   DSECT                                                                  
*                                                                               
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL3                                                              
PMKNM    DS    CL24                                                             
         DS    CL4                                                              
PEST     DS    CL3                                                              
         DS    CL4                                                              
PDPT     DS    CL1                                                              
         DS    CL4                                                              
PSLN     DS    CL3                                                              
         DS    CL3                                                              
PSDTE    DS    CL8                                                              
         DS    CL3                                                              
PEDTE    DS    CL8                                                              
         DS    CL3                                                              
PDOLS    DS    CL10                                                             
         DS    CL3                                                              
PPTS     DS    CL8                                                              
         EJECT                                                                  
SPK302   CSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENCLT                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087SPREPK302 05/01/02'                                      
         END                                                                    

*          DATA SET ACINQ10    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T60610A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - CONTRA LIST (CL) - T60610'                
T60610   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'CL' IN ACCOUNT ENQUIRY PROGRAM                  
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60610)                                               
         DC    A(FILTABLE-T60610)                                               
         DC    A(KNTRYPNT-T60610)                                               
         DC    A(FNTRYPNT-T60610)                                               
         DC    A(DNTRYPNT-T60610)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(27)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(AHST-GWS)                                                    
         DC    AL1(TRHSYEAR-TRHISTD)                                            
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(AHST-GWS)                                                    
         DC    AL1(TRHSYEAR-TRHISTD)                                            
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'BUCKTYPE'                                                   
         DC    CL2'BT'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYREF-ACKEYD)                                             
         DC    AL1(1)                                                           
         DC    AL2(EDITOPT1-GWS)   SET OPTIONS X'01' BIT                        
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              HANDLE CONTRA-ACCOUNT FILTER                                     
*              (CALLED BY FILTER ROUTINE IN ROOT)                               
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ10*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60610,RB                                                        
         USING T606TWA,RA                                                       
         LA    R2,WORK             SET UP DUMMY COMPARANDS                      
         BCTR  R3,0                R3 = LENGTH MINUS 1                          
         EX    R3,VCONTMV1         EQUATE COMPARANDS                            
         LA    R4,1                R4 = DISPLACEMENT INTO CA/C                  
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLC   ACKEYCON(3),SPACES                                               
         BNE   F02                                                              
         LA    R4,3                                                             
         CLC   ACKEYCON(12),SPACES                                              
         BNE   F02                                                              
         LA    R4,12                                                            
F02      LA    R7,ACKEYCON(R4)                                                  
         EX    R3,VCONTCLC                                                      
         BL    VCONTNO                                                          
         SPACE 1                                                                
         USING FTBD,R6                                                          
         MVI   FTBSTAT,1           DISABLE FILTER AS WEVE REACHED START         
         DROP  R6                                                               
         CLC   SAVECACN,SPACES     SAVE NAME IF AVAILABLE                       
         BNE   VCONTX                                                           
         LA    R9,L'ACKEYCON                                                    
         SR    R9,R4                                                            
         BCTR  R9,0                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R9,VCONTCLC         DOES THE CA/C READ MATCH THE FILTER          
         BNE   VCONTX              IN FULL                                      
         CLI   ACRECORD,X'43'      IF SO IS THIS A CA/C HEADER                  
         BNE   VCONTX                                                           
         LA    R7,ACRECORD         IF SO SAVE THE NAME                          
         USING TRSUBHD,R7                                                       
         IC    R4,TRSBLEN                                                       
         SH    R4,=H'18'                                                        
         BNP   VCONTX                                                           
         EX    R4,VCONTMV4                                                      
         B     VCONTX                                                           
         SPACE 1                                                                
VCONTNO  MVI   WORK,0              UNEQUATE COMPARANDS                          
         MVC   0(25,R7),SPACES     RESET KEY TO READ HIGH FOR THIS              
         EX    R3,VCONTMV5         CONTRA NEXT TIME                             
         MVC   KEY(L'ACCKEY),ACKEYD                                             
         MVI   KEY+L'ACCKEY-1,0                                                 
         MVI   DMARK,1                                                          
VCONTX   LA    R3,1(R3)                                                         
         LA    R5,INFCAC                                                        
         XIT1  REGS=(R2,R5)                                                     
         SPACE 1                                                                
VCONTMV1 MVC   WORK(0),INFCAC                                                   
VCONTMV4 MVC   SAVECACN(0),TRSBNAME                                             
VCONTCLC CLC   0(0,R7),INFCAC                                                   
VCONTMV5 MVC   0(0,R7),INFCAC                                                   
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ10*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60610,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLI   LASTKMK,0           CONTINUATION                                 
         BNE   TFIRST1                                                          
         SPACE 1                                                                
TFIRST   CLI   VIRGIN,2            FIRST REC                                    
         BH    TCONT                                                            
         ZAP   TOTDR,=P'0'         CLEAR ACCOUNT TOTALS                         
         ZAP   TOTCR,=P'0'                                                      
TFIRST1  MVI   VIRGIN,2                                                         
         CLI   INFCACH+5,0         INDICATE FILTER IS RECORD LEVEL              
         BE    TCONT                                                            
         NI    FILTAB+(FTBSTAT-FTBD),X'FB'                                      
         B     TCONT                                                            
         EJECT                                                                  
*              PROCESS A CONTRA HEADER                                          
         SPACE 1                                                                
TCONT    LH    R6,LINE             POINT R6 TO DISPLAY LINE                     
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         CLI   ACKEYACC,RUNLAST    END OF RECS                                  
         BE    TEND                                                             
         SPACE 1                                                                
T1       TM    OPTIONS,X'01'       IF WE ARE NOT FILTERING ON BUCKTYPE          
         BO    T2                  BUCKTYPE MUST BE SPACE                       
         CLI   ACKEYREF,C' '                                                    
         BNE   TNEXT                                                            
         SPACE 1                                                                
T2       ZAP   DEBIT,=P'0'         PREPARE FOR HISTORY EL LOOP                  
         ZAP   CREDIT,=P'0'                                                     
         LR    R9,R5                                                            
         MVI   ELCODE,X'55'        FIRST GET 55 IF THERE                        
         BAS   RE,GETEL                                                         
         BE    T3B                                                              
         USING TRHISTD,R9                                                       
T2B      LR    R9,R5                                                            
         MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
T3       BAS   RE,NEXTEL                                                        
         BNE   T31                                                              
T3B      ST    R9,AHST                                                          
         BAS   RE,CHADSP           FIDDLE WITH DISP. TO DATES                   
         MVI   DMCB,0              ELEMENT FILTERING                            
         L     RF,AFILTER                                                       
         BASR  RE,RF                                                            
         BZ    T3X                                                              
         CLI   ELCODE,X'55'                                                     
         BE    T3D                                                              
         AP    DEBIT,TRHSDR                                                     
         AP    CREDIT,TRHSCR                                                    
         B     T3F                                                              
         USING ACPBKD,R9                                                        
T3D      AP    DEBIT,ACPBKDR                                                    
         AP    CREDIT,ACPBKCR                                                   
T3F      LA    R0,1                                                             
T3X      CLI   ELCODE,X'55'                                                     
         BE    T2B                                                              
         B     T3                                                               
         SPACE 1                                                                
T31      CP    DEBIT,=P'0'         AT END OF REC CHECK FOR HITS                 
         BNE   *+14                                                             
         CP    CREDIT,=P'0'                                                     
         BZ    TNEXT                                                            
         MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,4                                                         
         BE    TDISP2                                                           
         BH    TDISP3                                                           
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
         MVC   INFDAT3,HEADING     AND HEADING                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT4,HEADING2                                                 
         OI    INFDAT4H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
         LA    R6,INFDAT5H                                                      
         B     TDISP3                                                           
HEADING  DC    CL39'CONTRA CODE / NAME'                                         
         DC    CL39'    DEBITS       CREDITS    DIFFERENCE'                     
HEADING2 DC    CL39'------------------'                                         
         DC    CL39'    ------       -------    ----------'                     
TDISP2   MVC   LINEDATA(3),=C'B/F' BROUGHT FORWARD                              
         LA    R4,TOTDR                                                         
         BAS   RE,DISPVALS                                                      
         LA    R6,LINELEN(R6)                                                   
         LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         MVI   LASTKMK,0                                                        
         SPACE 1                                                                
TDISP3   CLI   LINE+1,18           DISPLAY A CONTRA LINE                        
         BE    TFULL                                                            
         MVC   WORK(53),SPACES                                                  
         MVC   WORK(14),ACKEYCON+1                                              
         LR    R9,R5                                                            
         MVI   ELCODE,X'43'                                                     
         BAS   RE,GETEL                                                         
         BNE   TDISP4                                                           
         USING TRSUBHD,R9                                                       
         ZIC   RE,TRSBLEN                                                       
         SH    RE,=H'18'                                                        
         BNP   TDISP4                                                           
         MVI   WORK+15,C'/'                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+17(0),TRSBNAME                                              
TDISP4   GOTO1 VSQASHER,DMCB,WORK,53                                            
         MVC   LINEDATA(35),WORK                                                
         LA    R4,DEBIT                                                         
         BAS   RE,DISPVALS                                                      
         AP    TOTDR,DEBIT                                                      
         AP    TOTCR,CREDIT                                                     
         LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         B     TNEXT                                                            
         EJECT                                                                  
*              HANDLE SCREEN FULL AND END CONDITIONS                            
         SPACE 1                                                                
TEND     MVC   LINEDATA(13),=C'ACCOUNT TOTAL'                                   
         LA    R4,TOTDR                                                         
         BAS   RE,DISPVALS                                                      
         SR    R0,R0               CC = EQU FOR END                             
         B     TXIT                                                             
         SPACE 1                                                                
TFULL    MVC   LINEDATA(3),=C'C/F' IF SCREEN FULL AND MORE TO COME              
         LA    R4,TOTDR            DISPLAY C/F TOTALS                           
         BAS   RE,DISPVALS                                                      
         OI    LASTKMK,X'80'       CONTINUATION BIT                             
         LNR   RE,RE               CC = NEQ FOR SCREEN FULL                     
         MVI   LINE+1,4                                                         
         B     TXIT                                                             
         SPACE 1                                                                
TNEXT    TM    OPTIONS,X'01'       IF NO BUCKTYPE FILTER                        
         BO    TNEXT1              FORCE READ HIGH FOR NEXT CONTRA              
         MVI   KEY+32,X'FF'                                                     
         MVI   KEY+41,X'3F'                                                     
         MVI   DMARK,1                                                          
TNEXT1   LTR   RB,RB               CC = POS FOR NEXT RECORD PLEASE              
         SPACE 1                                                                
TXIT     XIT1                      CC = EQU FOR END                             
         EJECT                                                                  
*              ROUTINE TO ADJUST DISPLACEMENT TO DATE FILTERS                   
         SPACE 2                                                                
         USING FTBD,RF                                                          
CHADSP   DS    0H                                                               
         LA    RF,FILTAB                                                        
CHAD2    CLI   FTBELMNT,X'FF'      END OF TABLE                                 
         BER   RE                                                               
         TM    FTBSTAT,X'24'       IS THIS A DATE FILTER                        
         BO    *+12                                                             
CHAD4    LA    RF,FTBTBLEN(RF)                                                  
         B     CHAD2                                                            
         MVI   FTBDISP,TRHSYEAR-TRHISTD  DEFAULT IS 45 EL.                      
         CLI   0(R9),X'55'                                                      
         BNE   CHAD4                                                            
         MVI   FTBDISP,ACPBKHI-ACPBKD    ELSE USE END DATE FROM 55 EL.          
         B     CHAD4                                                            
         EJECT                                                                  
*              DISPLAY A LINE OF VALUES                                         
*              ON ENTRY R4 = A(DEBIT/CREDIT PAIR OF PL8 VALUE FIELDS)           
*                       R6 = A(LINE HEADER) COVERED BY DSECT LINED              
         SPACE 1                                                                
DISPVALS OI    LINEHDR+6,X'80'                                                  
         LA    RF,LINEDATA-2       RF=NOTIONAL COLUMN ZERO ON SCREEN            
         ZAP   DUB,0(8,R4)                                                      
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         MVC   38(14,RF),WORK+4                                                 
         ZAP   DUB,8(8,R4)                                                      
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         MVC   52(14,RF),WORK+4                                                 
         ZAP   WORK(8),0(8,R4)                                                  
         SP    WORK(8),8(8,R4)                                                  
         ZAP   DUB,WORK(8)                                                      
         MVC   WORK+20(18),=X'40202020202020202020202020214B202060'             
         ED    WORK+20(18),DUB                                                  
         MVC   66(14,RF),WORK+24                                                
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R9,DATADISP,ELCODE                                               
         SPACE 1                                                                
*              DSECT TO COVER LOCAL WORKING STORAGE                             
LOCALD   DSECT                                                                  
DEBIT    DS    PL8       P         TOTAL CONTRA DEBITS                          
CREDIT   DS    PL8       P         TOTAL CONTRA CREDITS                         
         SPACE 1                                                                
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
TOTDR    DS    PL8       P         TOTAL ACCOUNT DEBITS                         
TOTCR    DS    PL8       P         TOTAL ACCOUNT CREDITS                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACINQ10   05/01/02'                                      
         END                                                                    

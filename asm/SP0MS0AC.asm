*          DATA SET SP0MS0AC   AT LEVEL 093 AS OF 12/11/01                      
*PHASE T2340AA                                                                  
T2340A   TITLE 'SPOMS0A - BATCH DARE ORDERS'                                    
T2340A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2340A*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         OI    GENSTAT1,RDUPAPPL   APPL RESPON. FOR SETTING RDUPDATE            
         MVI   RDUPDATE,C'Y'       SO VALLIST WILL READ FOR UPDATE              
         OI    GLSTSTAT,APPLCDSP+RETEXTRA+CHNGLIST                              
         OI    GENSTAT5,SEL1BYTE   1 BYTE SELECT FIELD                          
         MVC   MKTPFLN(27),=CL27'PF6=Transmit PF9=Order/Send'                   
         OI    MKTPFLNH+6,X'80'                                                 
*                                                                               
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
*   DON'T CLEAR PREVIOUS LIST KEY BITS OR TRIED TO TRANSMIT AN ORDER            
         NI    BITFLAG,BFPREVKY+BFXMITED                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD?                      
         BE    LVR                                                              
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08' DON'T PASS BACK DELETED RECORDS              
         MVI   NLISTS,13           # OF LINES TO LIST, DEFAULT = 13             
         MVC   LLIST,=Y(LINNEXTL-LINDTLH)                                       
         MVI   MISCFLG1,0                                                       
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  DS    0H                                                               
         LA    R2,MKTMEDH                                                       
         TM    4(R2),X'20'            IF THIS KEY FIELD CHANGE                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG      THEN INDICATE IT                          
*                                                                               
         CLI   5(R2),0             NEED THE MEDIA                               
         BNE   VKMED05                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPMD                                 
         CLI   8(R1),0                                                          
         BE    VKMED05                                                          
         B     NEEDFLDS                                                         
*                                                                               
VKMED05  GOTO1 VALIMED                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPMD                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VKMEDX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE BUYER                                                                
***************                                                                 
VKBYR00  DS    0H                                                               
         LA    R2,MKTBUYRH                                                      
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY CHANGED, REDISPLAY LIST                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIBUYR,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
         OC    QBUYER,8(R2)                                                     
         MVC   SVBUYRCD,8(R2)                                                   
*                                                                               
VKBUYRX  OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE THE MARKET                                                           
***************                                                                 
VKMKT00  DS    0H                                                               
         NI    FILTFLG1,X'FF'-FFLG1MKT                                          
         LA    R2,MKTMRKTH         DID THIS FIELD CHANGE?                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             IF NO MARKET INPUT                           
         BNE   VKMKT10                                                          
         CLI   MKTSTAH+5,0         THEN MAKE SURE STATION IS FILLED IN          
         BE    MISSFLD             OTHERWISE, WE NEED THIS FIELD                
         B     VKMKTX                                                           
*                                                                               
VKMKT10  TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    INVLFLD             NO                                           
*                                                                               
         XR    R1,R1               SAVE THE MARKET NUMBER                       
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,FLTMKT                                                      
         OI    FILTFLG1,FFLG1MKT                                                
*                                                                               
VKMKTX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE THE STATION                                                          
***************                                                                 
VKSTA00  DS    0H                                                               
         NI    FILTFLG1,X'FF'-FFLG1STA                                          
         LA    R2,MKTSTAH          DID THIS FIELD CHANGE?                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKSTAX                                                           
*                                                                               
         CLI   8(R2),C'0'          CABLE STATION?                               
         BNL   INVLFLD             YES, NOT YET                                 
         GOTO1 VALISTA                                                          
         OI    FILTFLG1,FFLG1STA                                                
         MVC   FLTSTA,BSTA                                                      
*                                                                               
VKSTAX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE THE CLIENT                                                           
***************                                                                 
VKCLT00  DS    0H                                                               
         NI    FILTFLG1,X'FF'-FFLG1CLT                                          
         LA    R2,MKTCLTH          DID THIS FIELD CHANGE?                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKCLTX                                                           
*                                                                               
         GOTO1 VALICLT                                                          
         OI    FILTFLG1,FFLG1CLT                                                
         MVC   FLTCLT,BCLT                                                      
*                                                                               
VKCLTX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE THE PRODUCT(S)                                                       
***************                                                                 
VKPRD00  DS    0H                                                               
         NI    FILTFLG1,X'FF'-FFLG1PRD                                          
         LA    R2,MKTPRDH          DID THIS FIELD CHANGE?                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         OC    MKTPRD,SPACES                                                    
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKPRDX                                                           
*                                                                               
         CLI   MKTCLTH+5,0         IF WE HAVE A PRODUCT                         
         BNE   *+12                                                             
         LA    R2,MKTCLTH          THEN WE NEED A CLIENT                        
         B     MISSFLD                                                          
*                                                                               
         ZIC   RE,5(R2)            CONVERT THE '/' TO '-'                       
         LA    RF,8(R2)                                                         
VKPRD10  CLI   0(RF),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         BCT   RE,VKPRD10                                                       
*                                                                               
         L     RE,AIO2                                                          
         XC    0(256,RE),0(RE)                                                  
         GOTO1 SCANNER,DMCB,(R2),(X'82',AIO2),C',=-='                           
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
*                                                                               
         L     R3,AIO2                                                          
         CLI   0(R3),0                                                          
         BE    INVLFLD                                                          
         CLI   1(R3),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
*                                                                               
         MVC   FAKEFLDH,0(R2)                                                   
         MVC   FAKEFLDH+5(1),0(R3)                                              
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
         OC    FAKEFLD(8),SPACES                                                
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   SVBPRD,BPRD         SAVE PRIMARY PRODUCT CODE HERE FIRST         
         MVC   SVQPRD,QPRD             WE KNOW *** IF SVBPRD=X'FF' EVEN         
         OI    FILTFLG1,FFLG1PRD                                                
*                                                                               
         MVI   SVBPR2,0            CLEAR PIGGYBACK IF NONE                      
         XC    SVQPR2,SVQPR2                                                    
*                                                                               
         LA    R3,32(R3)           R3 = A(PIGGYBACK PRODUCT) IF ANY             
         CLI   0(R3),0                                                          
         BE    VKPRD20                                                          
         CLI   1(R3),0             PRODUCT CAN'T HAVE SUB-FIELDS                
         BNE   INVLFLD                                                          
*                                                                               
         CLI   BPRD,X'FF'          NO PIGGYBACK IF POL ORDER                    
         BE    INVLFLD                                                          
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
*                                                                               
         ZICM  R0,FAKEFLDH+2,2     CALCULATE COL OF PIGGY                       
         ZIC   R1,4(R3)                                                         
         AR    R0,R1                                                            
         STCM  R0,3,FAKEFLDH+2                                                  
         MVC   FAKEFLDH+5,0(R3)    GIVE IT THE PROPER LENGTH                    
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
         OC    FAKEFLD,SPACES                                                   
*                                                                               
         GOTO1 VALIPRD                                                          
         CLI   BPRD,X'FF'          POL PRODUCT?                                 
         BE    NOPOLPRD            YES, SHOULD NOT BE ACCEPTED                  
         MVC   SVBPR2,BPRD                                                      
         MVC   SVQPR2,QPRD                                                      
         MVC   BPRD,SVBPRD         REESTAB PRIMARY PRODUCT VALUE                
         MVC   QPRD,SVQPRD                                                      
*                                                                               
VKPRD20  L     R1,ATIOB            TAKE OFF ERROR MESSAGE CURSOR                
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
         LA    R2,MKTPRDH                                                       
         OI    6(R2),X'80'                                                      
VKPRDX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE THE ESTIMATE                                                         
***************                                                                 
VKEST00  DS    0H                                                               
         NI    FILTFLG1,X'FF'-FFLG1EST                                          
         LA    R2,MKTESTH          DID THIS FIELD CHANGE?                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKESTX                                                           
         CLI   MKTPRDH+5,0         IF WE HAVE A ESTIMATE                        
         BNE   *+12                                                             
         LA    R2,MKTPRDH          THEN WE NEED A PRODUCT                       
         B     MISSFLD                                                          
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC FIELD?                         
         BZ    INVLFLD                                                          
*                                                                               
         GOTO1 VALIEST                                                          
         OI    FILTFLG1,FFLG1EST                                                
*                                                                               
         OC    SVQPR2,SVQPR2                                                    
         BZ    VKEST10                                                          
         MVC   QPRD,SVQPR2         VALIDATE AGAINST THE PIGGYBACK               
         GOTO1 VALIEST                                                          
         MVC   QPRD,SVQPRD                                                      
*                                                                               
VKEST10  MVC   SVBEST,BEST                                                      
*                                                                               
VKESTX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE THE FLIGHT                                                           
***************                                                                 
VKFLT00  DS    0H                                                               
         NI    FILTFLG1,X'FF'-FFLG1FLT                                          
         MVI   SVFLTNUM,0                                                       
         LA    R2,MKTFLTH          DID THIS FIELD CHANGE?                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKFLTX                                                           
         CLI   MKTESTH+5,0         IF WE HAVE A FLIGHT                          
         BNE   *+12                                                             
         LA    R2,MKTESTH          THEN WE NEED A ESTIMATE                      
         B     MISSFLD                                                          
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC FIELD?                         
         BZ    INVLFLD                                                          
*                                                                               
         BAS   RE,GETFLTRC         ANY FLIGHT RECORDS?                          
         BNE   INVLFLD                                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CLM   R1,1,HIFLTNUM       FLIGHT NUMBER MUST BE <=16                   
         BH    INVLFLD                                                          
         STC   R1,SVFLTNUM                                                      
         OI    FILTFLG1,FFLG1FLT                                                
*                                                                               
VKFLTX   OI    4(R2),X'20'         VALIDATE THIS FIELD                          
***************                                                                 
* VALIDATE OPTIONS                                                              
***************                                                                 
VKOPT00  DS    0H                                                               
         LA    R2,MKTOPTNH         OPTIONS CURRENTLY DO NOT AFFECT THE          
         OC    MKTOPTN,SPACES        LISTING OF THE BATCH RECORDS               
*                                                                               
         MVI   OPTNFLAG,0                                                       
         CLI   5(R2),0                                                          
         BE    VKOPTX                                                           
         XR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SENDALL'                                              
         BNE   INVLFLD                                                          
         OI    OPTNFLAG,OFLGSNDA                                                
         EX    R1,*+8              CLEAR THIS OUT SO WE DON'T DO THIS           
         B     *+10                    AGAIN                                    
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         L     R2,AFRSTREC                                                      
VKOPT10  LA    R0,MKTPFLNH                                                      
         CR    R2,R0                                                            
         BNL   VKOPTX                                                           
         USING LINDSECT,R2                                                      
         OC    LINDTL,LINDTL                                                    
         BZ    VKOPTX                                                           
         MVI   LINSEL,C'T'                                                      
         MVI   LINSELH+5,1                                                      
         NI    LINSELH+4,X'FF'-X'20'                                            
         LA    R2,LINNEXTL                                                      
         B     VKOPT10                                                          
*                                                                               
VKOPTX   OI    4(R2),X'20'         VALIDATED                                    
         TM    OPTNFLAG,OFLGSNDA   SENDING ALL?                                 
         BZ    VKXIT               NO, EXIT THEN                                
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
VKXIT    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORDS ON SCREEN                                                    
***********************************************************************         
LVR      DS    0H                                                               
         L     R2,ATHISLST         R2 = A(LIST LINE BEING VALIDATED)            
         B     LVR10                                                            
VALLINE  NTR1                                                                   
*                                                                               
         USING DTLLINED,R2                                                      
LVR10    L     R6,AIO                                                           
         USING DBTKEY,R6                                                        
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
         MVC   BCLT,DBTKCLT        SET UP VALUES FOR GETFLTRC                   
         MVC   BEST,DBTKEST                                                     
         MVC   QPRD,DTLPRD                                                      
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         NI    BITFLAG,X'FF'-BFFLGHTS                                           
         MVI   CURRFLTN,0                                                       
         BAS   RE,GETFLTRC    <<=====   MESSES WITH KEY SEQUENCING              
         MVC   AIO,AIO1                                                         
         BNE   LVR15                                                            
         OI    BITFLAG,BFFLGHTS                                                 
         PACK  DUB,DTLFLT                                                       
         CVB   R1,DUB                                                           
         STC   R1,CURRFLTN                                                      
*                                                                               
LVR15    L     R6,AIO                                                           
         MVC   KEY(L'DBTKEY),0(R6)                                              
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
LVR20    L     R6,AIO                                                           
         USING DBTKEY,R6                                                        
         MVI   ELCODE,DBFLTELQ                                                  
         NI    BITFLAG,X'FF'-BFNOFLEL   ASSUME WE HAVE A FLIGHT ELEM            
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
LVR22    OI    BITFLAG,BFNOFLEL         DON'T HAVE CORRECT FLT ELEM             
         B     LVR30                     - WE ALSO KNOW NOT DARE COMMNT         
*                                                                               
         USING DBFLTELD,R6                                                      
LVR24    TM    BITFLAG,BFFLGHTS    ARE WE USING FLIGHTS?                        
         BNZ   LVR26                                                            
         CLI   DBFLTFLT,0                                                       
         BNE   LVR22                                                            
         B     LVR30                                                            
*                                                                               
LVR26    CLC   DBFLTFLT,CURRFLTN   YES, PASSED OUR FLIGHT?                      
         BE    LVR28                                                            
         BH    LVR22                                                            
         BAS   RE,NEXTEL                                                        
         BNE   LVR22                                                            
         B     LVR24                                                            
*                                                                               
LVR28    TM    DBFLTFL1,DBFLTDAR   LINKED TO A DARE ORDER?                      
         BNZ   LVR60                                                            
*                                                                               
LVR30    CLI   DTLSCMH+5,0         NO COMMENT ON THE LINE                       
         BNE   LVR40                                                            
         TM    BITFLAG,BFNOFLEL    DID WE HAVE ONE BEFORE?                      
         BNZ   LVRX                NO, THEN WE'RE OKAY                          
         ZIC   R1,DBFLTLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)                                  
         MVI   DBFLTLEN,DBFLTOVH   GET RID OF THE COMMENT                       
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         B     LVRX                                                             
         DROP  R6                                                               
*                                                                               
LVR40    LA    R1,ELEM             CREATE THE COMMENT ELEMENT                   
         XC    ELEM,ELEM                                                        
         USING DBFLTELD,R1                                                      
         MVI   DBFLTEL,DBFLTELQ                                                 
         MVC   DBFLTFLT,CURRFLTN                                                
         XR    RE,RE                                                            
         IC    RE,DTLSCMH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DBFLTTXT(0),DTLSCM                                               
         AHI   RE,DBFLTOVH+1                                                    
         STC   RE,DBFLTLEN                                                      
         DROP  R1                                                               
*                                                                               
         TM    BITFLAG,BFNOFLEL                 DID WE HAVE ONE BEFORE?         
         BZ    LVR50                            YES                             
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  NO, ADD IT TO THE REC           
         B     LVRX                                                             
*                                                                               
         USING DBFLTELD,R6                                                      
LVR50    MVC   ELEM+DBFLTFL1-DBFLTELD(1),DBFLTFL1  COPY THE FLAG                
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)  REMOVE IT                       
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  ADD THE NEW ONE                 
         B     LVRX                                                             
***********************************                                             
* HERE WE ARE MODIFYING THE AGENCY COMMENT RECORD (AIO2)                        
***********************************                                             
LVR60    L     R0,AIO1             COPY THIS RECORD INTO AIO2                   
         LA    R1,LIOS                                                          
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R1,ELEM             CREATE THE COMMENT ELEMENT                   
         XC    ELEM,ELEM                                                        
         USING DOCOMELD,R1                                                      
         MVI   DOCOMEL,DOCOMELQ                                                 
         MVI   DOCOMLIN,1                                                       
         MVC   DOCOMTXT(L'DTLSCM),DTLSCM                                        
         OC    DOCOMTXT(L'DTLSCM),SPACES                                        
         MVI   DOCOMLEN,DOCOMOVH+L'DTLSCM                                       
         DROP  R1                                                               
*                                                                               
LVR65    MVC   AIO,AIO2                                                         
         GOTO1 GTDARCMT,SAVEKEY    <<====   MESSES WITH KEY & AIO               
         BE    LVR70               WE HAVE A DARE AGENCY COMMENT RECORD         
         CLI   DTLSCMH+5,0         ANY COMMENT ON THE LINE?                     
         BE    LVR99               NOTHING ON LINE EITHER                       
***************                                                                 
* NEED TO ADD DARE AGENCY COMMENT RECORD                                        
***************                                                                 
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         MVC   DOKEY,ORDERKEY                                                   
         MVI   DOKCMT,X'01'        DARE AGENCY COMMENT RECORD                   
         MVC   DORAGY,AGENCY                                                    
         LA    R1,DORFRST                                                       
         XR    RE,RE                                                            
         IC    RE,ELEM+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ELEM                                                     
         AHI   RE,DORFRST-DOKEY+1                                               
         STCM  RE,3,DORLEN                                                      
         GOTO1 ADDREC                                                           
         B     LVR99                                                            
***************                                                                 
* WE HAVE A DARE AGENCY COMMENT RECORD OUT THERE                                
***************                                                                 
LVR70    L     R6,AIO                                                           
         MVI   ELCODE,DOCOMELQ                                                  
         BAS   RE,GETEL                                                         
         BE    LVR80                                                            
LVR75    CLI   DTLSCMH+5,0                                                      
         BE    LVR99                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  ADD COMMENT TO THE REC          
         B     LVR90                                                            
*                                                                               
         USING DOCOMELD,R6                                                      
LVR80    CLI   DOCOMLIN,1          FIRST COMMENT LINE?                          
         BNE   LVR75                                                            
         CLC   DOCOMLEN,ELEM+1                                                  
         BL    LVR85                                                            
         MVC   DOCOMTXT(L'DTLSCM),DTLSCM                                        
         OC    DOCOMTXT(L'DTLSCM),SPACES                                        
         B     LVR90                                                            
*                                                                               
LVR85    GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)  DELETE OLD ONE                  
         CLI   DTLSCMH+5,0                                                      
         BE    LVR90                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  ADD COMMENT TO THE REC          
*                                                                               
LVR90    GOTO1 PUTREC                                                           
*                                                                               
LVR99    MVC   KEY(L'DBTKEY),SAVEKEY                                            
         GOTO1 READ                                                             
         GOTO1 GETREC              READ THIS BACK TO ESTABLISH PTRS             
         MVC   AIO,AIO1            THIS IS OUR CHANGED BATCH RECORD             
*                                                                               
LVRX     OI    DTLSCMH+4,X'20'     SET VALIDATED                                
         OI    DTLSCMH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         LA    RE,LISTTABL                                                      
         ST    RE,ALSTKEYS                                                      
         LA    RF,LISTTBLX-LISTTABL                                             
         XCEFL                                                                  
         NI    BITFLAG,X'FF'-BFLSTMON                                           
*                                                                               
         LA    R2,MKTSELLH                                                      
         USING LINDSECT,R2                                                      
         LA    R2,LINSCMH                                                       
         DROP  R2                                                               
         TWAXC MKTSEL1H,(R2),PROT=Y                                             
*                                                                               
         LA    R4,KEY                                                           
         USING DBTKEY,R4                                                        
*                                                                               
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGED?                         
         BNZ   LR10                YES, START FROM BEGINNING                    
         TM    BITFLAG,BFPREVKY    A PREVIOUS KEY EXAMINED?                     
         BZ    LR10                                                             
         MVC   KEY(L'PREVKEY),PREVKEY   YES, THEN USED THAT KEY                 
         B     LR20                                                             
*                                                                               
LR10     MVI   CURRFLTN,0                                                       
         NI    BITFLAG,X'FF'-BFPREVKY   A PREVIOUS KEY EXAMINED?                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   DBTKTYP,DBTKTYPQ                                                 
         MVI   DBTKSTYP,DBTKSTYQ                                                
         MVC   DBTKAGMD,BAGYMD                                                  
*                                                                               
         TM    FILTFLG1,FFLG1MKT   FILTERING BY MARKET?                         
         BZ    LR12                                                             
         MVC   DBTKMKT,FLTMKT                                                   
*                                                                               
LR12     TM    FILTFLG1,FFLG1STA   FILTERING BY STATION?                        
         BZ    LR14                                                             
         MVC   DBTKSTA,FLTSTA                                                   
*                                                                               
LR14     TM    FILTFLG1,FFLG1CLT   FILTERING BY CLIENT?                         
         BZ    LR16                                                             
         MVC   DBTKCLT,FLTCLT                                                   
*                                                                               
LR16     TM    FILTFLG1,FFLG1PRD   FILTERING BY PRODUCT?                        
         BZ    LR18                                                             
         MVC   DBTKPRD,SVBPRD                                                   
         MVC   DBTKPRD2,SVBPR2                                                  
*                                                                               
LR18     TM    FILTFLG1,FFLG1EST   FILTERING BY ESTIMATE?                       
         BZ    LR20                                                             
         MVC   DBTKEST,SVBEST                                                   
*                                                                               
LR20     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
LRTEST   CLC   KEY(DBTKMKT-DBTKEY),KEYSAVE        MATCHES UPTO A/M?             
         BE    LR30                                                             
         OC    PREVKEY,PREVKEY                                                  
         BNZ   LRNOMORE                                                         
         NI    BITFLAG,X'FF'-BFPREVKY-BFLSTMON                                  
         XC    PREVKEY,PREVKEY                                                  
         B     LR100X                                                           
*                                                                               
LRNOMORE NI    BITFLAG,X'FF'-BFPREVKY-BFLSTMON                                  
         XC    PREVKEY,PREVKEY                                                  
         B     LR99                                                             
*                                                                               
LR30     TM    FILTFLG1,FFLG1MKT   FILTERING BY MARKET #?                       
         BZ    LR32                                                             
         CLC   DBTKMKT,FLTMKT                                                   
         BNE   LRNOMORE                                                         
*                                                                               
LR32     TM    FILTFLG1,FFLG1STA   FILTERING BY STATION?                        
         BZ    LR34                                                             
         CLC   DBTKSTA,FLTSTA      YES, MATCHES THE STATION?                    
         BE    LR34                                                             
         BL    LR32A                                                            
         XR    R0,R0               NO, BUMP THE MARKET NUMBER                   
         ICM   R0,3,DBTKMKT                                                     
         AHI   R0,1                                                             
         STCM  R0,3,DBTKMKT                                                     
LR32A    MVC   DBTKSTA,FLTSTA          STILL LOOK FOR THAT STATION              
         XC    DBTKCLT(DBTRLEN-DBTKCLT),DBTKCLT   CLEAR REST OF KEY             
         B     LR20                                                             
*                                                                               
LR34     TM    FILTFLG1,FFLG1CLT   FILTERING ON CLIENT?                         
         BZ    LR36                                                             
         CLC   DBTKCLT,FLTCLT      SAME CLIENT?                                 
         BE    LR36                YES, PAST THIS TEST                          
         BL    LR34A                                                            
         XR    R0,R0               NO, BUMP THE STATION                         
         ICM   R0,7,DBTKSTA                                                     
         AHI   R0,1                                                             
         STCM  R0,7,DBTKSTA                                                     
LR34A    MVC   DBTKCLT,FLTCLT          STILL LOOK FOR THAT CLIENT               
         XC    DBTKPRD(DBTRLEN-DBTKPRD),DBTKPRD   CLEAR REST OF KEY             
         B     LR20                                                             
*                                                                               
LR36     TM    FILTFLG1,FFLG1PRD   FILTERING ON PRODUCT?                        
         BZ    LR38                                                             
         CLC   DBTKPRD,SVBPRD      SAME PRODUCT?                                
         BNE   LR36M               NO                                           
         CLC   DBTKPRD2,SVBPR2     YES, MAKE SURE PIGGY MATCHES TOO             
         BE    LR38                YES                                          
         BL    LR36A                                                            
         XR    R0,R0               NO, BUMP THE ESTIMATE                        
         IC    R0,DBTKEST                                                       
         AHI   R0,1                                                             
         STC   R0,DBTKEST                                                       
LR36A    MVC   DBTKPRD2,SVBPR2         STILL LOOK FOR THAT PRODUCT              
         B     LR20                                                             
*                                                                               
LR36M    BL    LR36N                                                            
         XR    R0,R0               NO, BUMP THE CLIENT                          
         ICM   R0,3,DBTKCLT                                                     
         AHI   R0,1                                                             
         STCM  R0,3,DBTKCLT                                                     
LR36N    MVC   DBTKPRD,SVBPRD          STILL LOOK FOR THAT PRODUCT              
         XC    DBTKEST(DBTRLEN-DBTKEST),DBTKEST   CLEAR REST OF KEY             
         B     LR20                                                             
*                                                                               
LR38     TM    FILTFLG1,FFLG1EST   FILTERING ON ESTIMATE?                       
         BZ    LR40                                                             
         CLC   DBTKEST,SVBEST      SAME ESTIMATE?                               
         BE    LR40                YES, PAST THIS TEST                          
         BL    LR38A                                                            
         XR    R0,R0               NO, BUMP THE PRODUCT                         
         ICM   R0,7,DBTKCLT        INCLUDE THE CLIENT IN CASE OF POL            
         AHI   R0,1                    PRODUCT CYCLING                          
         STCM  R0,7,DBTKCLT                                                     
LR38A    MVC   DBTKEST,SVBEST          STILL LOOK FOR THAT ESTIMATE             
         MVI   DBTKPRD2,0              CLEAR REST OF KEY                        
         B     LR20                                                             
***************                                                                 
* PASSED ALL THE KEY FILTERS                                                    
***************                                                                 
LR40     DS    0H                                                               
         MVC   PREVKEY,KEY         DEFINITELY SAVE THE KEY                      
*                                                                               
LR41     L     R2,ATHISLST         R2 = A(CURRENT LIST LINE)                    
         USING DTLLINEH,R2                                                      
         TM    BITFLAG,BFLSTMON    REACHED END WITH LISTMON ALREADY?            
         BZ    *+8                 DON'T OVERWRITE THE PFKEY LINE               
         L     R2,AIO3             TEMPORARY PLACE TO DISPLAY                   
*                                                                               
         GOTO1 CLUNPK,DMCB,DBTKCLT,DTLCLT                                       
         GOTO1 MSUNPK,DMCB,DBTKMKT,DTLMKT,DTLSTA                                
         MVC   FAKEFLDH,DTLLINEH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(3),DTLCLT                                                
         MVI   FAKEFLDH+5,3                                                     
         CLI   DTLCLT+2,C' '                                                    
         BH    *+8                                                              
         MVI   FAKEFLDH+5,2                                                     
*                                                                               
         LA    R2,FAKEFLDH         <===  CHANGING R2                            
         GOTO1 VALICLT             <===  MESSES UP KEY & AIO                    
         L     R1,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),PREVKEY                                            
         GOTO1 READ                                                             
*                                                                               
         L     R2,ATHISLST         R2 = A(CURRENT LIST LINE)                    
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,DBTKCLT),DTLCLT                           
         TM    BITFLAG,BFLSTMON                                                 
         BZ    *+8                                                              
         LA    R2,FAKEFLDH         DON'T OVERWRITE THE PFKEY LINE               
*                                                                               
         LA    R1,DTLPRD                                                        
         MVC   BYTE,DBTKPRD                                                     
         BAS   RE,GTQPRD                                                        
         CLI   DBTKPRD2,0                                                       
         BE    LR45                                                             
         MVI   DTLPRD+3,C'-'                                                    
         LA    R1,DTLPRD+4                                                      
         MVC   BYTE,DBTKPRD2                                                    
         BAS   RE,GTQPRD                                                        
*                                                                               
LR45     EDIT  (B1,DBTKEST),(3,DTLEST),FILL=0                                   
         MVC   BEST,DBTKEST                                                     
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         NI    BITFLAG,X'FF'-BFFLGHTS                                           
         XC    FLTNONFL(L'FLTNONFL*3),FLTNONFL                                  
         BAS   RE,GETFLTRC    <<=====   MESSES WITH KEY SEQUENCING              
         BNE   *+8                                                              
         OI    BITFLAG,BFFLGHTS                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),PREVKEY                                            
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'       PASSED ALL TESTS                             
         GOTO1 GETREC              GET THE RECORD NOW                           
*                                                                               
         TM    BITFLAG,BFFLGHTS    ARE WE USING FLIGHTS?                        
         BNZ   LR45B               YES                                          
         TM    FILTFLG1,FFLG1FLT   NO, BUT ARE WE FILTERING BY FLIGHT?          
         BZ    LR45Z                   NO, WE'RE OKAY                           
LR45A    XC    8(L'MKTDTL1,R2),8(R2)                                            
         B     LR67                    YES, DONE WITH THIS RECORD               
*                                                                               
LR45B    TM    FILTFLG1,FFLG1FLT   ARE WE FILTERING BY FLIGHT #?                
         BZ    LR45C                                                            
         CLC   CURRFLTN,SVFLTNUM                                                
         BL    LR67                NEXT FLIGHT NUMBER                           
         BH    LR45A               DONE WITH THIS MED/CLT/PRD/EST/...           
LR45C    ZIC   R1,CURRFLTN                                                      
         CVD   R1,DUB                                                           
         UNPK  DTLFLT(2),DUB                                                    
         OI    DTLFLT+1,X'F0'                                                   
***************                                                                 
* AIO3 HAS OUR FLIGHT RECORD                                                    
***************                                                                 
         L     R6,AIO3             GET THE DATES FOR THE FLIGHT                 
         USING DFLKEY,R6                                                        
         XR    R0,R0                                                            
         LA    R6,DFLEL                                                         
LR45E    CLI   0(R6),0                                                          
         BE    LR45Z                                                            
         CLI   0(R6),DFINFELQ      INFO ELEM (X'01')                            
         BNE   LR45H                                                            
         USING DFINFEL,R6                                                       
         MVC   FLTNONFL,DFINFSDT                                                
         CLI   CURRFLTN,0          NON-FLIGHTED FLIGHT NUMBER?                  
         BE    LR45Z               YES, NO MORE FOR THIS NUMBER                 
         B     LR45M                                                            
*                                                                               
LR45H    CLI   0(R6),DFFLTELQ      FLIGHT ELEM (X'05')                          
         BNE   LR45M                                                            
         USING DFFLTEL,R6                                                       
         CLC   DFFLTNUM,CURRFLTN                                                
         BNE   LR45M                                                            
         MVC   FLTSDATE,DFFLTSTR                                                
         MVC   FLTEDATE,DFFLTEND                                                
         B     LR45Z                                                            
*                                                                               
LR45M    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LR45E                                                            
         DROP  R6                                                               
*                                                                               
LR45Z    L     R6,AIO                                                           
         USING DBTKEY,R6                                                        
         XR    R0,R0                                                            
         LA    R6,DBTRFRST                                                      
LR50     CLI   0(R6),0             LOOK FOR FLIGHT ELEMENTS                     
         BE    LR60                                                             
         CLI   0(R6),DBFLTELQ                                                   
         BE    LR55                                                             
LR50LP   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LR50                                                             
*                                                                               
         USING DBFLTELD,R6                                                      
LR55     CLC   CURRFLTN,DBFLTFLT   EVEN NON-FLIGHTED WILL HAVE ELEM             
         BNE   LR50LP                                                           
         TM    DBFLTFL1,DBFLTSNT   ALREADY SENT?                                
         BNZ   LR45A                                                            
         TM    FILTFLG1,FFLG1FLT   YES, ARE WE FILTERING BY FLIGHT?             
         BZ    *+14                     NO                                      
         CLC   DBFLTFLT,SVFLTNUM        MAKE SURE WE HAVE A MATCH               
         BNE   LR65                     NO MATCH, CHECK NEXT RECORD             
*                                                                               
LR55A    TM    DBFLTFL1,DBFLTDAR    DO WE NEED TO GET DAR COMMENT?              
         BNZ   LR50X                                                            
         XR    R4,R4               SHOW THE COMMENT                             
         IC    R4,DBFLTLEN                                                      
         SHI   R4,DBFLTOVH                                                      
         BNP   LR60                IF NOT POSITIVE, THEN NO COMMENT             
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     LR60                                                             
         MVC   DTLSCM(0),DBFLTTXT                                               
         DROP  R6                                                               
*                                                                               
LR50X    MVI   DTLASTRX,C'*'                                                    
         TM    BITFLAG,BFLSTMON    REACHED END WITH LISTMON ALREADY?            
         BNZ   LR60                DON'T OVERWRITE THE PFKEY LINE               
         GOTO1 GTDARCMT,PREVKEY    PUT DARE COMMENT INTO WORK                   
         BE    *+12                                                             
         MVI   DTLASTRX,C' '                                                    
         B     *+10                                                             
         MVC   DTLSCM,WORK                                                      
*                                                                               
         XC    KEY,KEY             RE-ESTABLISH OUR BATCH KEY                   
         MVC   KEY(L'DBTKEY),PREVKEY                                            
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
LR60     TM    BITFLAG,BFLSTMON    REACHED END WITH LISTMON ALREADY?            
         BNZ   LR61A               DON'T OVERWRITE THE PFKEY LINE               
*                                                                               
         OI    DTLSCMH+4,X'20'     SO IT WON'T VALIDATE AGAIN                   
         L     R1,ALSTKEYS                                                      
         USING LSTTABLD,R1                                                      
         MVC   LSTBTKEY,PREVKEY                                                 
         MVC   LSTFLTNM,CURRFLTN                                                
         TM    BITFLAG,BFFLGHTS    ARE WE USING FLIGHTS?                        
         BZ    LR61                                                             
         CLI   CURRFLTN,0          NON-FLIGHTED?                                
         BNE   LR60B                                                            
         OC    FLTNONFL,FLTNONFL   ANY NONFLIGHT END DATE?                      
         BZ    LR45A               NONE, NOTHING TO SHOW FOR FLIGHT 0           
         MVC   LSTFLTED,FLTNONFL   YES, LAST DATE ON NONFLIGHT PERIOD           
         B     LR61                                                             
*                                                                               
LR60B    MVC   LSTFLTSD,FLTSDATE   NO, START AND END DATE OF FLIGHT             
         MVC   LSTFLTED,FLTEDATE                                                
         DROP  R1                                                               
LR61     LA    R1,L'LISTTABL(R1)                                                
         ST    R1,ALSTKEYS                                                      
*                                                                               
LR61A    TM    BITFLAG,BFLSTMON    DID WE ALREADY REACH END W/ LISTMON?         
         BZ    LR61B                                                            
         MVC   PREVKEY,KEY                                                      
         OI    BITFLAG,BFPREVKY                                                 
         B     LR100               TIME TO DO THE TOTALS                        
*                                                                               
LR61B    GOTO1 LISTMON             PUT LINE OUT ONTO THE LIST SCREEN            
         BE    LR65                WE STILL HAVE MORE LINES LEFT                
         TM    BITFLAG,BFFLGHTS    DO WE HAVE FLIGHTS?                          
         BZ    LR62                                                             
         CLC   CURRFLTN,HIFLTNUM                                                
         BNL   LR62                                                             
         ZIC   RE,CURRFLTN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,CURRFLTN                                                      
         MVC   PREVKEY,KEY                                                      
         OI    BITFLAG,BFPREVKY+BFLSTMON                                        
         B     LR41                TIME TO DO THE TOTALS                        
*                                                                               
LR62     GOTO1 SEQ                 NEXT PAGE STARTS WITH THE NEXT KEY           
         MVC   PREVKEY,KEY                                                      
         OI    BITFLAG,BFPREVKY+BFLSTMON                                        
         B     LRTEST                                                           
*                                                                               
LR65     TM    BITFLAG,BFFLGHTS    DO WE HAVE FLIGHTS?                          
         BZ    LR70                                                             
LR67     CLC   CURRFLTN,HIFLTNUM                                                
         BNL   LR70                                                             
         ZIC   RE,CURRFLTN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,CURRFLTN                                                      
         B     LR41                YES, WE NEED TO DO ALL THE FLIGHTS           
*                                                                               
LR70     GOTO1 SEQ                 YES, GET NEXT DARE BATCH KEY                 
         NI    BITFLAG,X'FF'-BFFLGHTS   NOT USING FLIGHTS NOW                   
         MVI   CURRFLTN,0                                                       
         LA    R4,KEY                                                           
         B     LRTEST                                                           
         DROP  R2                                                               
*                                                                               
LR99     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* TIME TO GO THROUGH THE BUYS MATCHING THE KEYS IN LISTTABL SO THAT WE          
* CAN GET THE CORRECT NUMBER OF SPOTS AND DOLLARS                               
***********************************************************************         
LR100    LA    R2,LISTTABL                                                      
         USING LSTTABLD,R2                                                      
LR100A   LA    R0,LISTTBLX                                                      
         CR    R2,R0               DID WE GO THRU ALL THE KEYS?                 
         BNL   LR190               YES                                          
*                                                                               
         OC    0(LSTTBLLN,R2),0(R2)                                             
         BZ    LR190                                                            
*                                                                               
         TM    LSTFLTNM,X'80'      DID WE READ FOR THIS ALREADY?                
         BNZ   LR100LP             YES, GO GET NEXT KEY IN TABLE                
*                                                                               
LR100DS1 USING DBTKEY,LSTBTKEY                                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BUYKEY,R6                                                        
         MVC   BUYKAM,LR100DS1.DBTKAGMD                                         
         MVC   BUYKCLT,LR100DS1.DBTKCLT                                         
         MVC   BUYKPRD,LR100DS1.DBTKPRD                                         
         MVC   BUYMSTA,LR100DS1.DBTKMKT                                         
         MVC   BUYKEST,LR100DS1.DBTKEST                                         
         DROP  LR100DS1                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE                                      
         BNE   LR100LP                                                          
*                                                                               
         CLI   BUYKBUY,X'FF'       POL BUYING?                                  
         BNE   LR110                                                            
*                                                                               
         XC    BUYKBUY,BUYKBUY                                                  
         MVI   BUYKPRD,X'FF'       YES, GET ALL THE POL BUYLINES THEN           
         GOTO1 HIGH                                                             
LR100M   CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE                                      
         BNE   LR100LP                                                          
*                                                                               
LR110    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   BUYKPRD,X'FF'       POL BUY?                                     
         BNE   LR110REG            REGULAR BUYLINES                             
***************                                                                 
* POL BUY                                                                       
***************                                                                 
LR110POL LA    R6,BDELEM                                                        
LR110P00 CLI   0(R6),0             NO MORE POOL ORIGINAL ELEM?                  
         BE    LR100SEQ                                                         
*                                                                               
         CLI   0(R6),X'0B'         POOL ORIGINAL ELEM                           
         BE    *+12                                                             
         CLI   0(R6),X'0C'                                                      
         BNE   LR110P90                                                         
         USING REGELEM,R6                                                       
         TM    RSTATUS,X'C0'       DON'T WANT MINUSED SPOTS                     
         BNZ   LR110P90                                                         
*****                                                                           
* CHECK AGAINST OUR LIST TABLE                                                  
*****                                                                           
         L     R4,AIO                                                           
         USING BUYKEY,R4                                                        
         LR    R3,R2               ALL ENTRIES ABOVE R2 SHOULD BE READ          
LR100DS2 USING LSTTABLD,R3             ALREADY                                  
LR110P10 TM    LR100DS2.LSTFLTNM,X'80'  READ FOR THIS ONE ALREADY?              
         BNZ   LR110P80                 YES, NO NEED TO TOTAL FOR THIS          
*                                                                               
LR110DBT USING DBTKEY,LR100DS2.LSTBTKEY                                         
         CLC   BUYMSTA,LR110DBT.DBTKMKT  MARKET/STATION MATCHES?                
         BNE   LR110P80                                                         
         CLC   BUYKCLT,LR110DBT.DBTKCLT     CLIENT MATCHES?                     
         BNE   LR110P80                                                         
         CLC   BUYKEST,LR110DBT.DBTKEST     ESTIMATE MATCHES?                   
         BNE   LR110P80            NO, CHECK NEXT LIST TABLE ENTRY?             
         DROP  R4                                                               
*                                                                               
         CLI   RLEN,RPALLOC-REGELEM   ANY PRODUCT ALLOCATION?                   
         BH    LR110P20               YES                                       
*********                                                                       
* PRODUCT NOT ALLOCATED                                                         
*********                                                                       
         CLI   LR110DBT.DBTKPRD,X'FF'  POL BATCH RECORD?                        
         BNE   LR110P80                NO                                       
         B     LR110P30                                                         
*********                                                                       
* PRODUCT WAS ALLOCATED                                                         
*********                                                                       
LR110P20 CLI   LR110DBT.DBTKPRD,X'FF'  POL BATCH RECORD?                        
         BE    LR110P30                YES, SHOULD COUNT THIS TOO               
         CLC   LR110DBT.DBTKPRD,RPPRD  MATCHES AGAINST THE ALLOCATION?          
         BE    LR110P25                                                         
         CLI   RLEN,RPALLOC-REGELEM+L'RPALLOC   ANY PB ALLOCATION?              
         BNH   LR110P80                         NONE                            
         CLI   LR110DBT.DBTKPRD2,0              YES, THIS ENTRY ALSO?           
         BE    LR110P80                              NO                         
         CLC   LR110DBT.DBTKPRD,RPPRD+L'RPALLOC MATCH AGAINST PB?               
         BNE   LR110P80                                                         
         CLC   LR110DBT.DBTKPRD2,RPPRD          YES, NTRY PB MATCH PRD?         
         BNE   LR110P80                                                         
         B     LR110P30            WE HAVE A REVERSE PR2-PRD                    
*****                                                                           
* FIRST PRODUCT MATCHES WITH ENTRY'S FIRST PRD                                  
*****                                                                           
LR110P25 CLI   RLEN,RPALLOC-REGELEM+L'RPALLOC   ANY PB ALLOCATION?              
         BH    LR110P26                         YES                             
         CLI   LR110DBT.DBTKPRD2,0              NO, NONE FOR NTRY ALSO?         
         BNE   LR110P80                                                         
         B     LR110P30                                                         
*                                                                               
LR110P26 CLI   LR110DBT.DBTKPRD2,0              YES, THIS ENTRY ALSO?           
         BE    LR110P80                              NO                         
         CLC   LR110DBT.DBTKPRD2,RPPRD+L'RPALLOC MATCH AGAINST PB?              
         BNE   LR110P80                                                         
*********                                                                       
* ANY FLIGHTS?                                                                  
*********                                                                       
LR110P30 OC    LR100DS2.LSTFLTSD(L'LSTFLTSD*2),LR100DS2.LSTFLTSD                
         BZ    LR110P70            NO, ADD TO THIS ENTRY'S TOTALS               
         GOTO1 DATCON,DMCB,(2,RDATE),(3,FULL)                                   
         CLC   FULL(3),LR100DS2.LSTFLTSD   IN OUR FLIGHT?                       
         BL    LR110P80                                                         
         CLC   FULL(3),LR100DS2.LSTFLTED                                        
         BH    LR110P80                    NO                                   
*********                                                                       
* TOTAL THEM UP                                                                 
*********                                                                       
LR110P70 XR    R1,R1               TOTAL UP THE NUMBER OF SPOTS                 
         ICM   R1,3,LR100DS2.LSTNSPTS                                           
         LA    R0,1                                                             
         AR    R1,R0                                                            
         STCM  R1,3,LR100DS2.LSTNSPTS                                           
*                                                                               
         GOTO1 GETRATE,DMCB,(LR110DBT.DBTKPRD,WORK),(0,AIO),(0,(R6)),  X        
               (SVAPROF+7,0)                                                    
         MVC   TEMPCOST,WORK+4                                                  
         ICM   R1,15,TEMPCOST                                                   
         CLI   LR110DBT.DBTKPRD2,0 ANY PIGGYBACK COST?                          
         BE    LR110P75                                                         
         GOTO1 GETRATE,DMCB,(LR110DBT.DBTKPRD,WORK),(0,AIO),(0,(R6)),  X        
               (SVAPROF+7,0)                                                    
         L     R1,WORK+4                                                        
         ICM   R0,15,TEMPCOST                                                   
         AR    R1,R0                                                            
         STCM  R1,15,TEMPCOST                                                   
LR110P75 ICM   R0,15,LR100DS2.LSTTDOLR                                          
         AR    R1,R0                                                            
         STCM  R1,15,LR100DS2.LSTTDOLR                                          
         B     LR110P90            NEXT SPOT ELEMENT                            
*                                                                               
LR110P80 AHI   R3,LSTTBLLN         SHOULD TRY TO MATCH THE SPOT AGAINST         
         LA    R0,LISTTBLX            ALL OUR KEYS                              
         CR    R3,R0               DID WE?                                      
         BL    LR110P10            NO, MIGHT BE ABLE TO MATCH THIS ELEM         
*                                                                               
LR110P90 ZIC   R0,1(R6)            NEXT SPOT ELEMENT                            
         AR    R6,R0                                                            
         B     LR110P00                                                         
***************                                                                 
* REGULAR BUY                                                                   
***************                                                                 
         USING BUYKEY,R6                                                        
LR110REG LA    R6,BDELEM                                                        
LR110R00 CLI   0(R6),0             NO MORE ORIGINAL ELEM?                       
         BE    LR100SEQ                                                         
*                                                                               
         CLI   0(R6),X'06'         POOL ORIGINAL ELEM                           
         BE    *+12                                                             
         CLI   0(R6),X'07'                                                      
         BNE   LR110R90                                                         
         USING REGELEM,R6                                                       
         TM    RSTATUS,X'C0'       DON'T WANT MINUSED SPOTS                     
         BNZ   LR110R90                                                         
*****                                                                           
* CHECK AGAINST OUR LIST TABLE                                                  
*****                                                                           
         L     R4,AIO                                                           
         USING BUYKEY,R4                                                        
         LR    R3,R2               ALL ENTRIES ABOVE R2 SHOULD BE READ          
********************************************************                        
***  LR100DS2 USING LSTTABLD,R3    THIS WAS DONE ABOVE                          
********************************************************                        
LR110R10 TM    LR100DS2.LSTFLTNM,X'80'  READ FOR THIS ONE ALREADY?              
         BNZ   LR110R80                 YES, NO NEED TO TOTAL FOR THIS          
********************************************************                        
*** LR110DBT USING DBTKEY,LR100DS2.LSTBTKEY   THIS WAS DONE ABOVE               
********************************************************                        
         CLC   BUYMSTA,LR110DBT.DBTKMKT  MARKET/STATION MATCHES?                
         BNE   LR110R80                                                         
         CLC   BUYKCLT,LR110DBT.DBTKCLT     CLIENT MATCHES?                     
         BNE   LR110R80                                                         
         CLC   BUYKPRD,LR110DBT.DBTKPRD     PRODUCT MATCHES?                    
         BNE   LR110R80                                                         
         CLC   BUYKEST,LR110DBT.DBTKEST     ESTIMATE MATCHES?                   
         BNE   LR110R80            NO, CHECK NEXT LIST TABLE ENTRY?             
         DROP  R4                                                               
*********                                                                       
* ANY FLIGHTS?                                                                  
*********                                                                       
LR110R30 OC    LR100DS2.LSTFLTSD(L'LSTFLTSD*2),LR100DS2.LSTFLTSD                
         BZ    LR110R70            NO, ADD TO THIS ENTRY'S TOTALS               
         GOTO1 DATCON,DMCB,(2,RDATE),(3,FULL)                                   
         CLC   FULL(3),LR100DS2.LSTFLTSD   IN OUR FLIGHT?                       
         BL    LR110R80                                                         
         CLC   FULL(3),LR100DS2.LSTFLTED                                        
         BH    LR110R80                    NO                                   
*********                                                                       
* TOTAL THEM UP                                                                 
*********                                                                       
LR110R70 XR    R1,R1               TOTAL UP THE NUMBER OF SPOTS                 
         ICM   R1,3,LR100DS2.LSTNSPTS                                           
         XR    R0,R0                                                            
         IC    R0,RNUM                                                          
         AR    R1,R0                                                            
         STCM  R1,3,LR100DS2.LSTNSPTS                                           
*                                                                               
         GOTO1 GETRATE,DMCB,(LR110DBT.DBTKPRD,WORK),(0,AIO),(0,(R6)),  X        
               (SVAPROF+7,0)                                                    
         MVC   TEMPCOST,WORK+4                                                  
         ICM   R1,15,TEMPCOST                                                   
*                                                                               
LR110R75 ICM   R0,15,LR100DS2.LSTTDOLR                                          
         AR    R1,R0                                                            
         STCM  R1,15,LR100DS2.LSTTDOLR                                          
         B     LR110R90            NEXT SPOT ELEMENT                            
*                                                                               
LR110R80 AHI   R3,LSTTBLLN         SHOULD TRY TO MATCH THE SPOT AGAINST         
         LA    R0,LISTTBLX            ALL OUR KEYS                              
         CR    R3,R0               DID WE?                                      
         BL    LR110R10            NO, MIGHT BE ABLE TO MATCH THIS ELEM         
*                                                                               
LR110R90 ZIC   R0,1(R6)            NEXT SPOT ELEMENT                            
         AR    R6,R0                                                            
         B     LR110R00                                                         
         DROP  R6,LR110DBT,LR100DS2                                             
*****************                                                               
LR100SEQ GOTO1 SEQ                 GO THROUGH ALL THE BUYLINES                  
         B     LR100M                                                           
*                                                                               
LR100LP  OI    LSTFLTNM,X'80'      MARK THIS ENTRY AS READ ALREADY              
         LR    R1,R2                                                            
         LA    R0,LISTTBLX                                                      
LR100LP1 CR    R2,R0                                                            
         BNL   LR100LP2                                                         
         OC    LSTNSPTS(L'LSTNSPTS+L'LSTTDOLR),LSTNSPTS                         
         BZ    *+8                                                              
         OI    LSTFLTNM,X'80'      MARK THOSE THAT HAVE SPOTS AND $$            
         CLC   LSTBTKEY,0(R1)      SAME KEY (DUPLICATED FOR FLIGHTS)            
         BNE   *+8                                                              
         OI    LSTFLTNM,X'80'      MARK THOSE WITH SAME BASE KEY                
         AHI   R2,LSTTBLLN             AS READ ALREADY                          
         B     LR100LP1                                                         
*                                                                               
LR100LP2 LR    R2,R1                                                            
         AHI   R2,LSTTBLLN         GO THRU ALL BUYS FOR THOSE KEYS              
         B     LR100A                                                           
***********************************                                             
* TIME TO SHOW THE NUMBERS                                                      
***********************************                                             
LR190    LA    R2,LISTTABL                                                      
         USING LSTTABLD,R2                                                      
         LA    R3,MKTSEL1H                                                      
         USING LINDSECT,R3                                                      
*                                                                               
LR191    OC    0(LSTTBLLN,R2),0(R2)  ANY LIST ENTRY?                            
         BZ    LR100X                NO, WE'RE DONE WITH ALL THE #'S            
         OC    LSTNSPTS(L'LSTNSPTS+L'LSTTDOLR),LSTNSPTS                         
         BZ    LR199                                                            
LR190D   USING DTLLINED,LINDTLH                                                 
         EDIT  (B2,LSTNSPTS),(4,LR190D.DTLSPTS),ALIGN=LEFT                      
         EDIT  (B4,LSTTDOLR),(10,LR190D.DTLDOLR),2                              
         DROP  LR190D                                                           
*                                                                               
LR199    LA    R3,LINNEXTL                                                      
         AHI   R2,LSTTBLLN                                                      
         B     LR191                                                            
         DROP  R2,R3                                                            
*                                                                               
LR100X   TM    BITFLAG,BFLSTMON    DONE WITH LIST BECAUSE OF LISTMON?           
         BNZ   LR100X1             YES                                          
         LA    R2,MKTSEL1H                                                      
         TM    BITFLAG,BFPREVKY    ARE WE GOING TO CONTINUE THE LIST?           
         BZ    EOFLSELS            NO, END OF LIST - INPUT CHANGES OR .         
         B     MAKESELS            YES, LIST DISPLAYED - INPUT CHANGES.         
* THE FOLLOWING LISTMON WILL EXIT FROM THIS OVERLAY                             
LR100X1  GOTO1 LISTMON                                                          
*                                                                               
LRXIT    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIGURES OUT THE EBCDIC EQUIVALENT FOR THE BINARY PRODUCT CODE                 
*                                                                               
* ON ENTRY:    BYTE                CONTAINS THE BINARY PRODUCT CODE             
*              (R1)                A(WHERE TO PUT EBCDIC PRODUCT CODE)          
***********************************************************************         
GTQPRD   NTR1                                                                   
         LA    RE,SVCLIST                                                       
         LA    RF,SVUSER                                                        
GQPRD10  OC    0(4,RE),0(RE)                                                    
         BZ    GQPRD15                                                          
         CLC   3(1,RE),BYTE                                                     
         BE    GQPRD20                                                          
         LA    RE,4(RE)                                                         
         CR    RE,RF                                                            
         BL    GQPRD10                                                          
GQPRD15  MVC   0(3,R1),=C'???'     UNKNOWN PRODUCT                              
         B     GQPRDX                                                           
*                                                                               
GQPRD20  MVC   0(3,R1),0(RE)                                                    
GQPRDX   B     XIT                                                              
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPF10                                                           
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,MKTSEL1H         CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFND                                                         
         LA    R0,MKTPFLNH                                                      
         CR    R2,R0                                                            
         BNL   RECNTFND                                                         
*                                                                               
STPF10   LA    R2,PFTABLE          YES, USE LIST PFKEY TABLE                    
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
         CLI   PFKEY,0             PFKEYING SOMEWHERE?                          
         BNE   STPF20              YES                                          
         TM    BITFLAG,BFXMITED    DID WE TRANSMIT SOMEWHERE?                   
         BZ    STPFX               NO, CONTINUE WE REGULAR PROCESSING           
         NI    BITFLAG,X'FF'-BFXMITED                                           
         B     STPFX                                                            
*                                                                               
STPF20   ZIC   R0,PFKEY                                                         
         AH    R0,=H'12'                                                        
         STC   R0,PFKEY                                                         
*                                                                               
         LH    RE,CURDISP          IF PFKEY IS PRESSED, CURDISP IS              
         AR    RE,RA                   DISP OF THE CURSOR, BUT IF A SEL         
         XR    R1,R1                   CODE WAS USED THEN CURDISP IS            
         ICM   R1,3,2(RE)              DISP TO THE SELECT FIELD                 
         XR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         L     RE,AFRSTREC         FIRST SELECT FIELD                           
         XR    RF,RF                                                            
         ICM   RF,3,2(RE)                                                       
         XR    RE,RE                                                            
         D     RE,=F'80'           RF = ROW NUMBER MINUS 1 OF 1ST REC           
         SR    R1,RF                                                            
         STC   R1,PFKEYLIN         DISP FROM FIRST SELECT LINE                  
*                                                                               
         MHI   R1,LINNEXTL-LINDSECT                                             
         L     RE,AFRSTREC                                                      
         AR    RE,R1                                                            
         LR    R0,RE                                                            
         SR    R0,RA                                                            
         STH   R0,CURDISP                                                       
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         USING LINDSECT,R2                                                      
         LA    R2,LINDTLH                                                       
         USING DTLLINED,R2                                                      
         XC    PASSPRD,PASSPRD                                                  
         MVC   PASSPRD,DTLPRD                                                   
         CLC   =C'POL',DTLPRD                                                   
         BNE   STPF25                                                           
         XC    PASSPRD,PASSPRD                                                  
         MVC   PASSPRD(3),=C'***'                                               
*                                                                               
STPF25   MVI   XMITCTB,C'B'        ASSUME USER WANTS BOTH TYPES                 
         XC    ESTWFLT,ESTWFLT                                                  
         MVC   ESTWFLT(L'DTLEST),DTLEST                                         
*                                                                               
         TM    DTLSCMH+4,X'20'     DID THIS FIELD CHANGE?                       
         BNZ   STPF30              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,PFKEYLIN                                                      
         LA    RE,LISTDIR                                                       
         CLI   PFKEYLIN,0                                                       
         BE    *+12                                                             
         LA    RE,6(RE)            BUMP TO NEXT ENTRY IN D/A TABLE              
         BCT   RF,*-4                                                           
*                                                                               
         MVC   KEY+14(4),2(RE)     DISK ADDRESS                                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   GLOBDA,DMDSKADD                                                  
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)                                                    
*                                                                               
         BAS   RE,VALLINE                                                       
*                                                                               
         GOTO1 PUTREC                                                           
         OI    DTLSCMH+4,X'20'     SET VALIDATED                                
         OI    DTLSCMH+6,X'80'                                                  
*                                                                               
STPF30   CLC   DTLFLT,SPACES       NOTHING OR NULL VALUE IN FLIGHT FLD?         
         BNH   STPF40                                                           
         MVI   ESTWFLT+L'DTLEST,C'/'                                            
         MVC   ESTWFLT+L'DTLEST+1(L'DTLFLT),DTLFLT                              
*                                                                               
STPF40   CLI   PFKEY,18            ACTUAL PFKEY TO TRANSMIT?                    
         BNE   *+8                 NO                                           
         OI    BITFLAG,BFXMITED    TRIED TO TRANSMIT AN ORDER                   
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,CURDISP                                                     
         AR    RE,RA                                                            
         USING LINDSECT,RE                                                      
         CLI   LINSCMH+5,0                ANY STATION COMMENT?                  
         BE    STPF50                     NONE TO PASS                          
         CLI   DTLASTRX,C'*'       WE HAD A COMMENT FROM THE ORDER?             
         BE    STPF50              YES, DON'T PASS THIS TO ORDER/SEND           
         DROP  R2                                                               
*                                                                               
         L     RF,ACOMFACS                PASS STATION COMMENT TO               
         L     RF,CGLOBBER-COMFACSD(RF)      SPOMS09                            
         LA    R1,LINSCMH                                                       
         ST    R1,DMCB+4                                                        
         GOTO1 (RF),DMCB,=C'PUTF',,,GLVSMSG                                     
         DROP  RE                                                               
*                                                                               
STPF50   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1TSELQ   DON'T TEST THE SEL CODES IN TESTSEL          
         GOTO1 INITIAL,DMCB,(R2)   2ND PASS                                     
*                                                                               
STPFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE FLIGHT RECORD                                                         
*                                                                               
* ON EXIT:     (CC)                EQ = FLIGHT EXISTS FOR THE EST               
*                                  NE = NO FLIGHT RECORD EXISTS                 
***********************************************************************         
GETFLTRC NTR1                                                                   
         MVI   HIFLTNUM,0          HIGHEST FLIGHT NUMBER                        
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R4,KEY                 POL ON THE ESTIMATE                       
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'POL'                                                  
         MVC   DFLKEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DFLKEY),KEYSAVE    FLIGHT REC EXISTS FOR POL EST?          
         BE    GFLT10                   YES                                     
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R4,KEY                 PRIMARY PRODUCT ON THE ESTIMATE           
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,QPRD                                                     
         MVC   DFLKEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DFLKEY),KEYSAVE    FLIGHT REC EXISTS FOR PRD/EST?          
         BE    GFLT10                   YES                                     
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC FOR            
         LA    R4,KEY                 'ALL' PRODUCTS ON THIS ESTIMATE           
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'ALL'                                                  
         MVC   DFLKEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DFLKEY),KEYSAVE    FLIGHT REC EXISTS FOR EST?              
         BNE   GFLTNO                   NO                                      
*                                                                               
GFLT10   GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   GFLT20                                                           
         USING DFFLTEL,R6                                                       
GFLT15   MVC   HIFLTNUM,DFFLTNUM                                                
         BAS   RE,NEXTEL                                                        
         BE    GFLT15                                                           
         DROP  R6                                                               
*                                                                               
GFLT20   CLI   HIFLTNUM,0          SHOULD HAVE AT LEAST ONE FLIGHT              
         BE    GFLTNO              OTHERWISE, NO FLIGHT                         
*                                                                               
GFLTYES  B     YES                                                              
*                                                                               
GFLTNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GET THE DARE COMMENT RECORD                                                   
*                                                                               
* ON ENTRY:    (R1)                KEY FOR THE BATCH RECORD                     
*              AIO                 A(BATCH RECORD)                              
*              (R2)                DETAIL LINE                                  
*                                                                               
* ON EXIT:     WORK                1ST COMMENT LINE IN DARE COMMENT REC         
*              AIO                 A(COMMENT RECORD)                            
***********************************************************************         
GTDARCMT NTR1                                                                   
         MVC   WORK,SPACES                                                      
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R4,KEY                 POL ON THE ESTIMATE                       
         USING DOKEY,R4                                                         
GDCMPKEY USING DBTKEY,R1                                                        
*                                                                               
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,GDCMPKEY.DBTKAGMD                                        
         MVC   DCKCLT,GDCMPKEY.DBTKCLT                                          
         MVC   DCKPRD,GDCMPKEY.DBTKPRD                                          
         MVC   DCKEST,GDCMPKEY.DBTKEST                                          
         MVC   DCKSTA,GDCMPKEY.DBTKSTA                                          
         MVC   DCKPRD2,GDCMPKEY.DBTKPRD2                                        
         DROP  GDCMPKEY                                                         
*                                                                               
         TM    BITFLAG,BFFLGHTS    WILL WE HAVE FLIGHTS?                        
         BZ    GDCMHIGH                                                         
         MVC   DCKFLTNM,CURRFLTN                                                
*                                                                               
GDCMHIGH GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE   GOT THE PROPER DARE RECORD?               
         BNE   GDCMNO                 NO                                        
*                                                                               
         MVC   ORDERKEY,KEY        SAVE THE DARE ORDER KEY                      
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                 PRIMARY PRODUCT ON THE ESTIMATE           
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   DOKCMT,1               GET AGENCY COMMENT RECORD                 
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE   COMMENT RECORD EXISTS?                    
         BNE   GDCMNO                 NO                                        
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DOCOMELQ     COMMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    GDCM10              GOT A COMMENT ELEMENT                        
         USING DTLLINED,R2                                                      
         MVI   DTLASTRX,C' '       GOT A COMMENT RECORD W/O COMMENTS            
         B     GDCMYES                                                          
         DROP  R2                                                               
*                                                                               
         USING DOCOMELD,R6                                                      
GDCM10   CLI   DOCOMLIN,1          FIRST COMMENT NOT ON FIRST LINE              
         BNE   GDCMYES                                                          
*                                                                               
         XR    RE,RE               RE = L(TEXT)-1                               
         IC    RE,DOCOMLEN                                                      
         SHI   RE,DOCOMOVH+1                                                    
         CHI   RE,L'MKTSCM1-1      MORE THAN WE CAN FIT IN THE FIELD?           
         BNH   *+14                                                             
         MVC   WORK(L'MKTSCM1),DOCOMTXT    YES, JUST FIT WHAT WE CAN            
         B     GDCMYES                                                          
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DOCOMTXT                                                 
*                                                                               
GDCMYES  B     YES                                                              
*                                                                               
GDCMNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         B     ERREXIT                                                          
*                                                                               
INVLDEST MVI   GERROR1,INVDEST                                                  
         B     ERREXIT                                                          
*                                                                               
RECXISTS MVI   GERROR1,RECEXIST    RECORD ALREADY EXISTS                        
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 2)                                                     
***********************************************************************         
NOPOLPRD MVI   GERROR1,CNTBEPOL    PRODUCT CAN'T BE POL                         
         B     ER2EXIT                                                          
*                                                                               
NODATASN MVC   GERROR,=Y(NODATA)   NO DATA TO SEND                              
         B     ER2EXIT                                                          
*                                                                               
ER2EXIT  MVI   GETMSYS,2           SPOT MESSAGES                                
         B     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
CANTSNDU MVI   GERROR1,UNDRORDR    CAN'T SEND UNDARED/NODARED ORDER             
         B     ERREXIT                                                          
*                                                                               
CANTWAIT MVI   GERROR1,WAITRESP    ORDER SENT ALRDY,WAITING FOR RESPONS         
         B     ERREXIT                                                          
*                                                                               
CANTSNDC MVI   GERROR1,CONFORDR    CAN'T SEND THIS CONFIRMED ORDER              
         B     ERREXIT                                                          
*                                                                               
NEEDRJCT MVI   GERROR1,GETRJCTD    ORDER HAS TO BE REJECTED FIRST BY...         
         B     ERREXIT                                                          
*                                                                               
NTDARAGY MVI   GERROR1,NOTDARAG    AGENCY IS NOT A VALID DARE TRADING..         
         B     ERREXIT                                                          
***********************************************************************         
* ERRORS WITH REPLACEMENT TEXT (&1)                                             
***********************************************************************         
BYRALRDY MVI   GERROR1,GOTBYRAL    BUYER &1 ALREADY ASSIGNED TO THIS ..         
         B     ERRRTEXT                                                         
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
*                                                                               
NTRCHNGS MVI   GERROR1,RCDSPCHA    RECORD DISPLAYED - NOW ENTER CHANGES         
         B     INFEXIT                                                          
*                                                                               
RECCHNGD MVI   GERROR1,RCWASCHA    RECORD WAS CHANGED - ENTER NEXT RE..         
         B     INFEXIT                                                          
*                                                                               
RECADDED MVI   GERROR1,NEWRECRD    NEW RECORD HAS BEEN ADDED TO THE F..         
         B     INFEXIT                                                          
*                                                                               
EOFLSELS MVI   GERROR1,33          END OF LIST - INPUT CHANGES OR HIT..         
         B     INFEXIT                                                          
*                                                                               
MAKESELS MVI   GERROR1,32          LIST DISPLAYED - INPUT CHANGES OR ..         
         B     INFEXIT                                                          
***********************************************************************         
* INFO MESSAGES (SYSTEM 23)                                                     
***********************************************************************         
TOOMNYIO MVI   GERROR1,MAXNMIOS    MAXIMUM NUMBER OF I/O'S EXCEEDED             
         L     R1,ATIOB            SOUND AN ALARM                               
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBALRM                                                
         DROP  R1                                                               
         B     MYINFXIT                                                         
***********************************************************************         
* MESSAGE ROUTINES                                                              
***********************************************************************         
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
INFRTEXT LA    R1,MYINFXIT                                                      
         B     *+8                                                              
ERRRTEXT LA    R1,ERREXIT                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         BR    R1                                                               
*                                                                               
NOTHING  DC    H'0'                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,55,RUN                                                        
         DC    X'00'                                                            
*                                                                               
ZEROS    DC    12C'0'                                                           
RELO     DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* STATUS                                                                        
         DC    AL1(PF06X-*,06,0,0,0,PFTRETRN)                                   
         DC    CL3'T',CL8' ',CL8' '                                             
PF06X    EQU   *                                                                
*                                                                               
* SEND                                                                          
         DC    AL1(PF09X-*,09,0,0,0,PFTRETRN)                                   
         DC    CL3'X',CL8' ',CL8' '                                             
PF09X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
* ACTUAL TRANSMIT                                                               
*                                                                               
         DC    AL1(PF18X-*,18,PFTCPROG,0,(PF18X-PF18)/KEYLNQ,0)                 
         DC    CL3' ',CL8'ORDER',CL8'TRANSMIT'                                  
PF18     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYWS,L'XMITCTB-1),AL2(XMITCTB-MYAREAD)                    
         DC    AL1(KEYTYCUR,L'DTLCLT-1),AL2(DTLCLT-DTLMKT)                      
         DC    AL1(KEYTYWS,L'PASSPRD-1),AL2(PASSPRD-MYAREAD)                    
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-MYAREAD)                    
         DC    AL1(KEYTYCUR,L'DTLSTA-1),AL2(DTLSTA-DTLMKT)                      
*                                                                               
PF18X    EQU   *                                                                
*                                                                               
* ACTUAL SEND                                                                   
*                                                                               
         DC    AL1(PF21X-*,21,PFTCPROG,0,(PF21X-PF21)/KEYLNQ,0)                 
         DC    CL3' ',CL8'ORDER',CL8'SEND'                                      
PF21     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYWS,L'XMITCTB-1),AL2(XMITCTB-MYAREAD)                    
         DC    AL1(KEYTYCUR,L'DTLCLT-1),AL2(DTLCLT-DTLMKT)                      
         DC    AL1(KEYTYWS,L'PASSPRD-1),AL2(PASSPRD-MYAREAD)                    
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-MYAREAD)                    
         DC    AL1(KEYTYCUR,L'DTLSTA-1),AL2(DTLSTA-DTLMKT)                      
PF21X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDCOMFACSD                                                                    
* FAFACTS                                                                       
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* CTGENEDICT                                                                    
* FATIOB                                                                        
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENEST                                                                      
* SPGENMKT                                                                      
* SPGENPRD                                                                      
* SPADAVCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENEDICT                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPADAVCOM                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSF9D          (OUR MAINTENANCE SCREEN)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSEAD          (OUR LIST SCREEN)                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSD9D          (OUR DISPLAY SCREEN)                         
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC3D          (OUR STANDARD COMMENTS LIST SCREEN)          
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENDRBTC        (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENDRFLT        (RECORD DSECTS)                              
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
ALSTKEYS DS    A                   A(NEXT AVAILABLE LIST KEY ENTRY)             
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
AGYNSPTS DS    F                   NUMBER OF SPOTS                              
AGYNMRCS DS    F                   NUMBER OF RECORDS PRINTED                    
AGYDOLRS DS    PL6                 NUMBER OF DOLLARS                            
*                                                                               
ELEMDISP DS    H                   ELEMENT DISPLACEMENT (USED FOR DIS)          
*                                  HOB = X'80' = AT THE DELNOT                  
*                                  HOB = X'40' = AT THE STATUS                  
*                                                                               
BITFLAG  DS    XL1                 VARIOUS FLAGS                                
BFXMITED EQU   X'80'                 TRIED TO TRANSMIT AN ORDER                 
BFNOFLEL EQU   X'40'                 NO COMMENT ELEMENT YET                     
BFPREVKY EQU   X'20'                 PREVIOUS KEY TO BE USE IN LIST             
BFFLGHTS EQU   X'10'                 WE USING FLIGHTS FOR THIS                  
BFLSTMON EQU   X'08'                 DONE WITH LIST BECAUSE OF LISTMON          
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                 A KEY FIELD HAS BEEN CHANGED               
MF1RECNF EQU   X'40'                 RECORD WAS NOT FOUND                       
MF1CHNGD EQU   X'20'                 RECORD WAS CHANGED                         
MF1NODAR EQU   X'10'                 SEND A NOTDARE ON PQ                       
MF1VAROR EQU   X'08'                 WE GOT A VAR ORDER HERE                    
MF1CNFCM EQU   X'04'                 WE GOT A CONFIRM WITH COMMENT              
MF1REVOR EQU   X'02'                 WE GOT A REVISED ORDER HERE                
*                                                                               
FILTFLG1 DS    XL1                 VARIOUS FILTER BITS FOR LIST MODE            
FFLG1MKT EQU   X'80'                 FILTER ON THE MARKET                       
FFLG1STA EQU   X'40'                               STATION                      
FFLG1CLT EQU   X'20'                               CLIENT                       
FFLG1PRD EQU   X'10'                               PRODUCT                      
FFLG1EST EQU   X'08'                               ESTIMATE                     
FFLG1FLT EQU   X'04'                               FLIGHT                       
*                                                                               
FLTMKT   DS    XL2                 FILTER FOR MARKET   (BINARY)                 
FLTSTA   DS    XL3                        FOR STATION  (BINARY)                 
FLTCLT   DS    XL2                        FOR CLIENT   (BINARY)                 
*                                                                               
OPTNFLAG DS    XL1                 VARIOUS OPTION BITS FOR LIST MODE            
OFLGSNDA EQU   X'80'                 TRANSMIT ALL RECORDS ON SCREEN             
*                                                                               
RCOMLINE DS    XL1                 LINE NUMBER OF NEXT REP COMMENT              
BUYSTDAT DS    CL6                 BUY START DATE                               
CURRDATE DS    CL6                 BUY START DATE                               
LSDATYMD DS    CL6                 LIST START DATE                              
PASSPRD  DS    CL7                 PASSED PRODUCT CODE TO ORDER/SEND            
ESTWFLT  DS    CL7                 PASSED EST W/ FLT   TO ORDER/SEND            
XMITCTB  DS    CL1                 TRANSMIT CASH/TRADE/BOTH                     
*                                                                               
SVBUYRCD DS    CL3                 BUYER CODE                                   
SVAGYID  DS    CL3                 AGENCY ID                                    
SVAGYOFF DS    CL2                 AGENCY OFFICE                                
SVBPRD   DS    XL1                 SAVED BINARY PRODUCT                         
SVBPR2   DS    XL1                 SAVED BINARY PIGGYBACK                       
SVQPRD   DS    CL3                 SAVED EBCDIC PRODUCT                         
SVQPR2   DS    CL3                 SAVED EBCDIC PIGGYBACK                       
SVBEST   DS    XL1                 SAVED ESTIMATE                               
SVBES2   DS    XL1                 SAVED PIGGY ESTIMATE                         
TEMPCOST DS    XL4                 SAVED BUY COST                               
SVBDNOWK DS    XL1                 SAVED BUY NUMBER OF WEEKS                    
SVFLTNUM DS    XL1                 SAVED FLIGHT NUMBER                          
HIFLTNUM DS    XL1                 HIGHEST FLIGHT NUMBER                        
CURRFLTN DS    XL1                 CURRENT FLIGHT NUMBER                        
FLTNONFL DS    XL3                 NON-FLIGHT END DATE                          
FLTSDATE DS    XL3                 FLIGHT START DATE                            
FLTEDATE DS    XL3                 FLIGHT END DATE                              
ESTDAILY DS    CL1                 DAILY SCHEDULE                               
PR1TIMSH DS    XL1                 PRODUCT 1 TIME SHARE, 0 - USE BDTIME         
PFKEYLIN DS    XL1                 DISP FROM FIRST SELECT LINE                  
*                                                                               
PREVKEY  DS    XL(L'DOKEY)         SAVED KEYS                                   
ORDERKEY DS    XL(L'DOKEY)         SAVED KEYS                                   
SAVEKEY  DS    XL(L'DOKEY)                                                      
DISKADDR DS    XL4                 DISK ADDRESS                                 
*                                                                               
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE AREA                          
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL60                                                             
*                                                                               
DOIDELEM DS    XL(DOIDLNQ)         COPY OF DARE ID ELEMENT                      
DOXMELEM DS    XL(DOXMTLNQ)                     TRANSMISSION ELEMENT            
DOBYELEM DS    XL(DOBUYLNQ)                     BUYLINE ELEMENT                 
*                                                                               
LISTTABL DS    13XL(LSTTBLLN)      LIST OF KEYS SHOWN ON SCREEN                 
LISTTBLX DS    0X                                                               
*                                                                               
BIGSPLKY DS    XL128               BIG SPOOLKEY                                 
         EJECT                                                                  
***********************************************************************         
* ONLINE LIST LINE                                                              
***********************************************************************         
LINDSECT DSECT                                                                  
LINSELH  DS    CL(L'MKTSEL1H)                                                   
LINSEL   DS    CL(L'MKTSEL1)                                                    
LINDTLH  DS    CL(L'MKTDTL1H)                                                   
LINDTL   DS    CL(L'MKTDTL1)                                                    
LINSCMH  DS    CL(L'MKTSCM1H)                                                   
LINSCM   DS    CL(L'MKTSCM1)                                                    
LINNEXTL DS    0C                                                               
***********************************************************************         
* DETAIL LINE DSECT                                                             
***********************************************************************         
DTLLINED DSECT                                                                  
DTLLINEH DS    CL8                                                              
DTLMKT   DS    CL4                                                              
         DS    CL1                                                              
DTLSTA   DS    CL8                                                              
         DS    CL1                                                              
DTLCLT   DS    CL3                                                              
         DS    CL1                                                              
DTLPRD   DS    CL7                                                              
         DS    CL1                                                              
DTLEST   DS    CL3                                                              
         DS    CL1                                                              
DTLFLT   DS    CL2                                                              
         DS    CL2                                                              
DTLSPTS  DS    CL4                                                              
         DS    CL1                                                              
DTLDOLR  DS    CL10                                                             
DTLASTRX DS    CL1                                                              
DTLSCMH  DS    CL8                                                              
DTLSCM   DS    CL(L'MKTSCM1)                                                    
DTLNEXTL DS    0C                                                               
       ++INCLUDE SPOMSDARED                                                     
         EJECT                                                                  
***********************************************************************         
* ERROR LIST DSECT                                                              
***********************************************************************         
ERRDSECT DSECT                                                                  
ERRLNGTH DS    XL1                 LENGTH OF ERROR ENTRY                        
ERRNUMBR DS    CL3                 ERROR NUMBER (EBCDIC NUMERIC)                
ERRTEXT  DS    0C                  NUMBER OF SPOTS                              
         SPACE 2                                                                
***********************************************************************         
* LIST KEY TABLE DSECT                                                          
***********************************************************************         
LSTTABLD DSECT                                                                  
LSTBTKEY DS    XL(L'PREVKEY)       KEY OF BATCH ORDER RECORD                    
LSTFLTNM DS    XL1                 FLIGHT (X'80'-READ THIS KEY ALREADY)         
LSTFLTSD DS    XL3                 FLIGHT START DATE                            
LSTFLTED DS    XL3                 FLIGHT END DATE                              
LSTNSPTS DS    XL2                 # OF SPOTS                                   
LSTTDOLR DS    XL4                 TOTAL DOLLARS                                
LSTTBLLN EQU   *-LSTTABLD          LENGTH OF TABLE ENTRY                        
         EJECT                                                                  
***********************************************************************         
* EDICT CHUNKY DSECT                                                            
***********************************************************************         
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093SP0MS0AC  12/11/01'                                      
         END                                                                    

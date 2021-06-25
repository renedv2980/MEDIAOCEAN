*          DATA SET SPEZF25    AT LEVEL 080 AS OF 12/26/12                      
*PHASE T23025A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T23025 - EASI INVOICE LIST AND MAINT                        *         
*  COMMENTS: THIS PROGRAM LISTS INVOICES AND CAN OVERIDE CLIENT       *         
*            AND/OR PRODUCT FOR EACH INVOICE.                         *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG                                              *         
*          R5 - WORK REG                                              *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORKING STORAGE                                                 
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T23025 - EASI BUY LIST'                                         
*                                                                               
T23025   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 EZ25WKLQ,**3025**,RR=R2                                          
         LR    R7,RC                                                            
         USING EZ25WKD,R7                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,LISTRECS                                                    
         BE    LIST                                                             
*                                                                               
EQXIT    XC    ERRNUM,ERRNUM                                                    
         CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
*                                                                               
VKEY     DS    0H                                                               
         OC    T230FFD+6(2),T230FFD+6   TEST ANY LIMIT ACCESS                   
         BZ    VK00                                                             
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
VK00     DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   ASSUME NO KEY FIELDS CHANGED           
         NI    MISCFLG1,X'FF'-MF1CABLE   CABLE STATION                          
*                                                                               
         MVC   BUYMEDN,SPACES                                                   
         OI    BUYMEDNH+6,X'80'                                                 
         MVC   BUYCLTN,SPACES                                                   
         OI    BUYCLTNH+6,X'80'                                                 
         MVC   BUYHPRN,SPACES                                                   
         OI    BUYHPRNH+6,X'80'                                                 
         MVC   BUYMKTN,SPACES                                                   
         OI    BUYMKTNH+6,X'80'                                                 
         MVC   BUYPERN,SPACES                                                   
         OI    BUYPERNH+6,X'80'                                                 
*                                                                               
         LA    R2,BUYMEDH                                                       
         OC    BUYMED,SPACES                                                    
         OI    BUYMEDH+6,X'80'                                                  
         BRAS  RE,CHKVAL                                                        
         BRAS  RE,VMED                                                          
         BNE   SPERREX                                                          
         MVC   BUYMEDN,MEDNM                                                    
         OC    BUYMEDN,SPACES                                                   
         OI    BUYMEDNH+6,X'80'                                                 
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         LA    R2,BUYCLTH                                                       
         OC    BUYCLT,SPACES                                                    
         OI    BUYCLTH+6,X'80'                                                  
         BRAS  RE,CHKVAL                                                        
         GOTO1 VALICLT                                                          
         L     RF,AIO1                                                          
         MVC   SVOFC,COFFICE-CLTHDR(RF)                                         
         MVC   SVCACCS,CACCESS-CLTHDR(RF)                                       
*                                                                               
         XC    SVMACCS,SVMACCS                                                  
         MVC   ERRNUM,=AL2(SECLOCQ)                                             
         CLI   T230FFD+6,C'+'      TEST MARKET LIMIT ACCESS                     
         BE    *+12                                                             
         BRAS  RE,CALLOFCR                                                      
         BNE   SPERREX                                                          
*                                                                               
         MVC   BUYCLTN,CLTNM                                                    
         OI    BUYCLTNH+6,X'80'                                                 
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
*                                                                               
         MVC   BUYHPRN,SPACES                                                   
         LA    R2,BUYHPRDH                                                      
         OC    BUYHPRD,SPACES                                                   
         OI    BUYHPRDH+6,X'80'                                                 
         BRAS  RE,CHKVAL                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK03                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK05                                                             
         CLI   5(R2),3                                                          
         BNE   VK05                                                             
*                                                                               
VK03     DS    0H                                                               
         MVC   BUYHPRD(3),=CL3'ALL'                                             
         OI    BUYHPRDH+6,X'80'                                                 
         MVC   BUYHPRN(12),=CL12'ALL PRODUCTS'                                  
         B     VK15                                                             
*                                                                               
VK05     DS    0H                                                               
         LA    RE,BLOCK                                                         
         LHI   RF,L'BLOCK                                                       
         XCEFL                                                                  
*                                                                               
         MVC   ERRNUM,=AL2(INVPRDQ)                                             
         GOTO1 SCANNER,DMCB,(R2),(X'82',BLOCK),C',=-='                          
         CLI   DMCB+4,0                                                         
         BE    SPERREX                                                          
*                                                                               
         LA    R3,BLOCK            R3 = A(1ST PRODUCT)                          
         CLI   0(R3),2                                                          
         BL    SPERREX                                                          
         CLI   0(R3),3                                                          
         BH    SPERREX                                                          
         CLI   1(R3),0             PRODUCT CAN'T HAVE SUB-FIELDS                
         BNE   SPERREX                                                          
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH,11                                                      
         MVC   FAKEFLDH+5(1),0(R3)    L'INPUT                                   
         ZIC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
*                                                                               
         LA    R2,FAKEFLDH                                                      
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         LA    R2,BUYHPRDH                                                      
         CLI   ERROR,0                                                          
         BNE   SPERREX                                                          
*                                                                               
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   BUYHPRN,WORK+4                                                   
*                                                                               
         LA    R3,32(R3)           R3 = A(2ND PRODUCT)                          
*                                                                               
         CLI   0(R3),0                                                          
         BE    VK10                                                             
*                                                                               
         CLI   0(R3),2                                                          
         BL    SPERREX                                                          
         CLI   0(R3),3                                                          
         BH    SPERREX                                                          
         CLI   1(R3),0             PRODUCT CAN'T HAVE SUB-FIELDS                
         BNE   SPERREX                                                          
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH,11                                                      
         MVC   FAKEFLDH+5(1),0(R3)    L'INPUT                                   
         ZIC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
*                                                                               
         LA    R2,FAKEFLDH                                                      
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         LA    R2,BUYHPRDH                                                      
         CLI   ERROR,0                                                          
         BNE   SPERREX                                                          
*                                                                               
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
*                                                                               
VK10     DS    0H                                                               
         CLI   BPRD2,0                                                          
         BE    VK15                                                             
         MVI   BUYHPRN+(L'BUYHPRN/2),C'-'                                       
         LHI   RF,L'BUYHPRN                                                     
         SHI   RF,L'BUYHPRN/2+1                                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUYHPRN+(L'BUYHPRN/2+1)(0),WORK+4                                
*                                                                               
VK15     DS    0H                                                               
         OI    BUYHPRNH+6,X'80'                                                 
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
         MVC   QEST,SPACES                                                      
         MVI   BEST,X'00'                                                       
         LA    R2,BUYESTH                                                       
         OC    BUYEST,SPACES                                                    
         OI    BUYESTH+6,X'80'                                                  
         BRAS  RE,CHKVAL                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK16                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK18                                                             
         CLI   5(R2),3                                                          
         BNE   VK18                                                             
*                                                                               
VK16     DS    0H                                                               
         MVC   BUYEST(3),=CL3'ALL'                                              
         OI    BUYESTH+6,X'80'                                                  
         MVC   BUYESTN(13),=CL13'ALL ESTIMATES'                                 
         B     VK20                                                             
*                                                                               
VK18     DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     RF,AIO1                                                          
         MVC   BUYESTN,EDESC-ESTHDR(RF)                                         
*                                                                               
VK20     DS    0H                                                               
         OI    BUYESTNH+6,X'80'                                                 
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
         LA    R2,BUYSTAH                                                       
         OC    BUYSTA,SPACES                                                    
         OI    BUYSTAH+6,X'80'                                                  
         BRAS  RE,CHKVAL                                                        
         BRAS  RE,VSTA                                                          
         BNE   SPERREX                                                          
*                                                                               
         MVC   BUYMKTN,MKTNM                                                    
         OI    BUYMKTNH+6,X'80'                                                 
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
* VALIDATE OPTIONS FIRST, TO SEE IF "CALENDAR" OR "WEEKLY" ARE ENTERED          
*                                                                               
         LA    R2,BUYOPTH                                                       
         BRAS  RE,CHKVAL                                                        
         BRAS  RE,VOPT                                                          
         BNE   SPERREX                                                          
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
         LA    R2,BUYPERH                                                       
         OC    BUYPER,SPACES                                                    
         OI    BUYPERH+6,X'80'                                                  
         BRAS  RE,CHKVAL                                                        
         BRAS  RE,VPER                                                          
         BNE   SPERREX                                                          
         OI    4(R2),X'20'         FIELD VALIDATED                              
*                                                                               
* DISPLAY PERIOD START-END ON SCREEN                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,BSTART),(5,BUYPERN)                               
         GOTO1 DATCON,DMCB,(2,BEND),(5,BUYPERN+9)                               
         MVI   BUYPERN+8,C'-'                                                   
         OI    BUYPERNH+6,X'80'                                                 
*                                                                               
* CALCULATE WEEK STARTING DAYS FOR "WEEKLY" OPTION                              
*                                                                               
         TM    OPTFLAG,OFWEEKQ     DO WE EVEN NEED WEEKS?                       
         BZ    VKXIT               NO - SKIP IT ALL                             
*                                                                               
         XC    BWEEKS,BWEEKS                                                    
         LA    R2,BWEEKS                                                        
         GOTO1 DATCON,DMCB,(2,BSTART),(0,WORK)                                  
*                                                                               
VK35     GOTO1 DATCON,DMCB,(0,WORK),(2,WORK+6)                                  
         CLC   BEND,WORK+6                                                      
         BNH   VKXIT                                                            
         MVC   0(2,R2),WORK+6                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'7'                                      
         MVC   WORK(6),WORK+6                                                   
         LA    R2,2(R2)                                                         
         B     VK35                                                             
*                                                                               
VKXIT    DS    0H                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*   LIST - LIST RECORDS                                               *         
***********************************************************************         
*                                                                               
LIST     DS    0H                                                               
         LA    R3,TSARBLK                                                       
         USING TSARD,R3                                                         
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    LS00                YES, RE-READ BUYS INTO BUFFER                
*                                                                               
         MVI   TSACTN,TSAINI                                                    
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSARFLG,C'I'        TSAR INITIALIZED                             
*                                                                               
LS00     DS    0H                                                               
*                                                                               
* BUILD COLUMN LIST                                                             
*                                                                               
         BRAS  RE,BLDCOL                                                        
*                                                                               
         LA    R2,BUYTTLEH                                                      
         BRAS  RE,CLRSCR                                                        
         BRAS  RE,BLDTTL                                                        
         OI    BUYTTLEH+6,X'80'    TRANSMIT                                     
*                                                                               
         LA    R2,BUYULNH          START AT BEGINNING OF LINE                   
         ICM   R2,8,=C'-'                                                       
         BRAS  RE,BLDTTL                                                        
         OI    BUYULNH+6,X'80'     TRANSMIT                                     
*                                                                               
* FIRST READ THE BUYS, THE ELEMENTS, EXTRACT                                    
* THE TOTALS FOR PRD-EST, AND SAVE THEM                                         
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BNZ   LS05                YES, RE-READ BUYS INTO BUFFER                
*                                                                               
         MVI   TSACTN,TSARES       RESTORE TSAR BUFFER                          
         GOTO1 VTSAR,TSARBLK                                                    
         BE    LS100                                                            
         DC    H'0'                                                             
*                                                                               
         MVI   TSARFLG,C'R'        TSAR BUFFER RSTORED                          
*                                                                               
LS05     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BUYREC,R6                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'                                                    
         MVC   BUYKMSTA,BMKTSTA                                                 
         MVC   BUYKEST,BEST                                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'SPTDIR',KEY,KEY                      
         B     LS20                                                             
*                                                                               
LS10     GOTO1 DATAMGR,DMCB,=C'DMRSEQ ',=C'SPTDIR',KEY,KEY                      
*                                                                               
LS20     DS    0H                                                               
         MVC   BYTE,KEY+BUYKSTAC+2-BUYKEY                                       
         CLI   QSTANEW,C'0'        IS THIS CABLE?                               
         BL    LS30                NO, DO NOT BOTHER WITH THE NETWORK           
         CLC   QNET,SPACES                                                      
         BH    LS30                                                             
         NI    KEY+BUYKSTAC-BUYKEY+2,X'80'    TURN OFF LAST 7 BITS              
*                                                                               
LS30     DS    0H                                                               
         CLC   KEY(BUYKEST-BUYKEY),KEYSAVE                                      
         MVC   KEY+BUYKSTAC+2-BUYKEY(1),BYTE                                    
         BNE   LS50                                                             
         DROP  R6                                                               
*                                                                               
         CLI   BEST,X'00'                                                       
         BE    *+14                                                             
         CLC   KEY+BUYKEST-BUYKEY(1),BEST                                       
         BNE   LS10                                                             
*                                                                               
* GET CURRENT NETWORK                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SPOTCAN                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,BUYKMSTA-BUYKEY+KEY                                     
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURRMKT(12),STAPQMKT      CURRENT MKT/STA/NET                    
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'SPTFIL',KEY+14,AIO                   
         CLI   8(R1),0                                                          
         BE    LS40                                                             
         CLI   8(R1),X'02'                                                      
         BE    LS10                                                             
         DC    H'0'                                                             
*                                                                               
LS40     DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         MVC   CURSREP,SPACES                                                   
*                                                                               
         MVC   DATADISP,=AL2(24)                                                
         MVI   ELCODE,X'00'                                                     
         BRAS  RE,GETEL                                                         
         B     *+12                PROCESS ELEMENT                              
         BRAS  RE,NEXTEL                                                        
         BNE   LS10                IF EOR - GET NEXT BUY                        
         BRAS  RE,PROCEL                                                        
         B     *-12                READ NEXT ELEMENT                            
*                                                                               
* READ NINV RECORDS                                                             
*                                                                               
LS50     DS    0H                                                               
         BRAS  RE,INITMNIO                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING SNVKEY,R1                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         TM    MISCFLG1,MF1CABLE                                                
         BZ    *+8                                                              
         NI    SNVKSTA+2,X'80'                                                  
         MVC   SNVKMOS,BMOS                                                     
         XC    SNVKMOS,=X'FFFF'                                                 
*                                                                               
LS60     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'XSPDIR',KEY,KEY                      
         CLC   KEY(SNVKINV-SNVKEY),KEYSAVE                                      
         BNE   LS100                                                            
*                                                                               
         LA    R1,KEY                                                           
         BRAS  RE,PROCINV                                                       
         MVC   SNVKINV+L'SNVKINV(2),=X'FFFF'                                    
         B     LS60                                                             
*                                                                               
* READ RECORDS FROM TSAR AND DISPLAY THEM ON SCREEN                             
*                                                                               
LS100    DS    0H                                                               
         LA    R2,BUYLN1H                                                       
*                                                                               
         LA    R6,TSREC                                                         
         USING BLINED,R6                                                        
*                                                                               
         XC    TSREC(BLINEDLQ),TSREC                                            
         MVI   TSACTN,TSARDH                                                    
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BNZ   LS110               YES, READ FIRST RECORD FROM BUFFER           
         MVC   TSREC(BLKLQ),SVTSREC                                             
*                                                                               
LS110    DS    0H                                                               
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TSERRS,TSEEOF                                                    
         BO    LISTX                                                            
*                                                                               
         BRAS  RE,XCFLD            CLEAR CURRENT LINE                           
*                                                                               
* DISPLAY NETWORK                                                               
*                                                                               
         LHI   R1,CTNWKQ                                                        
         BRAS  RE,GETLDISP                                                      
         BNE   *+14                                                             
*                                                                               
         LA    R4,8(RF,R2)                                                      
         MVC   0(3,R4),BLNETWK                                                  
*                                                                               
* DISPLAY PRODUCT(S)                                                            
*                                                                               
         LHI   R1,CTPRDQ                                                        
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                PRODUCT MUST ALWAYS BE THERE                 
         LA    R4,8(RF,R2)                                                      
*                                                                               
         MVC   0(3,R4),BLPRD                                                    
         OC    BLPRD2,BLPRD2                                                    
         BZ    LS120                                                            
*                                                                               
         CLI   2(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVI   3(R4),C'-'                                                       
         MVC   4(3,R4),BLPRD2                                                   
*                                                                               
* DISPLAY ESTIMATE                                                              
*                                                                               
LS120    DS    0H                                                               
         LHI   R1,CTESTQ                                                        
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
         EDIT  (B1,BLEST),(3,0(R4)),ALIGN=RIGHT                                 
*                                                                               
* DISPLAY START DATE OF THE WEEK                                                
*                                                                               
         TM    OPTFLAG,OFWEEKQ                                                  
         BZ    LS125                                                            
*                                                                               
         LHI   R1,CTSDATQ                                                       
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
         GOTO1 DATCON,DMCB,(2,BLSDATE),(4,0(R4))                                
*                                                                               
* DISPLAY REP                                                                   
*                                                                               
LS125    DS    0H                                                               
         LHI   R1,CTREPQ                                                        
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
         MVC   0(3,R4),BLSREP                                                   
*                                                                               
* DISPLAY NUMBER OF SPOTS                                                       
*                                                                               
         LHI   R1,CTSPTSQ                                                       
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
         EDIT  (B4,BLSPOTS),(4,0(R4)),ZERO=NOBLANK,COMMAS=YES                   
*                                                                               
* DISPLAY MP FLAGS                                                              
*                                                                               
         LHI   R1,CTMPQ                                                         
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
*                                                                               
         TM    BLFLAG,BLFMATQ                                                   
         BZ    *+8                                                              
         MVI   0(R4),C'M'                                                       
*                                                                               
         MVI   1(R4),C'*'                                                       
*                                                                               
         TM    BLFLAG,BLFALPDQ+BLFNOPDQ                                         
         BNO   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    BLFLAG,BLFALPDQ                                                  
         BZ    *+8                                                              
         MVI   1(R4),C'P'                                                       
*                                                                               
         TM    BLFLAG,BLFNOPDQ                                                  
         BZ    *+8                                                              
         MVI   1(R4),C' '                                                       
*                                                                               
* DISPLAY GROSS DOLLARS                                                         
*                                                                               
         LHI   R1,CTGRSQ                                                        
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
*                                                                               
         EDIT  (P8,BLGROSS),(13,0(R4)),2,ZERO=NOBLANK,COMMAS=YES                
*                                                                               
* CHECK WHETHER PAID GROSS COLUMN SHOULD BE BLANK                               
*                                                                               
         TM    OPTFLAG,OFPAIDQ                                                  
         BZ    LS127                                                            
         CP    BLGROSS,=P'0'                                                    
         BNE   LS127                                                            
         TM    BLFLAG,BLFNOPDQ                                                  
         BO    LS128                                                            
*                                                                               
* DISPLAY NET DOLLARS (OR PAID GROSS DOLLARS)                                   
*                                                                               
LS127    DS    0H                                                               
         LHI   R1,CTNETQ                                                        
         TM    OPTFLAG,OFPAIDQ                                                  
         BZ    *+8                                                              
         LHI   R1,CTPGRSQ                                                       
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
         EDIT  (P8,BLNET),(13,0(R4)),2,ZERO=NOBLANK,COMMAS=YES                  
*                                                                               
* DISPLAY LIST OF INVOICES                                                      
*                                                                               
LS128    DS    0H                                                               
         LHI   R1,CTINVQ                                                        
         BRAS  RE,GETLDISP                                                      
         BE    *+6                                                              
         DC    H'0'                ESTIMATE MUST ALWAYS BE THERE                
         LA    R4,8(RF,R2)                                                      
*                                                                               
         XC    BLOCK(L'BLINVS),BLOCK                                            
         LA    RE,BLINVS                                                        
         LHI   R0,10                                                            
         LA    RF,BLOCK                                                         
*                                                                               
LS130    DS    0H                                                               
         CLC   0(10,RE),SPACES                                                  
         BNH   LS140                                                            
         MVC   0(10,RF),0(RE)                                                   
         OC    0(20,RF),SPACES                                                  
         LA    RF,20(RF)                                                        
         LA    RE,10(RE)                                                        
         BCT   R0,LS130                                                         
*                                                                               
LS140    DS    0H                                                               
         LHI   RF,10                                                            
         SR    RF,R0                                                            
         STC   RF,BYTE                                                          
         XC    WORK,WORK                                                        
         LHI   R0,L'WORK                                                        
         AHI   R0,8                                                             
         STC   R0,WORK                                                          
*                                                                               
         GOTO1 UNSCAN,DMCB,(BYTE,BLOCK),WORK,0,0                                
*                                                                               
         ZIC   R0,INVCOLW                                                       
         LA    R1,WORK+8                                                        
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
*                                                                               
         CLC   1(10,R1),SPACES                                                  
         BNH   LS200                                                            
*                                                                               
         LHI   R0,9                                                             
         CLI   0(R1),C','                                                       
         BE    *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R0,*-14                                                          
*                                                                               
         MVI   0(R1),C'+'                                                       
*                                                                               
LS200    DS    0H                                                               
         ZIC   RF,INVCOLW                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),WORK+8                                                   
*                                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         MVC   SVTSREC(BLKLQ),TSREC                                             
         MVI   TSACTN,TSANXT       READ NEXT TSAR RECORD                        
*                                                                               
         LA    R2,(BUYLN1H-BUYULNH)(R2) NEXT LINE                               
         LA    RF,BUYPFLNH                                                      
         CR    R2,RF                                                            
         BL    LS110                                                            
*                                                                               
* REACHED END OF SCREEN HERE.  SAVE TSAR BUFFER INFORMATION                     
*                                                                               
         MVI   TSACTN,TSASAV                                                    
         GOTO1 VTSAR,TSARBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSARFLG,C'S'        TSAR BUFFER SAVED                            
*                                                                               
*&&DO                                                                           
* TEMP DEBUG                                                                    
         L     RE,ACOMFACS                                                      
         ICM   RF,15,CSWITCH-COMFACSD(RE)                                       
         GOTO1 (RF),DMCB,X'00FFFFFF'  NO-GET PROGRAM NAME                       
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,0(R1)            A(TCB)                                       
         L     R0,TCBTSAR-TCBD(R1) A(TSAR BUFFER)                               
         LHI   R1,2*4096           LETS JUST CLEAR THE FIRST 8K                 
         XR    RF,RF                                                            
         SAM31                                                                  
         MVCL  R0,RE                                                            
         SAM24                                                                  
*                                                                               
* END TEMP DEBUG                                                                
*&&                                                                             
*                                                                               
         OI    GENSTAT2,USMYOK                                                  
         MVC   CONHEAD(40),=CL40'List displayed - hit enter for next'           
         OI    CONHEADH+6,X'80'                                                 
         B     XIT                                                              
*                                                                               
LISTX    DS    0H                                                               
*                                                                               
* DISPLAYED ALL AVAILABLE LINES.  END OF SCREEN NOT REACHED                     
* TAKE OFF PREV VALIDATED BIT ON MEDIA FIELD TO RE-DISPLAY LIST                 
* FROM THE VERY FIRST LINE                                                      
*                                                                               
         NI    BUYMEDH+4,X'FF'-X'20'  TAKE OFF PREV VALIDATED BIT               
         OI    BUYMEDH+6,X'80'                                                  
*                                                                               
         OI    GENSTAT2,USMYOK                                                  
         MVC   CONHEAD(40),=CL40'end of list - hit enter for first'             
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         DROP  R6,R3                                                            
*                                                                               
*                                                                               
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
* RETURNED FROM ERREX. THIS MEANS ERROPT=Y                                      
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
* ERROR MESSAGE EQUATES                                                         
*                                                                               
MISSINGQ EQU   1                   MISSING INPUT FIELD                          
INVMEDQ  EQU   13                  INVALID MEDIA                                
INVPRDQ  EQU   15                  INVALID PRODUCT                              
INVMKTQ  EQU   17                  INVALID MARKET                               
INVSTAQ  EQU   18                  INVALID STATION                              
SECLOCQ  EQU   55                  SECIRITY LOCKOUT                             
MONERRQ  EQU   84                  DATE MUST BE A MONDAY                        
INVMOSQ  EQU   371                 INVALID MONTH EXPRESSION                     
INVOPTQ  EQU   603                 INVALID OPTION                               
NOMKTACQ EQU   1000                NO MARKET ACCESS ALLOWED                     
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*                                                                               
*********                                                                       
* SETUP *                                                                       
*********                                                                       
SETUP    NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         OI    GLSTSTAT,NOSELFLD                                                
*                                                                               
         L     RE,ACOMFACS                                                      
         MVC   VMINIO,CMINIO-COMFACSD(RE)                                       
*                                                                               
         OC    T230FFD+6(2),T230FFD+6   TEST ANY LIMIT ACCESS                   
         BZ    *+14                                                             
         LR    RF,RA                                                            
         AHI   RF,SECBLK-T230FFD                                                
         ST    RF,ASECBLK                                                       
*                                                                               
         LHI   RF,(BUYPFLNH-BUYLN1H)/(BUYLN1H-BUYULNH)                          
         STC   RF,NLISTS                                                        
         STC   RF,LISTNUM                                                       
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A5F' GETRATE                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETRATE,0(R1)                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000A68' GET STAVAL ADDRESS                        
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAVAL,0(R1)                                                    
*                                                                               
         MVC   DMCB+4(4),=X'D9000A7A' GET STAPACK ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,0(R1)                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000ABC' GET RCPACK ADDRESS                        
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VRCPACK,0(R1)                                                    
*                                                                               
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETBRD,0(R1)                                                    
*                                                                               
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 CALLOV,DMCB           CALL OFFICER                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFICER,0(R1)                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000A5D'                                           
         GOTO1 CALLOV,DMCB           CALL TSAR                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         L     RF,=A(COLTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,ACOLTAB                                                       
*                                                                               
         LA    R6,TSARBLK                                                       
         USING TSARD,R6                                                         
         LR    RE,R6                                                            
         LHI   RF,TSARDL2                                                       
         XCEFL                                                                  
*                                                                               
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,BLKLQ                                                     
         MVC   TSRECL,=AL2(BLINEDLQ)                                            
         MVI   TSPAGL,1                                                         
*        MVI   TSPAGN,8                                                         
         MVI   TSPAGN,4                                                         
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         DROP  R6                                                               
*                                                                               
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VMED     NTR1  BASE=*,LABEL=*                                                   
         MVC   ERRNUM,=AL2(MISSINGQ)                                            
         CLI   5(R2),0                                                          
         BE    VMEDNQX                                                          
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVI   SPOTCAN,0                                                        
         CLI   AGYPROF+7,C'C'                                                   
         BNE   *+8                                                              
         MVI   SPOTCAN,C'C'                                                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,AGYMEDEQ     X'02'                                        
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VMED2    BRAS  RE,NEXTEL                                                        
         MVC   ERRNUM,=AL2(INVMEDQ)                                             
         BNE   VMEDNQX                                                          
*                                                                               
         USING AGYMEDEL,R6                                                      
         CLC   AGYMEDCD,8(R2)                                                   
         BNE   VMED2                                                            
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,AGYMEDBT     DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,AGYMEDEX      MEDIA NAME                                   
         MVC   MEDCAPT,AGYVENEX    AND CAPTION                                  
         DROP  R6                                                               
*                                                                               
VMEDQX   DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
VMEDNQX  DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VSTA     NTR1  BASE=*,LABEL=*                                                   
         MVC   ERRNUM,=AL2(MISSINGQ)                                            
         CLI   5(R2),0                                                          
         BE    VSTANQX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(MISSINGQ)                                            
         LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,ACOMFACS                                                 
*                                                                               
         GOTO1 VSTAVAL,DMCB,(R4)                                                
         MVC   ERRNUM,=AL2(INVSTAQ)                                             
         CLI   STBERR,0                                                         
         BNE   VSTANQX                                                          
         MVC   QSTA,STBSTA         SET OUTPUT STATION                           
         MVC   QSTANEW,STBSTA      SET OUTPUT STATION                           
         MVC   QNET,STBNET         SET OUTPUT NETWORK                           
*                                                                               
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
*                                                                               
         MVC   STAPRNTN,SPACES                                                  
         MVC   STAPRNTN(4),QSTANEW                                              
*                                                                               
         CLI   QSTANEW,C'0'        IF THIS IS CABLE                             
         BL    VSTA12                                                           
*                                                                               
         OI    MISCFLG1,MF1CABLE                                                
         LA    RE,STAPRNTN+3       SET PRINTABLE STATION WITH                   
         CLI   0(RE),C' '                                                       
         BNE   *+6                 '/' & NETWORK                                
         BCTR  RE,0                                                             
         CLC   QSTANEW+4(3),SPACES                                              
         BNH   VSTA13                                                           
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),QSTANEW+5                                                
         B     VSTA13                                                           
*                                                                               
VSTA12   DS    0H                                                               
         GOTO1 VPRTSTA,DMCB,QSTANEW,STAPRNTN                                    
*                                                                               
*                                                                               
* READ STATION MASTER RECORD *                                                  
*                                                                               
VSTA13   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
*                                                                               
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTANEW                                                 
         CLI   STAKCALL+4,C' '                                                  
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,=C'000'                                                  
         CLC   QCLT,SPACES                                                      
         BNH   *+10                                                             
         MVC   STAKCLT,QCLT                                                     
         MVC   STAKFILL,=C'000'                                                 
*                                                                               
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'STATION',KEY,(R4)                    
         CLC   KEY(STAKEYLN),0(R4)                                              
         BNE   *+14                                                             
         MVC   QMKT,SMKT           SAVE MARKET                                  
         B     VSTA15              YES - USE IT                                 
*                                                                               
         CLC   QCLT,SPACES         DID WE GET CLT-SPECIFIC STA?                 
         BNH   VSTANQX             NO - INVALID STATION                         
* IF YES - TRY FOR NON-CLIENT SPECIFIC                                          
         LA    R4,KEY                                                           
         MVC   STAKCLT,=C'000'                                                  
*                                                                               
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'STATION',KEY,(R4)                    
         CLC   KEY(STAKEYLN),0(R4)                                              
         BNE   VSTANQX                                                          
         MVC   QMKT,SMKT                                                        
*                                                                               
VSTA15   DS    0H                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SPOTCAN                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,QMKT       MARKET                                       
         MVC   STAPQSTA(8),QSTANEW                                              
         GOTO1 VSTAPACK,(R1)                                                    
         MVC   ERRNUM,=AL2(INVSTAQ)                                             
         CLI   STAPERR,0                                                        
         BNE   VSTANQX                                                          
         MVC   BMKTSTA,STAPMKST                                                 
*                                                                               
         LA    R4,500(R4)                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVC   ERRNUM,=AL2(INVMKTQ)                                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'STATION',KEY,(R4)                    
         CLC   KEY(15),0(R4)                                                    
         BE    VSTA20                                                           
*                                                                               
         CLI   QMED,C'C'           ERROR EXCEPT FOR MEDIA=C, MKT=0000           
         BNE   VSTANQX                                                          
         CLC   QMKT,=C'0000'                                                    
         BNE   VSTANQX                                                          
         MVC   MKTNM,SPACES                                                     
         MVC   MKTNM(7),=C'NETWORK'                                             
         B     VSTAQX                                                           
*                                                                               
VSTA20   DS    0H                                                               
         MVC   MKTNM,MKTNAME-MKTREC(R4)                                         
         MVC   SVMACCS,MKTLTACC-MKTREC(R4)                                      
*                                                                               
         MVC   ERRNUM,=AL2(NOMKTACQ)                                            
         CLI   T230FFD+6,C'+'      TEST MARKET LIMIT ACCESS                     
         BNE   *+12                                                             
         BRAS  RE,CALLOFCR                                                      
         BNE   VSTANQX                                                          
*                                                                               
VSTAQX   DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
VSTANQX  DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VPER     NTR1  BASE=*,LABEL=*                                                   
         MVC   ERRNUM,=AL2(MISSINGQ)                                            
         CLI   5(R2),0                                                          
         BE    VPERNQX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(INVMOSQ)                                             
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK   VALIDATE FOR MMM/YY                 
         CLI   DMCB+3,0                                                         
         BE    VPERNQX                                                          
*                                                                               
         TM    OPTFLAG,OFCALQ                                                   
         BZ    VPER20                                                           
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(X'80',WORK+12),0                         
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+6(6),WORK                                                   
         B     VPER40                                                           
*                                                                               
VPER20   DS    0H                                                               
         MVC   WORK+4(2),=C'15'                                                 
*                                                                               
         GOTO1 VGETBRD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                        
         CLI   DMCB,0                                                           
         BE    VPERNQX                                                          
*                                                                               
VPER40   DS    0H                                                               
         GOTO1 DATCON,DMCB,WORK+6,(2,BSTART)                                    
         GOTO1 DATCON,DMCB,WORK+12,(2,BEND)                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(2,BMOS)                                        
*                                                                               
VPERQX   DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
VPERNQX  DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKVAL   TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZR  RE                  YES - EXIT                                   
         OI    MISCFLG1,MF1KYCHG   INDICATE KEY FIELD HAS BEEN CHANGED          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,BUYPFLNH         SAVE A(PFK LINE)                             
         SR    R0,R0                                                            
*                                                                               
CLR10    CR    R2,R3               PAST PFKEY LINE?                             
         BNL   CLRXIT              YES - EXIT                                   
         BRAS  RE,XCFLD                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     CLR10                                                            
*                                                                               
CLRXIT   J     EQXIT                                                            
         LTORG                                                                  
                                                                                
*                                                                               
*                                                                               
*                                                                               
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         OC    T230FFD+6(2),T230FFD+6   TEST ANY LIMIT ACCESS                   
         BZ    COFFQX                   NO                                      
*                                                                               
         LA    R4,WORK                                                          
         XC    0(64,R4),0(R4)                                                   
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFC                                                     
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),6(RA)                                                  
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',(R4)),ACOMFACS                               
         CLI   0(R1),0                                                          
         BE    COFFQX                                                           
*                                                                               
COFFNQX  J     NEQXIT                                                           
COFFQX   J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
PROCEL   NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R6),BDCODEQ                                                    
         BE    PREL05                                                           
         CLI   0(R6),RCPOLOQ                                                    
         BE    PREL10                                                           
         CLI   0(R6),RCPOTOQ                                                    
         BE    PREL10                                                           
         J     EQXIT                                                            
*                                                                               
PREL05   DS    0H                                                               
         USING BDELEM,R6                                                        
         OC    BDREP,BDREP                                                      
         JZ    EQXIT                                                            
         GOTO1 VRCPACK,DMCB,(C'U',BDREP),CURSREP                                
         JE    EQXIT                                                            
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
PREL10   DS    0H                                                               
         USING REGELEM,R6                                                       
*                                                                               
         TM    RSTATUS,X'C0'                                                    
         JNZ   EQXIT                                                            
*                                                                               
         L     R5,AIO                                                           
         USING BUYREC,R5                                                        
*                                                                               
* FILTER BY DATE                                                                
*                                                                               
         CLC   RDATE,BSTART                                                     
         JL    EQXIT                                                            
         CLC   RDATE,BEND                                                       
         JH    EQXIT                                                            
*                                                                               
         XC    TSREC,TSREC                                                      
         LA    R2,TSREC                                                         
         USING BLINED,R2                                                        
         LA    R3,TSARBLK                                                       
         USING TSARD,R3                                                         
*                                                                               
         TM    MISCFLG1,MF1CABLE                                                
         BZ    *+10                                                             
         MVC   BLNETWK,CURRNET                                                  
*                                                                               
         CLI   RLEN,10             PRD ALLOCATED?                               
         BNE   PREL20              YES                                          
         TM    RSTATUS,X'04'       HIATUS?                                      
         BNZ   PRELQX                                                           
         MVC   BLPRD,=C'POL'                                                    
         B     PREL30                                                           
*                                                                               
PREL20   DS    0H                  PRD ALLOCATED                                
         XC    FULL,FULL                                                        
         MVC   FULL(1),RPPRD                                                    
         BRAS  RE,FINDPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BLPRD,FULL+1                                                     
*                                                                               
         CLI   RLEN,RLPOL1LQ       PIGGYBACK ALLOCATION?                        
         BNH   PREL30                                                           
*                                                                               
         OC    RPPRD+4(1),RPPRD+4                                               
         BZ    PREL30                                                           
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(1),RPPRD+4                                                  
         BRAS  RE,FINDPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BLPRD2,FULL+1                                                    
*                                                                               
PREL30   DS    0H                  FILTER ON PRODUCTS                           
         CLI   BPRD,X'00'          X'00' = PRODUCT 'ALL'                        
         BE    PREL35                                                           
*                                                                               
         CLC   QPRD,BLPRD                                                       
         BNE   PRELQX                                                           
         CLC   QPRD2,BLPRD2                                                     
         BNE   PRELQX                                                           
*                                                                               
PREL35   DS    0H                                                               
*                                                                               
* CALL GETRATE TO GET COST, FILTER FOR ZERO-DOLLAR SPOTS                        
*                                                                               
         MVC   FULL,RPCOST         YES - SAVE NPW                               
         TM    BDSTAT,X'80'        TEST NPW IN COST                             
         BZ    *+12                                                             
         NI    RPCOST,X'03'                                                     
         OI    RPCOST,X'04'        SET NPW TO 1 FOR GETRATE                     
*                                                                               
         MVC   BYTE,RPPRD                                                       
         CLI   RLEN,10             PRD ALLOCATED?                               
         BNE   *+8                 YES - PASS IT TO GETRATE                     
         MVI   BYTE,X'00'          NO - GET POL RATE                            
*                                                                               
         CLI   SPOTCAN,C'C'                                                     
         BNE   *+8                                                              
         MVI   BDPURP,X'FD'                                                     
*                                                                               
         GOTO1 VGETRATE,DMCB,(BYTE,SPOTS),AIO,(R6)                              
         MVC   RPCOST,FULL         RESTORE RPCOST                               
*                                                                               
         TM    OPTFLAG,OFZEROQ                                                  
         BZ    *+14                                                             
         OC    GROSS,GROSS                                                      
         BNZ   PRELQX                                                           
*                                                                               
         MVC   BLEST,BUYKEST                                                    
*                                                                               
         TM    OPTFLAG,OFWEEKQ                                                  
         BZ    PREL37                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,RDATE),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         SR    R0,R0                                                            
         ICM   R0,1,DMCB                                                        
         BNZ   *+6                                                              
         DC    H'0'                DATE INVALID                                 
*                                                                               
         LNR   R0,R0                                                            
         AHI   R0,1                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,BLSDATE)                               
*                                                                               
PREL37   DS    0H                                                               
         MVC   BLSREP,CURSREP                                                   
*                                                                               
         MVI   TSACTN,TSARDH                                                    
         MVC   SVTSKEY,TSREC                                                    
         GOTO1 VTSAR,TSARBLK       SEE IF REC IN TSAR ALREADY                   
         BE    PREL50              YES IT IS - UPDATE IT                        
*                                                                               
PREL40   DS    0H                  RECORD NOT IN TSAR                           
         MVC   TSREC(BLKLQ),SVTSKEY                                             
         XC    BLSPOTS,BLSPOTS                                                  
         ZAP   BLGROSS,=P'0'                                                    
         ZAP   BLNET,=P'0'                                                      
         OI    BLFLAG,BLFALPDQ+BLFNOPDQ+BLFMATQ                                 
         MVC   BLINVS,SPACES                                                    
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARBLK       SEE IF REC IN TSAR ALREADY                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREL50   DS    0H                  RECORD FOUND IN TSAR                         
         ICM   RF,15,BLSPOTS       INCREMENT NUMBER OF SPOTS                    
         AHI   RF,1                                                             
         STCM  RF,15,BLSPOTS                                                    
*                                                                               
         OC    RPAY,RPAY           IS THIS SPOT PAID?                           
         BNZ   *+12                YES - TURN OFF "NO PAID SPOTS" FLAG          
         NI    BLFLAG,X'FF'-BLFALPDQ NO - TURN OFF "ALL PAID" FLAG              
         B     *+8                                                              
         NI    BLFLAG,X'FF'-BLFNOPDQ                                            
*                                                                               
         TM    RSTATUS,RSMINUSQ+RSMINSDQ                                        
         BNZ   PREL60                                                           
         ZIC   RF,1(R6)                                                         
         AR    RF,R6                                                            
         CLI   0(RF),ACCODEQ       AFFIDAVIT ELEMENT?                           
         BE    *+8                                                              
*                                                                               
PREL60   DS    0H                                                               
         NI    BLFLAG,X'FF'-BLFMATQ                                             
*                                                                               
         ICM   RF,15,GROSS                                                      
         CVD   RF,DUB                                                           
         AP    BLGROSS,DUB                                                      
*                                                                               
         TM    OPTFLAG,OFPAIDQ     PAID OPTION ENTERED?                         
         BZ    PREL70              NO - CALCULATE NET                           
*                                                                               
         OC    RPAY,RPAY           SPOT PAID?                                   
         BZ    PREL80              NO                                           
         AP    BLPGROSS,DUB        YES, CALCULATE PAID DOLLARS                  
         B     PREL80                                                           
*                                                                               
PREL70   DS    0H                                                               
         ICM   RF,15,NET                                                        
         CVD   RF,DUB                                                           
         AP    BLNET,DUB                                                        
*                                                                               
PREL80   DS    0H                                                               
         MVI   TSACTN,TSAWRT                                                    
         GOTO1 VTSAR,TSARBLK       SEE IF REC IN TSAR ALREADY                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRELQX   DS    0H                                                               
         J     EQXIT                                                            
         DROP  R6,R5,R3,R2                                                      
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* H.O. BYTE OF FULL EXPECTED TO HAVE 1-BYTE PRODUCT CODE                        
* PRD CODE LOOKED UP IN SVCLIST                                                 
* 3-CHAR PRODUCT CODE RETURNED IN FULL+1                                        
* UNEQUAL CONDITION SET IF PRD NOT FOUND                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
FINDPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SVCLIST                                                       
         LHI   R0,255                                                           
*                                                                               
FPRD10   CLI   0(R1),C' '                                                       
         JL    NEQXIT                                                           
         CLC   3(1,R1),FULL                                                     
         BE    FPRD50                                                           
         LA    R1,4(R1)                                                         
         BCT   R0,FPRD10                                                        
         J     NEQXIT                                                           
*                                                                               
FPRD50   DS    0H                                                               
         MVC   FULL+1(3),0(R1)                                                  
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VOPT     NTR1  BASE=*,LABEL=*                                                   
         MVI   OPTFLAG,X'00'                                                    
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
*                                                                               
         XC    ERRNUM,ERRNUM                                                    
         LA    RE,BLOCK                                                         
         LHI   RF,L'BLOCK                                                       
         XCEFL                                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK),0                                
         CLI   DMCB+4,0                                                         
         BE    VOPTX                                                            
         SR    R4,R4                                                            
         ICM   R4,1,DMCB+4         NUMBER OF OPTIONS                            
*                                                                               
         LA    R3,BLOCK            R3 = A(1ST PRODUCT)                          
*                                                                               
VOPT10   ZIC   R1,0(R3)            LENGTH                                       
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'MOS'                                               
         BNE   VOPT20                                                           
         CLI   1(R3),1                                                          
         BNE   VOPTNQX                                                          
         CLI   22(R3),C'C'                                                      
         BNE   VOPTNQX                                                          
*                                                                               
         OI    OPTFLAG,OFCALQ                                                   
         B     VOPT900                                                          
*                                                                               
VOPT20   DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'ZERO'                                              
         BNE   VOPT30                                                           
         CLI   1(R3),0                                                          
         BNE   VOPTNQX                                                          
*                                                                               
         OI    OPTFLAG,OFZEROQ                                                  
         B     VOPT900                                                          
*                                                                               
VOPT30   DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'PAID'                                              
         BNE   VOPT40                                                           
         CLI   1(R3),0                                                          
         BNE   VOPTNQX                                                          
*                                                                               
         OI    OPTFLAG,OFPAIDQ                                                  
         B     VOPT900                                                          
*                                                                               
VOPT40   DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'WEEKLY'                                            
         BNE   VOPT888                                                          
         CLI   1(R3),0                                                          
         BNE   VOPTNQX                                                          
*                                                                               
         OI    OPTFLAG,OFWEEKQ                                                  
         B     VOPT900                                                          
*                                                                               
VOPT888  DS    0H                                                               
         B     VOPTNQX                                                          
*                                                                               
VOPT900  LA    R3,32(R3)                                                        
         BCT   R4,VOPT10                                                        
*                                                                               
VOPTX    DS    0H                                                               
         TM    OPTFLAG,OFWEEKQ+OFCALQ                                           
         BO    VOPTNQX                                                          
*                                                                               
VOPTQX   DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
VOPTNQX  DS    0H                                                               
         MVC   ERRNUM,=AL2(INVOPTQ)                                             
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* INITIALIZE MINIO VALUES                                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
INITMNIO NTR1  BASE=*,LABEL=*                                                   
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         MVC   BYTE,MINBF2         PRESERVE FOR THOSE USING 2 MASTERS           
*                                                                               
         LA    RE,MINBLOCK         CLEAR MINBLOCK                               
         LHI   RF,MINBLKL                                                       
         XCEFL                                                                  
*                                                                               
         MVC   MINBF2,BYTE                                                      
*                                                                               
         L     RE,ACOMFACS                                                      
         MVC   MINRECUP,CRECUP-COMFACSD(RE)                                     
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINDIR,=C'XSPDIR  ' DIR NAME                                     
         MVC   MINFIL,=C'XSPFIL  ' FILE NAME                                    
         MVI   MINFKLEN,L'SNVKEY   KEY LENGTH                                   
         MVI   MINEKLEN,L'SNVKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'SNVKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVC   MINAGYC,AGENCY                                                   
         MVI   MINNCTL,L'SNVDSTAT  NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(3975) MAXIMUM RECORD LENGTH                        
         MVC   MINBUFF,AIO2        A(FIRST BUFFER)                              
         MVI   MINNBUF,2           USE TWO BUFFERS                              
         LA    RE,EZ25WKD                                                       
         AHI   RE,SNVMRTAB-EZ25WKD                                              
         ST    RE,MINRTAB          A(AREA FOR RECORD TABLE)                     
         MVC   MINRTABL,=Y(L'SNVMRTAB)  LENGTH OF RECORD TABLE                  
*                                                                               
         LA    RE,MELEM            A(AREA FOR ELEM OR CLUSTER)                  
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'MELEM)   MAX LENGTH OF ELEM OF CLUSTER             
         XC    0(L'MELEM,RE),0(RE)   CLEAR MINELEM AREA                         
         J     EQXIT                                                            
         DROP  R5                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* R1 EXPECTED TO ADDRESS INVOICE KEY                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PROCINV  NTR1  BASE=*,LABEL=*                                                   
         XC    FBPARMS(FBPARMLQ),FBPARMS                                        
         MVI   NINVFLAG,X'00'                                                   
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         XC    MINMKEY,MINMKEY                                                  
         MVC   MINMKEY(L'SNVKMAST),0(R1)                                        
*                                                                               
         MVC   FBINV,MINMKEY+SNVKINV-SNVKEY  SAVE INV #                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVDTELQ    LOOK FOR DETAIL ORDER ELEMENT                
*                                                                               
         GOTO1 VMINIO,DMCB,('MINHI',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    *+6                                                              
         DC    H'0'                DETAIL ORDER ELEMENT MUST BE PRESENT         
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVDTELD,R6                                                      
*                                                                               
* LOOP THROUGH DETAIL LIST, SEE WHAT IS THERE                                   
*                                                                               
         LA    R1,SNVDTFLD                                                      
         ZIC   R0,SNVDTLEN                                                      
         SHI   R0,7                                                             
*                                                                               
PI05     CLI   0(R1),FLDNPROD      PRODUCT?                                     
         BNE   *+12                                                             
         OI    NINVFLAG,NFPRDDET                                                
         B     PRI06                                                            
*                                                                               
         CLI   0(R1),FLDNESTM      ESTIMATE?                                    
         BNE   *+12                                                             
         OI    NINVFLAG,NFESTDET                                                
         B     PRI06                                                            
*                                                                               
         CLI   0(R1),FLDNNTWK      NETWORK?                                     
         BNE   *+8                                                              
         OI    NINVFLAG,NFNETDET                                                
*                                                                               
PRI06    LA    R1,1(R1)                                                         
         BCT   R0,PI05                                                          
         DROP  R6                                                               
*                                                                               
* NOW PROCESS HEADER, DETAIL ELEMENTS                                           
*                                                                               
* HEADER                                                                        
*                                                                               
         TM    NINVFLAG,NFPRDDET+NFESTDET      PRD+EST IN DETAILS?              
         BO    PI30                YES - DON'T EVEN PROCESS HEADER              
*                                                                               
* LOOK AT THE HEADER ELEMENT                                                    
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    LOOK FOR THE HEADER ELEMENT                  
*                                                                               
         GOTO1 VMINIO,DMCB,('MINHI',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    *+6                                                              
         DC    H'0'                HEADER ELEMENT MUST BE PRESENT               
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         TM    NINVFLAG,NFPRDDET   PRODUCT IN DETAILS?                          
         BO    PI30                                                             
*                                                                               
* HANDLE BINARY PRODUCT CODES HERE                                              
         CLI   SNVHDPRD,0          ANYTHING?                                    
         BE    PI10                NO - SEE IF WE HAVE ALPHA PRODUCT            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(1),SNVHDPRD    FIND 3-CHAR ALPHA CODES                      
         BRAS  RE,FINDPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FBPRD1,FULL+1                                                    
*                                                                               
         CLI   SNVHDPR2,0                                                       
         BE    PI20                                                             
         XC    FULL,FULL                                                        
         MVC   FULL(1),SNVHDPR2                                                 
         BRAS  RE,FINDPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FBPRD2,FULL+1                                                    
         B     PI20                                                             
*                                                                               
PI10     DS    0H                  HANDLE ALPHA PRD CODES HERE                  
         CLI   SNVHDLEN,SNVHDLN3   ALPHA PRD IN HEADER?                         
         BL    PI30                                                             
*                                                                               
         CLC   SNVHDAP1,SPACES                                                  
         BNH   PI30                NO - CHECK BINARY PRD CODE                   
*                                                                               
         MVC   FBPRD1,SNVHDAP1      SAVE IT                                     
         MVC   FBPRD2,SNVHDAP2                                                  
*                                                                               
PI20     DS    0H                                                               
         TM    NINVFLAG,NFESTDET                                                
         BO    *+10                                                             
         MVC   FBEST,SNVHDEST                                                   
*                                                                               
* LOOK AT DETAILS                                                               
*                                                                               
PI30     DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ    LOOK FOR DETAIL ELEMENT                      
         GOTO1 VMINIO,DMCB,('MINHI',(R5))                                       
         L     R6,MINELEM                                                       
         CLI   0(R6),SNVIDELQ                                                   
         BE    PI50                                                             
         B     PI100               CALL FINDBUY W CURRENT INFO                  
*                                                                               
PI40     DS    0H                                                               
         GOTO1 VMINIO,DMCB,('MINSEQ',(R5))                                      
         NI    FBFLAG,X'FF'-(FBFDONEQ+FBFSEQQ)                                  
*                                                                               
PI50     DS    0H                                                               
         CLI   MINERR,MINEEOF                                                   
         BE    PIX                                                              
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R6),SNVIDELQ                                                   
         BNE   PIX                                                              
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
* SEE IF WE NEED NETWORK, AND IF THERE IS ONE                                   
*                                                                               
         TM    MISCFLG1,MF1CABLE CACABLE STATION?                               
         BZ    PI55                NO - DON'T BOTHER WITN NETWORK BYTE          
         TM    NINVFLAG,NFNETDET                                                
         BZ    PI55                                                             
*                                                                               
         XC    FBNET,FBNET                                                      
         CLI   SNVIDNWK,0          ANYTHING IN NETWORK BYTE?                    
         BE    PI55                NO - DON'T BOTHER UNPACKING IT               
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SPOTCAN                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         XC    STAPMKT,STAPMKT                                                  
         MVC   STAPSTA,MINMKEY+SNVKSTA-SNVKEY                                   
         OC    STAPSTA+2(1),SNVIDNWK                                            
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FBNET,STAPQNET                                                   
         DROP  R1                                                               
*                                                                               
* SEE IF THERE IS PRODUCT AND IF WE NEED IT                                     
*                                                                               
PI55     DS    0H                                                               
         TM    NINVFLAG,NFPRDDET   PRODUCT IN DETAILS?                          
         BZ    PI100               NO, LOOK AT ESTIMATES                        
*                                                                               
         XC    FBPRD1,FBPRD1                                                    
         XC    FBPRD2,FBPRD2                                                    
         XC    FBEST,FBEST                                                      
*                                                                               
* HANDLE BINARY PRODUCTS HERE                                                   
         CLI   SNVIDPRD,0          ANYTHING?                                    
         BE    PI57                NO, SE IF WE HAVE ALPHA PRD CODES            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(1),SNVIDPRD                                                 
         BRAS  RE,FINDPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FBPRD1,FULL+1                                                    
*                                                                               
         CLI   SNVIDPR2,0                                                       
         BE    PI60                                                             
         XC    FULL,FULL                                                        
         MVC   FULL(1),SNVIDPR2                                                 
         BRAS  RE,FINDPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FBPRD2,FULL+1                                                    
         B     PI60                                                             
*                                                                               
PI57     DS    0H                  HANDLE ALPHA PRODUCT CODES HERE              
         CLI   SNVIDLEN,SNVIDL3Q   ALPHA PRD IN DETAILS?                        
         BL    PI100                                                            
*                                                                               
         CLC   SNVIDAP1,SPACES     ANYTHING THERE?                              
         BNH   PI100                                                            
*                                                                               
         MVC   FBPRD1,SNVIDAP1     YES, SAVE THEM                               
         MVC   FBPRD2,SNVIDAP2                                                  
*                                                                               
PI60     DS    0H                                                               
         TM    NINVFLAG,NFESTDET   ESTIMATE IN DETAILS?                         
         BZ    PI100               NO, GO LOOK FOR THE BUY                      
         MVC   FBEST,SNVIDEST                                                   
*                                                                               
PI100    DS    0H                                                               
         BRAS  RE,FINDBUY                                                       
         TM    FBFLAG,FBFDONEQ                                                  
         BO    PI40                                                             
*                                                                               
* PROCESS THE TSAR RECORD HERE                                                  
         LA    R2,TSREC                                                         
         BRAS  RE,ADDINV                                                        
*                                                                               
         CLI   SNVIDBES,X'00'                                                   
         BH    *+8                                                              
         NI    (BLFLAG-BLINED)(R2),X'FF'-BLFMATQ                                
*                                                                               
* WRITE THE UPDATED TSAR RECORD BACK TO BUFFER                                  
*                                                                               
         LA    R3,TSARBLK                                                       
         USING TSARD,R3                                                         
         MVI   TSACTN,TSAWRT                                                    
         GOTO1 VTSAR,TSARBLK                                                    
         DROP  R3                                                               
         B     PI100               GET NEXT TSAR REC                            
*                                                                               
PIX      J     EQXIT                                                            
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
FINDBUY  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TSREC                                                         
         USING BLINED,R2                                                        
         LA    R3,TSARBLK                                                       
         USING TSARD,R3                                                         
*                                                                               
         TM    FBFLAG,FBFSEQQ      READ NEXT?                                   
         BO    FB10                                                             
*                                                                               
         OI    FBFLAG,FBFSEQQ      SET READ NEXT                                
         XC    TSREC,TSREC                                                      
         MVI   TSACTN,TSARDH                                                    
         GOTO1 VTSAR,TSARBLK                                                    
         B     FB20                                                             
*                                                                               
FB10     DS    0H                                                               
         MVI   TSACTN,TSANXT                                                    
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
FB20     DS    0H                                                               
         TM    TSERRS,TSEEOF                                                    
         BZ    *+16                                                             
         OI    FBFLAG,FBFDONEQ     DONE                                         
         NI    FBFLAG,X'FF'-FBFSEQQ RE-SET READ NEXT                            
         B     FBX                                                              
*                                                                               
         OC    FBNET,FBNET         NETWORK                                      
         BZ    *+14                NO - MATCH TO ANY NETWORK                    
         CLC   BLNETWK,FBNET                                                    
         BNE   FB10                                                             
*                                                                               
         OC    FBPRD1,FBPRD1       PRODUCT                                      
         BZ    FB50                NO - MATCH TO ANY PRODUCT                    
         CLC   =C'POL',FBPRD1                                                   
         BE    FB50                POL - MATCH TO ANY PRODUCT                   
*                                                                               
         CLC   BLPRD,FBPRD1                                                     
         BNE   FB10                                                             
*                                                                               
         OC    FBPRD2,FBPRD2       PRODUCT 2                                    
         BZ    FB50                NO - MATCH TO ANY BRODUCT 2                  
         CLC   =C'POL',FBPRD2                                                   
         BE    FB50                POL - MATCH TO ANY PRODUCT 2                 
*                                                                               
         CLC   BLPRD2,FBPRD2                                                    
         BNE   FB10                                                             
*                                                                               
FB50     DS    0H                                                               
         OC    FBEST,FBEST         ESTIMATE                                     
         BZ    *+14                NO - MATCH TO ANY ESTIMATE                   
         CLC   BLEST,FBEST                                                      
         BNE   FB10                                                             
*                                                                               
FBX      DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         DROP  R3,R2                                                            
*                                                                               
*                                                                               
* R2 = TSAR RECORD                                                              
* FBINV - INVOICE TO ADD TO "MATCH" LIST                                        
ADDINV   NTR1  BASE=*,LABEL=*                                                   
         USING BLINED,R2                                                        
*                                                                               
         LHI   R0,L'BLINVS/L'SNVKINV                                            
         LA    R1,BLINVS                                                        
*                                                                               
AI10     CLC   0(L'SNVKINV,R1),SPACES                                           
         BNH   AI20                                                             
         CLC   0(L'SNVKINV,R1),FBINV                                            
         JE    EQXIT                                                            
         AHI   R1,L'SNVKINV                                                     
         BCT   R0,AI10                                                          
         J     NEQXIT              INV "MATCH" LIST FULL                        
*                                                                               
AI20     DS    0H                                                               
         MVC   0(L'SNVKINV,R1),FBINV                                            
         J     EQXIT                                                            
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
BLDCOL   NTR1  BASE=*,LABEL=*                                                   
         XC    COLLIST,COLLIST                                                  
         LA    R1,COLLIST                                                       
*                                                                               
         TM    MISCFLG1,MF1CABLE                                                
         BZ    *+12                                                             
         MVI   0(R1),CTNWKQ                                                     
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTPRDQ                                                     
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTESTQ                                                     
         LA    R1,2(R1)                                                         
*                                                                               
         TM    OPTFLAG,OFWEEKQ                                                  
         BZ    *+12                                                             
         MVI   0(R1),CTSDATQ                                                    
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTREPQ                                                     
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTSPTSQ                                                    
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTMPQ                                                      
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTGRSQ                                                     
         LA    R1,2(R1)                                                         
*                                                                               
         MVI   0(R1),CTNETQ                                                     
         TM    OPTFLAG,OFPAIDQ                                                  
         BZ    *+8                                                              
         MVI   0(R1),CTPGRSQ                                                    
         LA    R1,2(R1)                                                         
*                                                                               
* INVOICES COLUMN MUST ALWAYS BE LAST                                           
*                                                                               
         MVI   0(R1),CTINVQ                                                     
         MVI   2(R1),X'FF'         END                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
BLDTTL   NTR1  BASE=*,LABEL=*                                                   
         STCM  R2,8,BYTE                                                        
         LA    R2,0(R2)            CLEAR HOB                                    
         LA    R4,8(R2)                                                         
*                                                                               
         BRAS  RE,GFLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SPACES      INIT TO SPACES                               
*                                                                               
         LA    R1,COLLIST                                                       
*                                                                               
BLDT01   L     R3,ACOLTAB                                                       
*                                                                               
BLDT10   CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R1),0(R3)       FOUND COL EQU?                               
         BE    BLDT20                                                           
         ZIC   RF,1(R3)            COL WIDTH                                    
         AHI   RF,2                +EQU, +LENGTH BYTE                           
         AR    R3,RF                                                            
         B     BLDT10                                                           
*                                                                               
BLDT20   DS    0H                                                               
         CLI   0(R1),CTINVQ        INVOICE COLUMN?                              
         BE    BLDT30                                                           
*                                                                               
         LA    RE,2(R3)            TITLE                                        
         CLI   BYTE,C'-'           DOING UNDERLINES?                            
         BNE   *+8                                                              
         LA    RE,DASHES           YES, POINT RE TO DASHES                      
*                                                                               
         ZIC   RF,1(R3)            COL WIDTH                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RE)       MOVE COL TITLE TO LINE                       
*                                                                               
         LR    RE,R4                                                            
         LA    R0,8(R2)                                                         
         SR    RE,R0                                                            
         STC   RE,1(R1)                                                         
*                                                                               
         AR    R4,RF               ADVANCE TTLE LINE POINTER                    
         AHI   R4,2                +BCTR 0,+SPACE BTW COLUMNS                   
*                                                                               
         AR    R3,RF               ADVANCE COLTAB POINTER                       
         AHI   R3,3                +BCTR 0,+COL EQU,+COL WIDTH BYTE             
*                                                                               
         LA    R1,2(R1)            ADVANCE COLUMN LIST PTR                      
         CLI   0(R1),X'FF'                                                      
         BNE   BLDT01                                                           
         DC    H'0'         MUST EXIT ON INV COLUMN, AND NEVER GET HERE         
*                                                                               
* CALCULATE WIDTH OF INV COLUMN                                                 
*                                                                               
BLDT30   DS    0H                                                               
         LR    RE,R4                                                            
         LA    R0,8(R2)                                                         
         SR    RE,R0                                                            
         STC   RE,1(R1)                                                         
*                                                                               
         BRAS  RE,GFLEN            RF=L'FIELD                                   
         LR    R0,R4               R0 = CURRENT POSITION                        
         SR    R0,R2                                                            
         SHI   R0,8                - L'HEADER                                   
         SR    RF,R0               RF=CHARS REMAINING FOR INV COLUMN            
         STC   RF,INVCOLW          SAVE L'INV COLUMN                            
*                                                                               
         LA    RE,DASHES           YES, POINT RE TO DASHES                      
         CLI   BYTE,C'-'           DOING UNDERLINES?                            
         BE    *+12                                                             
         LA    RE,2(R3)            TITLE                                        
         IC    RF,1(R3)                                                         
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RE)       MOVE COL TITLE TO LINE                       
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS FIELD HEADER                                           
* ON EXIT RF HAS FIELD LENGTH                                                   
GFLEN    ZIC   RF,0(R2)            LENGTH OF THE FIELD                          
         SHI   RF,8                MINUS L(FIELD HEADER)                        
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZR   RE                  NO - EXIT                                    
         SHI   RF,8                YES - SUBTRACT L(EXT HEADER)                 
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS FIELD HEADER                                           
XCFLD    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GFLEN                                                         
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         J     EQXIT                                                            
                                                                                
*                                                                               
*                                                                               
* R1 = COLUMN EQUATE                                                            
GETLDISP NTR1  BASE=*,LABEL=*                                                   
         LA    R2,COLLIST                                                       
*                                                                               
GETLD10  CLI   0(R2),X'FF'                                                      
         JE    NEQXIT                                                           
*                                                                               
         CLM   R1,1,0(R2)                                                       
         BE    *+12                                                             
         AHI   R2,2                                                             
         B     GETLD10                                                          
*                                                                               
         ZIC   RF,1(R2)                                                         
         CR    RB,RB                                                            
         XIT1  REGS=(RF)                                                        
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*&&DO                                                                           
PATCH    NTR1  BASE=*,LABEL=*                                                   
         L     RF,=A(DIE)                                                       
         A     RF,RELO                                                          
         MVI   0(RF),X'00'                                                      
         J     EQXIT                                                            
*&&                                                                             
*                                                                               
*                                                                               
*                                                                               
* COLUMN TABLE EQUATES                                                          
CTNWKQ   EQU   1                                                                
CTPRDQ   EQU   2                                                                
CTESTQ   EQU   3                                                                
CTREPQ   EQU   4                                                                
CTSPTSQ  EQU   5                                                                
CTGRSQ   EQU   6                                                                
CTNETQ   EQU   7                                                                
CTPGRSQ  EQU   8                                                                
CTINVQ   EQU   9                                                                
CTSDATQ  EQU   10                                                               
CTMPQ    EQU   11                                                               
*                                                                               
COLTAB   DS    0X                                                               
         DC    AL1(CTNWKQ),AL1(3),CL3'Nwk'                                      
         DC    AL1(CTPRDQ),AL1(7),CL7'Product'                                  
         DC    AL1(CTESTQ),AL1(3),CL3'Est'                                      
         DC    AL1(CTREPQ),AL1(3),CL3'Rep'                                      
         DC    AL1(CTSPTSQ),AL1(4),CL4'Spts'                                    
         DC    AL1(CTGRSQ),AL1(13),CL13'    Gross'                              
         DC    AL1(CTNETQ),AL1(13),CL13'     Net'                               
         DC    AL1(CTPGRSQ),AL1(13),CL13' Paid Gross'                           
         DC    AL1(CTINVQ),AL1(8),CL8'Invoices'                                 
         DC    AL1(CTSDATQ),AL1(5),CL5'SDate'                                   
         DC    AL1(CTMPQ),AL1(2),CL2'MP'                                        
         DC    X'FF'                                                            
                                                                                
*                                                                               
*                                                                               
*                                                                               
EZ25WKD  DSECT                                                                  
*                                                                               
VGETRATE DS    A                                                                
VSTAVAL  DS    A                                                                
VSTAPACK DS    A                                                                
VGETBRD  DS    A                                                                
VOFFICER DS    A                                                                
VTSAR    DS    A                                                                
VRCPACK  DS    A                                                                
VMINIO   DS    A                                                                
ACOLTAB  DS    A                                                                
*                                                                               
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
ADJ      DS    F                                                                
*                                                                               
NINVFLAG DS    XL1                                                              
NFNETDET EQU   X'80'               NETWORK IN DETAILS                           
NFPRDDET EQU   X'40'               PRODUCT IN DETAILS                           
NFESTDET EQU   X'20'               ESTIMATE IN DETAILS                          
*                                                                               
ERRNUM   DS    XL2                                                              
SVAPROF  DS    CL20                SAVED AGY PROFILE                            
QSTANEW  DS    CL8                                                              
CURSREP  DS    CL4                                                              
CURRMKT  DS    CL4                                                              
CURRSTA  DS    CL5                                                              
CURRNET  DS    CL3                                                              
STAPRNTN DS    CL8                                                              
BMOS     DS    XL2                                                              
BSTART   DS    XL2                                                              
BEND     DS    XL2                                                              
BWEEKS   DS    XL10                5 WEEKS                                      
*                                                                               
*                                                                               
I2PROF   DS    XL16                                                             
SVOFC    DS    C                                                                
SVOFC2   DS    CL2                                                              
SVCACCS  DS    CL3                 CLIENT LIMIT ACCESS CODES                    
SVMACCS  DS    CL3                 MARKET LIMIT ACCESS CODES                    
*TSARBLK  DS    CL(TSARDL2)                                                     
TSARBLK  DS    CL(TSPXTNL)                                                      
TSREC    DS    CL(BLINEDLQ)                                                     
SVTSKEY  DS    CL(BLKLQ)                                                        
INVCOLW  DS    X                                                                
*                                                                               
*                                                                               
FBPARMS  DS    0X                                                               
FBNET    DS    CL3                                                              
FBPRD1   DS    CL3                                                              
FBPRD2   DS    CL3                                                              
FBEST    DS    X                                                                
FBINV    DS    CL10                                                             
*                                                                               
FBFLAG   DS    X                   01-READ SEQ                                  
FBFSEQQ  EQU   X'01'                                                            
FBFDONEQ EQU   X'02'                                                            
*                                  02-DONE                                      
FBPARMLQ EQU   *-FBPARMS                                                        
*                                                                               
*                                                                               
COLLIST  DS    XL41                20X(1 - EQU, 1 - DISP)+X'FF'                 
DASHES   DS    CL132                                                            
MELEM    DS    XL128               MINIO ELEMENT                                
MINBLOCK DS    XL532               MINIO CONTROL BLOCK                          
*                                                                               
SNVMRTAB DS    XL14336                                                          
*                                                                               
EZ25WKLQ EQU   *-EZ25WKD                                                        
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE SPEZFFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPEZFF4D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE SPEZFSYSD                                                      
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDTSBUFFD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPSNVFEQTB                                                     
       ++INCLUDE FATCB                                                          
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
* SAVED STORAGE                                                                 
*                                                                               
SYSD     DSECT                                                                  
         ORG   WRKFREC                                                          
MISCFLG1 DS    XL1                                                              
MF1KYCHG EQU   X'80'               A KEY FIELD WAS CHANGED                      
MF1CABLE EQU   X'40'               CABLE STATION                                
*                                                                               
OPTFLAG  DS    XL1                                                              
OFAGAINQ EQU   X'80'                                                            
OFCALQ   EQU   X'40'                                                            
OFZEROQ  EQU   X'20'               DISPLAY ZERO-DOLLAR SPOTS                    
OFPAIDQ  EQU   X'10'               PAID                                         
OFWEEKQ  EQU   X'08'               WEEKLY                                       
*                                                                               
SVTSREC  DS    CL(BLINEDLQ)                                                     
TSARFLG  DS    C                                                                
*                                                                               
*                                                                               
*                                                                               
T230FFD  DSECT                                                                  
         ORG   T230FFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
*                                                                               
*                                                                               
BLINED   DSECT                                                                  
BLNETWK  DS    CL3                                                              
BLPRD    DS    CL3                                                              
BLPRD2   DS    CL3                                                              
BLEST    DS    X                                                                
BLSDATE  DS    XL2                 START DATE (FOR WEEKLY OPTION)               
BLSREP   DS    CL3                                                              
BLKLQ    EQU   *-BLINED                                                         
BLFLAG   DS    X                                                                
BLFALPDQ EQU   X'80'               ALL BUYS PAID                                
BLFNOPDQ EQU   X'40'               NO BUYS PAID                                 
BLFMATQ  EQU   X'20'               ALL BUYS MATCHED                             
*                                                                               
BLSPOTS  DS    XL4                                                              
BLGROSS  DS    PL8                                                              
BLNET    DS    PL8                                                              
         ORG   BLNET                                                            
BLPGROSS DS    PL8                                                              
BLINVS   DS    CL100               10 10-CHAR INVOICES                          
BLINEDLQ EQU   *-BLINED                                                         
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080SPEZF25   12/26/12'                                      
         END                                                                    

*          DATA SET SPOMS02    AT LEVEL 032 AS OF 01/03/07                      
*PHASE T23402A                                                                  
T23402   TITLE 'SPOMS02 - REPORT OF PENDING ORDERS'                             
T23402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23402*,R7,RR=R3                                              
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
         NI    ORPBYRTH+1,X'F7'                                                 
*        NI    ORPCLTTH+1,X'F7'                                                 
         TM    WHEN,X'20'          SOON REQUEST?                                
         BZ    *+12                                                             
         OI    ORPBYRTH+1,X'08'                                                 
*        OI    ORPCLTTH+1,X'08'                                                 
         OI    ORPBYRTH+6,X'80'                                                 
*        OI    ORPCLTTH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BE    PR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
*                                                                               
         LA    R2,ORPMEDH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK10                                                             
*                                                                               
         MVI   GERROR1,REQFIELD                                                 
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
         BE    INFEXIT                                                          
*                                                                               
         GOTO1 VALIMED             VALIDATE MEDIA                               
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VK10     DS    0H                                                               
         LA    R2,ORPBUYRH         BUYER                                        
         XC    BUYRFILT,BUYRFILT                                                
         ZICM  R3,5(R2),1          LENGTH OF INPUT                              
         BZ    VK020                                                            
*                                                                               
VK15     MVI   GERROR1,INVALID                                                  
         GOTO1 VALIBUYR,DMCB,8(R2) VALIDATE IT                                  
         BNE   MYERREX                                                          
*                                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BUYRFILT(0),8(R2)   SAVE BUYER FILTER                            
         OC    BUYRFILT,SPACES                                                  
*                                                                               
VK020    LA    R2,ORPCLNTH         CLIENT                                       
         XC    CLTFILT,CLTFILT                                                  
         ZICM  R3,5(R2),1          LENGTH OF INPUT                              
         BZ    VK030                                                            
*                                                                               
VK025    GOTO1 VALICLT                                                          
         MVC   CLTFILT,BCLT                                                     
         OI    FILTERS,FCLIENT                                                  
*                                                                               
VK030    MVI   GERROR1,INVALID                                                  
         LA    R2,ORPREPH          REP                                          
*                                                                               
         LA    R1,IDTABLE                                                       
         ST    R1,NEXTID           INITIALIZE NEXT                              
*                                                                               
         ZICM  R1,5(R2),1          LENGTH OF INPUT                              
         BZ    VK040               LENGTH = 0, NO FILTER                        
         CLI   5(R2),3             CHECK IF LENGTH IS AT LEAST 3                
         BL    MYERREX                                                          
         BAS   RE,GETIDNUM                                                      
*                                                                               
VK040    LA    R2,ORPOFFRH                                                      
         ZICM  R1,5(R2),1          LENGTH OF INPUT                              
         BZ    INFEXIT             NO MAKEGOOD FILTER                           
         MVI   MISCFLG2,0                                                       
         CLI   8(R2),C'N'                                                       
         BE    VK045                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   MYERREX                                                          
         OI    MISCFLG2,MF2MKGD                                                 
         B     VK045                                                            
*                                                                               
VK045    LA    R2,ORPFILTH         VAL. FILTERS: DEFAULT ALL FILTERS ON         
         MVI   FILTERS,FSENT+FDELIVRD                                           
         MVI   FILTER2,F2APPRVD                                                 
         MVI   FILTER2A,0                                                       
         MVI   FILTER3,0                                                        
         MVI   FILTER3A,0                                                       
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
         BE    VKX                                                              
         GOTO1 SCANNER,DMCB,(R2),(X'84',BLOCK)   <---- MAXIMUM OF 4             
*                                                                               
         LA    R3,BLOCK                                                         
VK050    ZICM  R1,0(R3)            LENGTH OF FIRST FIELD                        
         BZ    VKX                                                              
         BCTR  R1,0                                                             
                                                                                
         EX    R1,*+8                                                           
         BNE   VK070                                                            
         CLC   12(0,R3),=CL5'DATE'   DATE FILTER?                               
*                                                                               
         TM    FILTERS,FDATE       CHECK IF DATE FILTER ALREADY ENTERED         
         BO    CURSERR                                                          
*                                                                               
         ZIC   R1,1(R3)            LENGTH OF DATE ENTERED                       
*                                                                               
         LA    RE,22(R3)           ADDRESS OF DATE FOR FILTER                   
         ST    RE,DMCB                                                          
         STC   R1,DMCB             LENGTH OF DATE                               
         OI    DMCB,X'40'                                                       
*                                                                               
         GOTO1 PERVAL,DMCB,,MYBLOCK                     CONVERT PWOS            
         MVI   ERROR,INVALID                                                    
         TM    4(R1),PVRCINV1                           DATE INVALID?           
         BO    CURSERR                                                          
         LA    R4,MYBLOCK                                                       
         USING PERVALD,R4                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(19,DATEFILT)   PWOS JULIAN             
         DROP  R4                                                               
*                                                                               
         OI    FILTERS,FDATE       THERE IS A VALID DATE FILTER                 
         B     VK100                                                            
*                                                                               
VK070    EX    R1,*+8                                                           
         BNE   VK090                                                            
         CLC   12(0,R3),=C'STATUS' STATUS FILTER?                               
*                                                                               
         TM    MISCFLG2,MF2MKGD                                                 
         BNZ   VK080                                                            
*                                                                               
         TM    FILTERS,FSTATUS     THERE ARE FILTERS ON STATUS                  
         BO    *+16                DON'T TURN ANY BITS OFF                      
         NI    FILTERS,X'FF'-FSENT-FDELIVRD                                     
         NI    FILTER2,X'FF'-F2APPRVD                                           
         OI    FILTERS,FSTATUS                                                  
*                                                                               
         CLC   =C'*SENT',22(R3)    STATUS = *SENT?                              
         BNE   *+12                                                             
         OI    FILTERS,FSENT       YES                                          
         B     VK100                                                            
*                                                                               
         CLC   =C'SENT',22(R3)     STATUS = DELIVERED?                          
         BNE   *+12                                                             
         OI    FILTERS,FDELIVRD    YES                                          
         B     VK100                                                            
*                                                                               
         CLC   =C'DEL',22(R3)      STATUS = DELIVERED?                          
         BNE   *+12                                                             
         OI    FILTERS,FDELIVRD    YES                                          
         B     VK100                                                            
*                                                                               
         CLC   =C'OPEN',22(R3)     STATUS = OPENED?                             
         BNE   *+12                                                             
         OI    FILTER2,F2APPRVD    YES                                          
         B     VK100                                                            
*                                                                               
         CLC   =C'CONF',22(R3)     STATUS = CONFIRMED?                          
         BNE   *+12                                                             
         OI    FILTER2,F2CNFM                                                   
         B     VK100                                                            
*                                                                               
         CLC   =C'CFMPND',22(R3)   STATUS = CONFIRM PENDING?                    
         BNE   *+12                                                             
         OI    FILTER2A,F2CFMPND                                                
         B     VK100                                                            
*                                                                               
         CLC   =C'ERROR',22(R3)    STATUS = ORDER IN ERROR?                     
         BNE   *+12                                                             
         OI    FILTER2,F2ERROR                                                  
         B     VK100                                                            
*                                                                               
         CLC   =C'FAX',22(R3)      STATUS = ORDER WAS FAXED?                    
         BNE   *+12                                                             
         OI    FILTER2,F2FAX                                                    
         B     VK100                                                            
*                                                                               
         CLC   =C'REJ',22(R3)      STATUS = REJECTED BY REP?                    
         BNE   *+12                                                             
         OI    FILTER2,F2RJCT                                                   
         B     VK100                                                            
*                                                                               
         CLC   =C'NOTDARE',22(R3)  STATUS = NOT DARE ANYMORE?                   
         BNE   *+12                                                             
         OI    FILTER2,F2NODARE                                                 
         B     VK100                                                            
*                                                                               
         CLC   =C'RECALL',22(R3)   STATUS = RECALLED?                           
         BNE   *+12                                                             
         OI    FILTER2,F2RECALL                                                 
         B     VK100                                                            
*                                                                               
         CLC   =C'UNDARE',22(R3)   STATUS = UNDARE?                             
         BNE   *+12                                                             
         OI    FILTER2A,F2UNDARE                                                
         B     VK100                                                            
*                                                                               
         CLC   =C'UNSENT',22(R3)   STATUS = UNSENT?                             
         BNE   *+12                                                             
         OI    FILTER2,F2UNSENT                                                 
         B     VK100                                                            
*                                                                               
         CLC   =C'EMPTY',22(R3)    STATUS = EMPTY?                              
         BNE   *+12                                                             
         OI    FILTER2A,F2EMPTY                                                 
         B     VK100                                                            
*                                                                               
         CLC   =C'ACTOFR',22(R3)   STATUS = OFFERS?                             
         BNE   *+12                                                             
         OI    FILTER2A,F2OFFERS                                                
         B     VK100                                                            
*                                                                               
         CLC   =C'EMAIL',22(R3)    STATUS = EMAIL?                              
         BNE   *+12                                                             
         OI    FILTER2A,F2EMAIL                                                 
         B     VK100                                                            
*                                                                               
         CLC   =C'AMEND',22(R3)    STATUS = AMEND?                              
         BNE   *+12                                                             
         OI    FILTER2A,F2AMEND                                                 
         B     VK100                                                            
*                                                                               
         CLC   =C'ALL',22(R3)      STATUS = ALL?                                
         BNE   CURSERR                                                          
         B     VK100               YES, THOSE 3 BITS ARE OFF THEN               
*                                                                               
VK080    CLC   =C'APP',22(R3)      STATUS = APPROVED?                           
         BNE   *+12                                                             
         OI    FILTER3,F3APPRVD                                                 
         B     VK100                                                            
         CLC   =C'*APP',22(R3)     STATUS = *APPROVED?                          
         BNE   *+12                                                             
         OI    FILTER3A,F3APPRV1                                                
         B     VK100                                                            
         CLC   =C'SELRCL',22(R3)   STATUS = CANCELLED W/MORE 2 FOLLOW?          
         BNE   *+12                                                             
         OI    FILTER3,F3CANMOR                                                 
         B     VK100                                                            
         CLC   =C'CANCEL',22(R3)   STATUS = CANCELLED?                          
         BNE   *+12                                                             
         OI    FILTER3,F3CANCEL                                                 
         B     VK100                                                            
         CLC   =C'ERROR',22(R3)    STATUS = ERROR?                              
         BNE   *+12                                                             
         OI    FILTER3,F3ERROR                                                  
         B     VK100                                                            
         CLC   =C'*OK',22(R3)      STATUS = TO BE OKAYED?                       
         BNE   *+12                                                             
         OI    FILTER3,F3TOBOK                                                  
         B     VK100                                                            
         CLC   =C'ONHOLD',22(R3)   STATUS = ON HOLD?                            
         BNE   *+12                                                             
         OI    FILTER3,F3ONHOLD                                                 
         B     VK100                                                            
         CLC   =C'AMEND',22(R3)    STATUS = AMENDED?                            
         BNE   *+12                                                             
         OI    FILTER3,F3AMEND                                                  
         B     VK100                                                            
         CLC   =C'NEW',22(R3)      STATUS = NEW?                                
         BNE   *+12                                                             
         OI    FILTER3,F3NEW                                                    
         B     VK100                                                            
         CLC   =C'OK',22(R3)       STATUS = OKAYED?                             
         BNE   *+12                                                             
         OI    FILTER3A,F3OK                                                    
         B     VK100                                                            
         CLC   =C'*REJ',22(R3)     STATUS = *REJECTED?                          
         BNE   *+12                                                             
         OI    FILTER3A,F3RJCT1                                                 
         B     VK100                                                            
         CLC   =C'REJ',22(R3)      STATUS = REJECTED?                           
         BNE   CURSERR                                                          
         OI    FILTER3A,F3RJCT                                                  
         B     VK100                                                            
*                                                                               
VK090    EX    R1,*+8                                                           
         BNE   VK095                                                            
         CLC   12(0,R3),=C'CITY'   CITY FILTER?                                 
         CLI   ORPREPH+5,0         MAKE SURE NO REP FILTER                      
         BNE   CURSERR                                                          
         MVC   CITY,22(R3)         SAVE CITY CODE                               
         OI    FILTERS,FCITY                                                    
         B     VK100                                                            
*                                                                               
VK095    EX    R1,*+8                                                           
         BNE   CURSERR                                                          
         CLC   12(0,R3),=C'PB'     PAGE BREAK AT NEW REP?                       
         CLI   22(R3),C'Y'                                                      
         BNE   *+12                                                             
         OI    FILTERS,PAGEBRAK                                                 
         B     VK100                                                            
         CLI   22(R3),C'N'                                                      
         BNE   CURSERR                                                          
*                                                                               
VK100    LA    R3,32(R3)                                                        
         B     VK050                                                            
*                                                                               
VKX      TM    WHEN,X'20'          SOON REQUEST?                                
         BZ    XIT                                                              
         OC    BUYRFILT,BUYRFILT                                                
         BNZ   XIT                                                              
         TM    FILTERS,FDATE       CHECK IF DATE FILTER ALREADY ENTERED         
         BNZ   XIT                                                              
         MVI   GERROR1,MISSING                                                  
         LA    R2,ORPBUYRH         BUYER                                        
         B     MYERREX                                                          
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
*                                                                               
         XC    SAVEBUYR,SAVEBUYR                                                
         MVI   RCSUBPRG,0          ORDER REPORT                                 
         TM    MISCFLG2,MF2MKGD                                                 
         BZ    *+8                                                              
         MVI   RCSUBPRG,1          MAKEGOOD NOTICE REPORT                       
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         TM    FILTERS,FCITY       FILTER ON CITY?                              
         BNO   *+8                                                              
         BAS   RE,GETIDNUM         GET REP ID'S FOR THAT CITY                   
*                                                                               
         TM    MISCFLG2,MF2MKGD    MAKEGOOD REPORT?                             
         BZ    PR05                NO                                           
         LA    R4,KEY                                                           
         USING MNKEY,R4                                                         
         XC    KEY,KEY                                                          
         MVI   MNKTYPE,MNKTYPQ     X'0D'                                        
         MVI   MNKSUBTY,MNKSTYPQ   X'36'                                        
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,BUYRFILT                                                  
         B     PR10                                                             
         DROP  R4                                                               
*                                                                               
PR05     LA    R4,KEY                                                           
         USING DAREORDD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   DBKTYPE,DBKTYPQ                                                  
         MVI   DBKSUBTY,DBKSTYPQ                                                
         MVC   DBKAGMD,BAGYMD      BINARY AGENCY MEDIA CODE                     
         MVC   DBKBYR,BUYRFILT     BUYER FILTER                                 
         DROP  R4                                                               
*                                                                               
PR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR30     CLC   KEY(3),KEYSAVE      AGENCY/MEDIA?                                
         BNE   PRX                 EXIT                                         
         MVC   SAVEKEY,KEY         SAVE BUYER OR NOTICE KEY                     
*                                                                               
         CLI   ORPBUYRH+5,0        IS THERE A BUYER FILTER                      
         BE    PR40                                                             
         CLC   BUYRFILT,KEY+3                                                   
         BNE   PRX                                                              
*                                                                               
PR40     TM    MISCFLG2,MF2MKGD    MAKEGOOD REPORT?                             
         BZ    PR42                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              YES, READ THE NOTICE REC INTO AIO3           
         MVC   AIO,AIO2                                                         
*                                                                               
         CLC   BINORDER,KEY+6      SAME ORDER?                                  
         BE    PR44                YES, DON'T READ IT AGAIN!                    
*                                                                               
* READ THE ORDER RECORD!                                                        
         MVI   KEY+1,X'B4'         BUYER KEY                                    
         XC    KEY+10(3),KEY+10    CLEAR THE GROUPCODE                          
         GOTO1 HIGH                RDHI THE ORDER KEY                           
*                                                                               
         CLC   KEY(10),KEYSAVE     DID WE GET THE ORDER?                        
****     BE    *+6                                                              
****     DC    H'0'                                                             
         BNE   PRX                                                              
*                                                                               
PR42     GOTO1 GETREC              READ THE ORDER RECORD INTO AIO2              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PR44     MVC   P,SPACES            CLEAR LINE                                   
*                                                                               
         L     R6,AIO                                                           
         USING DAREORDD,R6                                                      
         MVC   BINORDER,DOKORDER                                                
         LA    R2,PORDRNO                                                       
         BAS   RE,SHWORDER         PRINT ORDER NUMBER                           
*                                                                               
         XC    BMKT,BMKT                                                        
         MVC   BSTA,DOKSTA                                                      
         GOTO1 MSUNPK,DMCB,BMKTSTA,PMKT,PSTA  PRINT STATION AND MARKET          
*                                                                               
         MVI   ELCODE,DOIDELQ      GET THE ID ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   BADRECRD                                                         
         USING DOIDELD,R6                                                       
         MVC   PBUYER,DOIDBYR      PRINT BUYER                                  
         TM    FILTERS,FCLIENT                                                  
         BZ    *+14                                                             
         CLC   CLTFILT,DOIDCLT                                                  
         BNE   PR270                                                            
         GOTO1 CLUNPK,DMCB,DOIDCLT,QCLT                                         
         MVC   PCLT,QCLT           PRINT CLIENT                                 
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         XC    FAKEHDR,FAKEHDR                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         LA    R2,FAKEHDR          VALIDATE STATION TO GET MARKET NAME          
         MVI   5(R2),L'PSTA        STORE INPUT LENGTH IN FIELD HEADER           
         OI    4(R2),X'04'         MAKE ALPHA                                   
         MVC   FAKEFLD(L'PSTA),PSTA                                             
*                                                                               
         MVC   QCLT,=C'000'                                                     
         GOTO1 VALISTA             AND NUMERIC                                  
         MVC   PMKT(L'QMKT),QMKT                                                
         MVC   PMKN,MKTNM                                                       
         MVC   PSTA(4),STAPRNT                                                  
         CLI   PSTA+3,C'-'                                                      
         BNE   *+8                                                              
         MVI   PSTA+3,C' '                                                      
*                                                                               
         MVC   BCLT,DOIDCLT        CLIENT                                       
         MVC   BPRD,DOIDPRD        PRODUCT                                      
         GOTO1 GETQPRD                                                          
         BNE   BADRECRD                                                         
         MVC   PPRD(L'QPRD),QPRD   PRINT PRODUCT                                
*                                                                               
         CLI   DOIDPRD2,0          DISPLAY THE PIGGYBACK IF ANY                 
         BE    PR45                                                             
         MVC   BPRD,DOIDPRD2                                                    
         GOTO1 GETQPRD                                                          
         BNE   BADRECRD                                                         
         MVI   PPRD+3,C'-'                                                      
         MVC   PPRD2(L'QPRD),QPRD                                               
*                                                                               
PR45     EDIT  (B1,DOIDEST),(3,PEST),FILL=0                                     
         MVC   AIO,AIO2            RESTORE AIO                                  
*                                                                               
PR46     L     R6,AIO                                                           
         MVI   MISCFLG1,0                                                       
         MVI   ELCODE,DOI2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR48                                                             
         USING DOI2ELD,R6                                                       
         TM    DOI2FLG1,DOI2FVAR   VAR ORDER YET?                               
         BZ    *+8                                                              
         OI    MISCFLG1,MF1VAROR   YES                                          
*                                                                               
PR48     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR50                                                             
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENT?                        
         BZ    *+8                                                              
         OI    MISCFLG1,MF1CNFCM   YES                                          
*                                                                               
         CLI   DOSPREVN,0          GOT A REVISION NUMBER?                       
         BE    *+8                                                              
         OI    MISCFLG1,MF1REVOR   YES, REVISED ORDER                           
*                                                                               
PR50     XC    HALF,HALF                                                        
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
P51      CLI   0(R6),0             NO TRANSMISSION?                             
         BE    PR54                YES, ORDER IS UNSENT                         
*                                                                               
         CLI   0(R6),DOSTELQ       (NEW) TRANSMISSION ELEMENT X'12'             
         BH    PR54                ORDER IS UNSENT                              
         BE    PR56                                                             
*                                                                               
         CLI   0(R6),DORPELQ       REP CHANGE ELEM?                             
         BNE   PR53                                                             
         USING DOREPELD,R6                                                      
         MVC   HALF,DORPNREP       SAVE REP ID IF ANY                           
         DROP  R6                                                               
*                                                                               
PR53     ZIC   R0,1(R6)            BUMP TO NEXT ELEM                            
         AR    R6,R0                                                            
         B     P51                                                              
*                                                                               
PR54     DS    0H                                                               
         TM    MISCFLG2,MF2MKGD    MAKEGOOD REPORT?                             
         BNZ   PR270               YES, DON'T REPORT ON UNSENT ORDERS           
*                                                                               
         TM    FILTERS,FCITY       IS THERE A FILTER FOR CITY?                  
         BO    PR270               YES, AND NOT A MATCH SO GET NEXT             
         CLI   ORPREPH+5,0         IS THERE A REP FILTER?                       
         BNE   PR270                                                            
         MVC   PREP,SPACES                                                      
         TM    FILTER2,F2UNSENT                                                 
         BNZ   PR55                                                             
         CLI   FILTER2,0           FILTER FOR STATUS?                           
         BNE   PR270               SKIP, THIS IS UNSENT STATUS                  
         CLI   FILTER2A,0          FILTER FOR STATUS?                           
         BNE   PR270               SKIP, THIS IS UNSENT STATUS                  
         TM    FILTERS,FSENT+FDELIVRD FILTER SNT/DELIVRED?                      
         BNZ   PR270               SKIP, THIS IS UNSENT STATUS                  
PR55     MVC   PSTAT,=CL9'UNSENT'                                               
         B     PR220                                                            
*                                                                               
         USING DOSTELD,R6                                                       
PR56     ST    R6,R6SAVE           SAVE R6 ADDRESS TO COME BACK LATER           
*                                                                               
PR58     CLI   0(R6),0             END OF RECORD?                               
         BE    PR63                                                             
         CLI   0(R6),DOSTELQ       X'12'?                                       
         BL    PR59                LOW, SKIP TO NEXT ELEM                       
         BH    PR63                HIGH, NO MORE X'12'                          
*                                                                               
         CLI   DOSTSTAT,DDLVRD     X'82'                                        
         BE    PR60                                                             
         CLI   DOSTSTAT,DFXDLVD    X'84'                                        
         BE    PR60                                                             
         CLI   DOSTSTAT,DEMDLVD    X'86'                                        
         BE    PR60                                                             
PR59     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PR58                                                             
*                                                                               
PR60     OC    HALF,HALF           DID THE REP CHANGE?                          
         BNZ   *+10                YES                                          
         MVC   HALF,DOSTIDNM       NO, SAVE REP                                 
*                                                                               
PR63     L     R6,R6SAVE           RESTORE R6 TO FIRST X'12'                    
*                                                                               
PR65     TM    REPFLAG,EXMATCH     EXACT REP ID GIVEN?                          
         BZ    PR68                NO CHECK FOR SIMILAR ONES                    
         CLC   HALF,BINREP         SAME REP?                                    
         BNE   PR270               NO, GET NEXT RECORD                          
         B     PR80                                                             
*                                                                               
PR68     OC    HALF,HALF           IS THERE A REP ID?                           
         BNZ   PR70                YES                                          
         TM    FILTERS,FCITY       IS THERE A FILTER FOR CITY?                  
         BO    PR270               YES, AND NOT A MATCH SO GET NEXT             
         CLI   ORPREPH+5,0         IS THERE A REP FILTER?                       
         BNE   PR270                                                            
         MVC   CHRREP,SPACES                                                    
         B     PR80                                                             
*                                                                               
PR70     DS    0H                                                               
         CLI   DOSTSTAT,QEMPTY                                                  
         BNE   PR75                                                             
         MVC   CHRREP,SPACES                                                    
         B     PR80                                                             
*                                                                               
PR75     DS    0H                                                               
         MVC   BINREP,HALF                                                      
         BAS   RE,SHWIDNUM         LOOK UP REP ID                               
         TM    REPFLAG,NOMATCH     WAS THERE A MATCH?                           
         BO    PR270               NO, GET NEXT RECORD                          
*                                                                               
PR80     DS    0H                                                               
         TM    MISCFLG2,MF2MKGD    MAKEGOOD REPORT?                             
         BNZ   PR150               YES                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1DLVRD                                          
         CLI   DOSTSTAT,DDLVRD     X'82'                                        
         BNE   PR81                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         OI    MISCFLG1,MF1DLVRD                                                
*                                                                               
PR81     CLI   FILTER2,0           FILTER FOR STATUS?                           
         BNE   PR85                -YES                                         
         CLI   FILTER2A,0          FILTER FOR STATUS?                           
         BNE   PR85                -YES                                         
         TM    FILTERS,FSENT+FDELIVRD NO:FILTER SENT/DELIVERED?                 
         BZ    PR100               -NO:SHOW ALL STATUS                          
*** IF IT'S NOT SHOW ALL STATUSES, PROCEED LIKE NORMAL                          
*                                                                               
PR85     CLI   DOSTSTAT,QAPP       APPROVED?                                    
         BNE   *+12                                                             
         TM    FILTER2,F2APPRVD    INCLUDE APPROVED                             
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,QCFMDPND   CONFIRM PENDING?                             
         BNE   *+12                                                             
         TM    FILTER2A,F2CFMPND                                                
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,QCFMD      CONFIRMED?                                   
         BNE   PR87                                                             
         TM    FILTER2,F2CNFM                                                   
         BO    PR99                                                             
         TM    FILTER2A,F2OFFERS                                                
         BNO   PR99                                                             
         BAS   RE,OFFERCHK                                                      
         B     PR99                                                             
*                                                                               
PR87     CLI   DOSTSTAT,QBYRCNFM   BUYER CONFIRM?                               
         BNE   PR90                                                             
         TM    FILTER2,F2CNFM      (GOES WITH CONFIRM)                          
         BO    PR99                                                             
         TM    FILTER2A,F2OFFERS                                                
         BNO   PR99                                                             
         BAS   RE,OFFERCHK                                                      
         B     PR99                                                             
***********************************************************************         
OFFERCHK ST    R6,R6SAVE           SAVE ADDRESS OF R6                           
OFFER10  CLI   0(R6),0             NO MORE ELEMENTS?                            
         BE    OFFER30              - NOPE, NO MORE                             
         CLI   0(R6),MGCOLELQ      ANY MAKEGOODS (X'14')?                       
         BE    OFFER20                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     OFFER10                                                          
OFFER20  OI    MISCFLG1,MF1OFFER   TURN ON THE BIT                              
OFFER30  TM    MISCFLG1,MF1OFFER   SET THE CONDITION                            
         L     R6,R6SAVE           RESTORE R6                                   
         BR    RE                                                               
***********************************************************************         
*                                                                               
PR90     CLI   DOSTSTAT,QRCLUNKN   RECALLED-UNKNOWN?                            
         BE    *+12                                                             
         CLI   DOSTSTAT,QERRORED   ERRORED                                      
         BNE   *+12                                                             
         TM    FILTER2,F2ERROR                                                  
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,QFAXDLVD   FAX AND DELIVERED?                           
         BE    *+12                                                             
         CLI   DOSTSTAT,QFAXCNCL   FAX AND CANCELLED?                           
         BNE   *+12                                                             
         TM    FILTER2,F2FAX                                                    
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,QRJCT      REJECTED?                                    
         BNE   PR90D                                                            
         TM    FILTER2,F2RJCT                                                   
         BO    PR99                                                             
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   PR90C                                                            
         TM    FILTER2A,F2AMEND                                                 
         B     PR99                                                             
PR90C    TM    FILTER2,F2RJCT                                                   
         B     PR99                                                             
*                                                                               
PR90D    CLI   DOSTSTAT,QNODARE    NOT DARE?                                    
         BNE   *+12                                                             
         TM    FILTER2,F2NODARE                                                 
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,QUNDARE    UNDARED?                                     
         BNE   *+12                                                             
         TM    FILTER2A,F2UNDARE                                                
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,QEMPTY     EMPTY?                                       
         BNE   *+12                                                             
         TM    FILTER2A,F2EMPTY                                                 
         B     PR99                                                             
*                                                                               
         CLI   DOSTSTAT,DEMSENT    EMAIL SENT?                                  
         BNE   PR92                                                             
         TM    FILTER2A,F2EMAIL                                                 
         BO    PR99                                                             
         TM    FILTERS,FSENT                                                    
         B     PR99                                                             
*                                                                               
PR92     CLI   DOSTSTAT,DEMDLVD    EMAIL DELIVERED?                             
         BNE   PR93                                                             
         TM    FILTER2A,F2EMAIL                                                 
         BO    PR99                                                             
         TM    FILTERS,FDELIVRD                                                 
         B     PR99                                                             
*                                                                               
PR93     CLI   DOSTSTAT,DSENT      SENT?                                        
         BNE   PR94                                                             
         TM    MISCFLG1,MF1DLVRD   DID I GET A DELIVERY NOTICE?                 
         BO    *+12                YES                                          
         TM    FILTERS,FSENT       NO                                           
         B     PR99                                                             
         TM    FILTERS,FDELIVRD                                                 
         B     PR99                                                             
*                                                                               
PR94     CLI   DOSTSTAT,DFXSENT    FAX SENT?                                    
         BE    PR94E                                                            
         CLI   DOSTSTAT,DFXRSNT                                                 
         BNE   PR95                                                             
PR94E    TM    FILTER2,F2FAX                                                    
         BO    PR99                                                             
         TM    FILTERS,FSENT                                                    
         B     PR99                                                             
*                                                                               
PR95     CLI   DOSTSTAT,DFXDLVD    FAX DELIVERED?                               
         BNE   PR97                                                             
         TM    FILTER2,F2FAX                                                    
         BO    PR99                                                             
         TM    FILTERS,FDELIVRD                                                 
         B     PR99                                                             
*                                                                               
PR97     CLI   DOSTSTAT,QRECALL    RECALLED?                                    
         BE    PR97A                                                            
         CLI   DOSTSTAT,QRCLAPPR   RECALLED-APPROVED?                           
         BE    PR97A                                                            
         CLI   DOSTSTAT,QRCLCONF   RECALLED-CONFIRMED?                          
         BE    PR97A                                                            
         CLI   DOSTSTAT,QRCLDELN   RECALLED-DELIVERED?                          
         BE    PR97A                                                            
         CLI   DOSTSTAT,QRCLREJD   RECALLED-REJECTED?                           
         BE    PR97A                                                            
         CLI   DOSTSTAT,QRCLTRNS   RECALLED-TRANSMITTED?                        
         BE    PR97A                                                            
         CLI   DOSTSTAT,QRCLWIP    RECALLED-WORK IN PROGRESS?                   
         BNE   PR270                                                            
PR97A    TM    FILTER2,F2RECALL                                                 
*                                                                               
PR99     BNO   PR270                                                            
***************                                                                 
* DISREGARDING VAR ORDER AND REVISED ORDER STATUSES                             
***************                                                                 
PR100    LA    RE,STATTABL         REJECTED?                                    
*                                                                               
PR100A   CLI   0(RE),0             WE HAVE A STATUS THAT DOESN'T MATCH?         
         BE    PR100D              YES, COPY THE QUESTION MARKS                 
*                                                                               
         CLC   DOSTSTAT,0(RE)      MATCHES THE STATUS?                          
         BE    PR100B                                                           
         LA    RE,L'STATTABL(RE)   NO, CHECK NEXT STATUS                        
         B     PR100A                                                           
*                                                                               
PR100B   CLI   DOSTSTAT,DSENT                                                   
         BE    PR100C                                                           
         CLI   DOSTSTAT,QRECALL                                                 
         BNE   PR100D                                                           
PR100C   TM    MISCFLG1,MF1DLVRD   DID I GET A DELIVERY NOTICE?                 
         BZ    PR100D                                                           
         LA    RE,L'STATTABL(RE)   NO, CHECK NEXT STATUS                        
*                                                                               
PR100D   MVC   PSTAT,1(RE)                                                      
*                                                                               
PR105    DS    0H                                                               
         CLI   DOSTSTAT,QRJCT      REJECTED?                                    
         BNE   PR110                                                            
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVREJCTD'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    PR105A                                                           
         MVC   PSTAT,=CL9'VARREJCTD'                                            
*                                                                               
PR105A   CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   PR110                                                            
         MVC   PSTAT,=CL9'AMENDED'                                              
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVAMNDED'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    PR110                                                            
         MVC   PSTAT,=CL9'VARAMNDED'                                            
*                                                                               
PR110    DS    0H                                                               
         CLI   DOSTSTAT,QAPP       APPROVED?                                    
         BNE   PR115                                                            
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVOPENED'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    PR115                                                            
         MVC   PSTAT,=CL9'VAROPENED'                                            
*                                                                               
PR115    DS    0H                                                               
         CLI   DOSTSTAT,QCFMD      CONFIRMED?                                   
         BNE   PR120                                                            
*                                                                               
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVCONFRM'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'VARCONFRM'                                            
*                                                                               
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   PR115A                                                           
         TM    DOSTTYPE,DCNFMCOM   CONFIRM WITH COMMENTS?                       
         BO    PR115B                                                           
         B     PR115C                                                           
PR115A   TM    MISCFLG1,MF1CNFCM   CONFIRM WITH COMMENTS?                       
         BZ    PR115C                                                           
*                                                                               
PR115B   MVC   PSTAT,=CL9'***PCNFRM'                                            
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'***RCNFRM'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    PR115C                                                           
         MVC   PSTAT,=CL9'***VCNFRM'                                            
*                                                                               
PR115C   BAS   RE,OFFERCHK                                                      
         TM    MISCFLG1,MF1OFFER                                                
         BZ    PR120                                                            
         MVC   PSTAT,=CL9'OFFERS   '                                            
*                                                                               
PR120    DS    0H                                                               
         CLI   DOSTSTAT,QRECALL    RECALLED?                                    
         BNE   PR125                                                            
         TM    MISCFLG1,MF1DLVRD   AM I DELIVERED?                              
         BO    PR120B                                                           
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVRCLLNG'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    PR125                                                            
         MVC   PSTAT,=CL9'VARRCLLNG'                                            
         B     PR125                                                            
*                                                                               
PR120B   TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVRECALL'                                            
         TM    MISCFLG1,MF1VAROR                                                
         BZ    PR125                                                            
         MVC   PSTAT,=CL9'VARRECALL'                                            
*                                                                               
PR125    DS    0H                                                               
         CLI   DOSTSTAT,DSENT      SENT?  (X'81')                               
         BNE   PR135                                                            
         TM    MISCFLG1,MF1DLVRD   DID I GET A DELIVERY NOTICE?                 
         BO    PR130                                                            
         TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'*REVSENT'                                             
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'*VARSENT'                                             
         B     PR135                                                            
*                                                                               
PR130    TM    MISCFLG1,MF1REVOR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'REVSENT'                                              
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+10                                                             
         MVC   PSTAT,=CL9'VARSENT'                                              
*                                                                               
PR135    DS    0H                                                               
         CLI   DOSTSTAT,QSNTPNDG   SEND PENDING?  (C'X')                        
         BNE   PR140                                                            
         MVC   PSTAT,=CL9'SNDPNDING'                                            
*                                                                               
PR140    ST    R6,R6SAVE           SAVE R6 ADDRESS BEFORE LOOPING               
         LA    R2,DOSTDATE                                                      
PR140A   CLI   DOSTEL,DOSTELQ      X'12' NEW STATUS ELEMENT?                    
         BNE   PR145                - YES, END LOOP                             
         CLI   DOSTSTAT,DDLVRD     IS IT DELIVERED?                             
         BNE   PR140NXT             - NO, NEXT X'12' PLEASE                     
         LA    R2,DOSTDATE         SAVE THE DOSTDATE ADDRESS                    
         B     PR145                                                            
*                                                                               
PR140NXT ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PR140A                                                           
*                                                                               
PR145    L     R6,R6SAVE           HAVE TO RESTORE R6 ADDRESS                   
         B     PR200                                                            
         DROP  R6                                                               
*********                                                                       
** IF MAKEGOOD NOTICE RECORD EXISTS, FILTER STATUS' AND DATE                    
*********                                                                       
PR150    MVC   AIO,AIO3            RESTORE NOTICE RECORD                        
*                                                                               
         L     R6,AIO                                                           
         USING MNKEY,R6                                                         
         MVI   PSLASH,C'/'                                                      
         MVC   PGRPCODE,MNKGROUP                                                
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BADRECRD                                                         
         USING MNSTELD,R6                                                       
*                                                                               
         NI    MISCFLG2,X'FF'-MF2DELNT                                          
PR155    CLI   MNSTSTAT,MNSTDELV   STATUS = DELIVERED?                          
         BNE   PR160                                                            
         OI    MISCFLG2,MF2DELNT                                                
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT MG STATUS ELEMENT               
         B     PR155                                                            
*                                                                               
PR160    CLI   FILTER3,0                                                        
         BNE   *+12                                                             
         CLI   FILTER3A,0                                                       
         BE    PR175                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTAPP    STATUS = APPROVED                            
         BNE   PR165                                                            
         TM    MISCFLG2,MF2DELNT                                                
         BNZ   *+12                                                             
         TM    FILTER3A,F3APPRV1                                                
         B     PR170                                                            
         TM    FILTER3,F3APPRVD                                                 
         B     PR170                                                            
*                                                                               
PR165    CLI   MNSTSTAT,MNSTCANM   STATUS = CANCELLED W/MORE TO FOLLOW          
         BNE   *+12                                                             
         TM    FILTER3,F3CANMOR                                                 
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTCAN    STATUS = CANCELLED                           
         BNE   *+12                                                             
         TM    FILTER3,F3CANCEL                                                 
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTERR    STATUS = ERROR                               
         BNE   *+12                                                             
         TM    FILTER3,F3ERROR                                                  
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTGOIN   STATUS = GOING TO BE OKAYED                  
         BNE   *+12                                                             
         TM    FILTER3,F3TOBOK                                                  
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTHOLD   STATUS = ON HOLD                             
         BNE   *+12                                                             
         TM    FILTER3,F3ONHOLD                                                 
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTAMND   STATUS = AMENDED                             
         BNE   *+12                                                             
         TM    FILTER3,F3AMEND                                                  
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTNEW    STATUS = NEW                                 
         BNE   *+12                                                             
         TM    FILTER3,F3NEW                                                    
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTOKAY   STATUS = OKAYED                              
         BNE   *+12                                                             
         TM    FILTER3A,F3OK                                                    
         B     PR170                                                            
*                                                                               
         CLI   MNSTSTAT,MNSTREJ    STATUS = REJECTED                            
         BNE   PR260                                                            
         TM    MISCFLG2,MF2DELNT                                                
         BNZ   *+12                                                             
         TM    FILTER3A,F3RJCT1                                                 
         B     PR170                                                            
         TM    FILTER3A,F3RJCT                                                  
*                                                                               
PR170    BNO   PR260                                                            
*                                                                               
PR175    DS    0H                                                               
         CLI   MNSTSTAT,MNSTAPP                                                 
         BNE   PR180                                                            
         MVC   PSTAT,=CL9'APPROVED'                                             
         TM    MISCFLG2,MF2DELNT                                                
         BNZ   PR190A                                                           
         MVC   PSTAT,=CL9'*APPROVED'                                            
         B     PR190A                                                           
*                                                                               
PR180    DS    0H                                                               
         CLI   MNSTSTAT,MNSTREJ                                                 
         BNE   PR185                                                            
         MVC   PSTAT,=CL9'REJECTED'                                             
         TM    MISCFLG2,MF2DELNT                                                
         BNZ   PR190A                                                           
         MVC   PSTAT,=CL9'*REJECTED'                                            
         B     PR190A                                                           
*                                                                               
PR185    LA    RE,STATTBL2                                                      
PR185A   CLI   0(RE),0             NO STATUS MATCH?                             
         BE    PR190               NO -- MOVE IN QUESTION MARKS                 
*                                                                               
         CLC   MNSTSTAT,0(RE)      STATUS MATCH?                                
         BE    PR190               YES-MOVE IN STATUS                           
         AHI   RE,L'STATTBL2       NO-BUMP                                      
         B     PR185A                                                           
PR190    MVC   PSTAT,1(RE)                                                      
PR190A   LA    R2,MNSTDATE                                                      
*                                                                               
PR200    TM    FILTERS,FDATE       IS THERE A DATE FILTER?                      
         BNO   PR210                                                            
         CLC   DATEFILT,0(R2)                                                   
         BNL   PR210                                                            
         TM    MISCFLG2,MF2MKGD    READING FOR MAKEGOODS?                       
         BO    PR260               YES                                          
         B     PR270                                                            
*                                                                               
PR210    GOTO1 DATCON,DMCB,(8,(R2)),(8,PDATE)                                   
*                                                                               
         MVC   PREP,CHRREP                                                      
*                                                                               
PR220    CLI   LINE,99             FIRST LINE ON PAGE?                          
         BE    PR227                                                            
*                                                                               
PR225    CLC   PBUYER,SAVEBUYR     SAME BUYER?                                  
         BE    PR227                                                            
*                                                                               
         TM    FILTERS,PAGEBRAK    PAGE BREAK INSTEAD FOR BUYER CHANGE          
         BZ    PR230                                                            
         MVI   FORCEHED,C'Y'                                                    
PR227    MVC   SAVEBUYR,PBUYER                                                  
         B     PR250                                                            
*                                                                               
PR230    MVC   SAVEBUYR,PBUYER                                                  
         MVC   P2,P                SKIP A LINE                                  
         CLI   LINE,57             CHECK IF LAST LINE                           
         BNE   PR240                                                            
         MVC   P,SPACES                                                         
         B     PR250                                                            
*                                                                               
PR240    MVI   P+1,C'*'            SEPARATE DIFFENT BUYERS W/LINE               
         MVC   P+2(PLINELNQ-2),P+1                                              
*                                                                               
PR250    GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
*                                                                               
PR260    TM    MISCFLG2,MF2MKGD    READ NEXT MAKEGOORD RECORD?                  
         BZ    PR270               NO: SKIP                                     
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         B     PR280                                                            
*                                                                               
PR270    NI    REPFLAG,X'FF'-NOMATCH                                            
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         TM    MISCFLG2,MF2MKGD                                                 
         BZ    *+10                                                             
         MVC   KEY+10(3),=3X'FF'   SKIP OFFERS FOR THIS ORDER                   
         GOTO1 HIGH                                                             
         TM    MISCFLG2,MF2MKGD                                                 
         BO    PR20                                                             
PR280    CLC   KEY(25),KEYSAVE                                                  
         BE    PR20                                                             
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
PRX      NI    REPFLAG,X'FF'-EXMATCH-NOMATCH-FNDMATCH                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT THE RECORD BEFORE DIEING                                                
***********************************************************************         
BADRECRD GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
* LOOKS UP THE ID NUMBER BASED ON THE ID NAME                                   
*                                                                               
* ON ENTRY:    ORPREP              CHARACTER ID NAME                            
*              OR CITY             CHARACTER CITY CODE                          
*                                                                               
* ON EXIT:     BINREP              BINARY ID NUMBER                             
*              'KEY' & 'KEYSAVE' GOT CLOBBERED                                  
*              AIO1 WAS USED                                                    
*              'ELCODE' GOT CLOBBERED                                           
***********************************************************************         
GETIDNUM NTR1                                                                   
         LA    R3,IDTABLE                                                       
         L     R1,DAREREPS         TABLE OR REP IDS                             
         ST    R1,NEXTREP                                                       
*                                                                               
         TM    FILTERS,FCITY       IS THERE A CITY FILTER?                      
         BNO   GIDN6                                                            
*                                                                               
GIDN2    L     R1,NEXTREP          READ TABLE OF REPS TO APPEND W/ CITY         
         CLI   0(R1),X'FF'                                                      
         BE    GIDNX                                                            
         CLI   13(R1),0            TEST BITS ON IN TABLE                        
         BE    GIDN4                                                            
         LA    R1,25(R1)           NEXT TABLE ENTRY                             
         ST    R1,NEXTREP                                                       
         B     GIDN2                                                            
*                                                                               
GIDN4    MVC   CHRREP,15(R1)       BUILD REP ID WITH CITY CODE                  
         ZIC   R0,14(R1)           LENGTH OF ID                                 
         LA    R1,25(R1)           NEXT TABLE ENTRY                             
         ST    R1,NEXTREP                                                       
         LA    R1,CHRREP                                                        
         AR    R1,R0                                                            
         MVC   0(2,R1),CITY        APPEND CITY CODE                             
         B     GIDN8                                                            
*                                                                               
GIDN6    LA    R2,ORPREPH          REP FILTER FROM SCREEN                       
         MVC   CHRREP,SPACES                                                    
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CHRREP(0),8(R2)     REP ID FILTER                                
*                                                                               
GIDN8    XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CHRREP       CHARACTER REP                                
         DROP  R4                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R4),AIO1                 
GIDN10   MVI   GERROR1,INVALID                                                  
         CLI   8(R1),0                                                          
         BNE   MYERREX                                                          
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
*                                                                               
         CLI   0(R6),CTIKTYPQ      ID RECORD?                                   
         BE    GIDN12                                                           
         TM    FILTERS,FCITY       NO, FILTER FOR CITY                          
         BO    GIDNX                                                            
         TM    REPFLAG,FNDMATCH    NO, FOUND AT LEAST ONE MATCH?                
         BNO   MYERREX             NO, DOES NOT EXIST                           
*                                                                               
GIDN12   MVC   CHRREP,CTIKID       SAVE CHARACTER REP ID                        
         CLC   CTIKID,SAVEKEY+15   CHECK IF EXACTLY SAME ID                     
         BNE   GIDN15                                                           
         TM    FILTERS,FCITY       FILTER FOR CITY                              
         BO    GIDN20                                                           
         OI    REPFLAG,EXMATCH     YES                                          
         B     GIDN20                                                           
*                                                                               
GIDN15   TM    FILTERS,FCITY       FILTER FOR CITY                              
         BO    GIDN2               TRY NEXT TABLE ENTRY                         
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    GIDN20                                                           
         CLC   CTIKID(0),SAVEKEY+15   COMPARE ID RETURNED                       
*                                                                               
         TM    REPFLAG,FNDMATCH    FOUND AT LEAST ONE MATCH                     
         BO    GIDNX                                                            
         B     MYERREX                                                          
*                                                                               
GIDN20   LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        LOOK FOR THE DESCRIPTION ELEMENT             
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTDSCD,R6                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                RAN OUT OF ROOM IN ID TABLE                  
*                                                                               
         MVC   BINREP,CTDSC        SAVE BINARY REP ID                           
         TM    REPFLAG,EXMATCH     EXACT MATCH FOUND                            
         BO    GIDNX               YES, DONE                                    
*                                                                               
         MVC   0(2,R3),CTDSC       PUT IDNUM IN TABLE                           
         MVC   2(10,R3),CHRREP     PUT ID IN TABLE                              
         LA    R3,12(R3)           NEXT TABLE ENTRY                             
         ST    R3,NEXTID                                                        
         OI    REPFLAG,FNDMATCH    FOUND AT LEAST ONE MATCH                     
         TM    FILTERS,FCITY       FILTER FOR CITY                              
         BO    GIDN2               DO NEXT TABLE ENTRY                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',(R4),AIO1                 
         B     GIDN10                                                           
         DROP  R6                                                               
*                                                                               
GIDNX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE ID NAME BASED ON AN ID NUMBER                                    
*                                                                               
* ON ENTRY:    BINREP              BINARY ID NUMBER                             
*                                                                               
* ON EXIT:     CHRREP              CHARACTER ID                                 
*              'KEY' & 'KEYSAVE' GOT CLOBBERED                                  
*              AIO1 WAS USED                                                    
*              'ELCODE' GOT CLOBBERED                                           
***********************************************************************         
SHWIDNUM NTR1                                                                   
         CLC   =2X'FF',BINREP      FAX DARE?                                    
         BNE   SHWID05                                                          
         MVC   CHRREP,=CL10'FAX'   SET THE ID TO FAX                            
         B     SHWIDX                                                           
*                                                                               
* FIRST CHECK IF ID IS IN TABLE                                                 
SHWID05  LA    R3,IDTABLE                                                       
SHWID10  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    SHWID20             NOT IN TABLE                                 
         C     R3,NEXTID           NO MORE ENTRIES IN TABLE                     
         BE    SHWID20                                                          
         CLC   0(2,R3),BINREP      SAME ID                                      
         BE    *+12                                                             
         LA    R3,12(R3)                                                        
         B     SHWID10             NOT IN TABLE                                 
*                                                                               
         MVC   CHRREP,2(R3)        FOUND ID IN TABLE                            
         B     SHWIDX                                                           
*                                                                               
SHWID20  TM    FILTERS,FCITY       FILTER FOR CITY?                             
         BO    *+12                                                             
         CLI   ORPREPH+5,0         WAS THERE A REP FILTER?                      
         BE    *+12                                                             
         OI    REPFLAG,NOMATCH     YES, AND NO MATCH FOUND                      
         B     SHWIDX                                                           
*                                                                               
         L     R3,NEXTID           ADDRESS TO INSERT NEXT ID IN TABLE           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,BINREP                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),AIO1                 
         CLI   8(R1),0                                                          
         BE    SHWID25                                                          
         MVC   CHRREP(3),=C'ID='   IF IT CAN'T FIND ID RECORD...                
         GOTO1 HEXOUT,DMCB,BINREP,CHRREP+3,L'BINREP  ...DISPLAY ID NUM          
         B     SHWID30                                                          
*                                                                               
SHWID25  L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        LOOK FOR THE DESCRIPTION ELEMENT             
         BAS   RE,NEXTEL                                                        
         BNE   BADRECRD                                                         
*                                                                               
         USING CTDSCD,R6           COPY USER ID TO SCREEN                       
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=Y(CTDSC-CTDSCD+1)                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CHRREP(0),CTDSC                                                  
*                                                                               
SHWID30  MVC   0(2,R3),BINREP      ADD TO TABLE                                 
         MVC   2(10,R3),CHRREP     ADD TO TABLE                                 
         LA    R3,12(R3)           NEXT TABLE ENTRY                             
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BNE   *+8                                                              
         LA    R3,IDTABLE          START AT BEGINNING OF TABLE                  
         ST    R3,NEXTID                                                        
         DROP  R6                                                               
*                                                                               
SHWIDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE ORDER NUMBER                                                     
* ON ENTRY:    BINORDER            ORDER NUMBER AS STORED IN RECORD             
*              (R2)                PORDRNO                                      
***********************************************************************         
SHWORDER NTR1                                                                   
         MVC   FULL,BINORDER       SHOW THE ORDER NUMBER                        
         XC    FULL,=4X'FF'                                                     
         L     R1,FULL                                                          
         AHI   R1,1                                                             
         ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         UNPK  0(4,R2),DUB                                                      
         OI    3(R2),X'F0'                                                      
*                                                                               
         ZICM  R3,FULL+2,2                                                      
         EDIT  (R3),(4,4(R2)),0,FILL=0    SEQUENCE NUMBER                       
*                                                                               
SORDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HEAD HOOK ROUTINE                                                             
***********************************************************************         
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H1+48(21),=C'PENDING ORDERS REPORT'                              
         MVI   H2+48,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+49(20),H2+48                                                  
*                                                                               
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+7,C'C'                                                   
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+37,C'C'                                                  
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+51,C'C'                                                  
         MVI   BOXCOLS+76,C'C'                                                  
         MVI   BOXCOLS+83,C'C'                                                  
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+108,C'C'                                                 
         MVI   BOXCOLS+118,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
HDHOOKX  B     XIT                                                              
         SPACE 3                                                                
HEDSPECS SPROG 0,1                                                              
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,86,REPORT                                                     
         SSPEC H1,105,REQUESTOR                                                 
         SSPEC H2,86,RUN                                                        
         SSPEC H2,112,PAGE                                                      
         SSPEC H7,2,C'BUYER'                                                    
         SSPEC H7,27,C'REP'                                                     
         SSPEC H7,39,C'STATION'                                                 
         SSPEC H7,47,C'MKT'                                                     
         SSPEC H7,53,C'MARKET'                                                  
         SSPEC H7,78,C'CLIENT'                                                  
         SSPEC H7,85,C'PRODUCTS'                                                
         SSPEC H7,94,C'EST'                                                     
         SSPEC H7,99,C'STATUS'                                                  
         SSPEC H7,110,C'DATE'                                                   
*                                                                               
         SPROG 0                                                                
         SSPEC H7,9,C'ORDER NUMBER'                                             
         SPROG 1                                                                
         SSPEC H7,9,C'ORDER #/GRP CODE'                                         
HEDSPECX DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
CURSERR  MVI   GERROR1,REQFIELD                                                 
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
         B     MYERREX                                                          
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
MYERREX  GOTO1 MYERR                                                            
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
STATTABL DS    0CL10                                                            
         DC    AL1(QAPP),C'OPENED   '                                           
         DC    AL1(QCFMDPND),C'CFMPNDING'                                       
         DC    AL1(QCFMD),C'CONFIRMED'                                          
         DC    AL1(QERRORED),C'ERRORED  '                                       
         DC    AL1(QFAXDLVD),C'FAX DLVRD'                                       
         DC    AL1(QFAXCNCL),C'FAX CANCL'                                       
         DC    AL1(QBYRCNFM),C'BUYERCNFM'                                       
         DC    AL1(QRJCT),C'REJECTED '                                          
         DC    AL1(QEMPTY),C'EMPTY    '                                         
         DC    AL1(QNODARE),C'NOTDARED '                                        
         DC    AL1(QRECALL),C'RECALLING'                                        
         DC    AL1(QRECALL),C'RECALLED '                                        
         DC    AL1(QRCLAPPR),C'RCLAPP   '                                       
         DC    AL1(QRCLCONF),C'RCLCFM   '                                       
         DC    AL1(QRCLDELN),C'RCLDNT   '                                       
         DC    AL1(QRCLREJD),C'RCLREJ   '                                       
         DC    AL1(QRCLUNKN),C'ERR999   '                                       
         DC    AL1(QUNDARE),C'UNDARED  '                                        
         DC    AL1(QRCLTRNS),C'RCLTRN   '                                       
         DC    AL1(QRCLWIP),C'RCLWIP   '                                        
         DC    AL1(DSENT),C'*SENT    '                                          
         DC    AL1(DDLVRD),C'SENT     '                                         
         DC    AL1(DFXSENT),C'*FXSENT  '                                        
         DC    AL1(DFXDLVD),C'FXSENT   '                                        
         DC    AL1(DEMSENT),C'*EMSENT  '                                        
         DC    AL1(DEMDLVD),C'EMSENT   '                                        
         DC    AL1(DFXRSNT),C'*FXSENT  '                                        
         DC    X'00',C'?????????'                                               
STATTBL2 DC    0CL10                                                            
*        DC    AL1(MNSTAPP),C'APPROVED '                                        
         DC    AL1(MNSTCANM),C'SELRECALL'                                       
         DC    AL1(MNSTCAN),C'CANCELLED'                                        
         DC    AL1(MNSTERR),C'ERROR    '                                        
         DC    AL1(MNSTGOIN),C'*OKAYED  '                                       
         DC    AL1(MNSTHOLD),C'ON HOLD  '                                       
         DC    AL1(MNSTAMND),C'AMENDED  '                                       
         DC    AL1(MNSTNEW),C'NEW      '                                        
         DC    AL1(MNSTOKAY),C'OKAYED   '                                       
*        DC    AL1(MNSTREJ),C'REJECTED '                                        
         DC    X'00',C'?????????'                                               
RELO     DS    A                                                                
*&&DO                                                                           
***********************************************************************         
* ORDER STATUS TABLE                                                            
***********************************************************************         
ORDSTAB  DS    0H                                                               
*                                  ** UNSENT **                                 
ORDUNST  DC    AL1(QUNSENT,ORDUNSTX-*+1),CL9'UNSENT'                            
         DC    AL1(0,0,ORD2XREV)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDUNSTX-*)/L'ORDDATA)                                      
ORDUNSTX EQU   *                                                                
*                                  ** SENT **                                   
ORDSENT  DC    AL1(DSENT,ORDSENTX-*+1),CL9'SENT'                                
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSENTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORDSENTX EQU   *                                                                
*                                  ** EMAIL SENT **                             
ORDEMLS  DC    AL1(DEMSENT,ORDEMLSX-*+1),CL9'EMSENT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REMSNT'                                          
         DC    AL1(MF2VAROR),C'VEMSNT'                                          
ORDEMLSX EQU   *                                                                
*                                  ** FAX SENT **                               
ORDFAXS  DC    AL1(DFXSENT,ORDFAXSX-*+1),CL9'FAXSNT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'RFXSNT'                                          
         DC    AL1(MF2VAROR),C'VFXSNT'                                          
ORDFAXSX EQU   *                                                                
*                                  ** FAX RESENT ONLY SHOWS AS FAX SENT         
ORDFAXRS DC    AL1(DFXRSNT,ORDFXRSX-*+1),CL6'FAXSNT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFXRSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'RFXSNT'                                          
         DC    AL1(MF2VAROR),C'VFXSNT'                                          
ORDFXRSX EQU   *                                                                
*                                  ** APPROVED **                               
ORDAPPR  DC    AL1(QAPP,ORDAPPRX-*+1),CL9'OPENED'                               
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDAPPRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVOPN'                                          
         DC    AL1(MF2VAROR),C'VAROPN'                                          
ORDAPPRX EQU   *                                                                
*                                  ** CONFIRM PENDING **                        
ORDCFPD  DC    AL1(QCFMDPND,ORDCFPDX-*+1),CL9'CFMPND'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCFPDX-*)/L'ORDDATA)                                      
ORDCFPDX EQU   *                                                                
*                                  ** CONFIRMED **                              
ORDCNFM  DC    AL1(QCFMD,ORDCNFMX-*+1),CL9'CNFRMD'                              
         DC    AL1(ORDTNCMT,ORDIFTST+ORDINSTS,ORD2ACFM)                         
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFMX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
         DC    AL1(MF2REVOR),C'REVCNF'                                          
         DC    AL1(MF2VAROR),C'VARCNF'                                          
ORDCNFMX EQU   *                                                                
*                                  ** CONFIRMED WITH COMMENTS **                
ORDCNFC  DC    AL1(QCFMD,ORDCNFCX-*+1),CL9'**PCFM'                              
         DC    AL1(ORDTCMTS,ORDINSTS,ORD2ACFM)                                  
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFCX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
         DC    AL1(MF2REVOR),C'**RPCF'                                          
         DC    AL1(MF2VAROR),C'**VPCF'                                          
ORDCNFCX EQU   *                                                                
*                                  ** ERROR **                                  
ORDERR   DC    AL1(QERRORED,ORDERRX-*+1),CL9'ERROR'                             
         DC    AL1(0,ORDIXREV+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDERRX-*)/L'ORDDATA)                                       
         DC    AL1(MF2EMAIL),C'EMERR '                                          
         DC    AL1(MF2FAXED),C'FXERR '                                          
ORDERRX  EQU   *                                                                
*                                  ** EMAIL DELIVERED **                        
ORDEMLD  DC    AL1(DEMDLVD,ORDEMLDX-*+1),CL9'EMDLVD'                            
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDEMLDX-*)/L'ORDDATA)                                      
ORDEMLDX EQU   *                                                                
*                                  ** FAX DELIVERED **                          
ORDFAXD  DC    AL1(DFXDLVD,ORDFAXDX-*+1),CL9'FXDLVD'                            
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXDX-*)/L'ORDDATA)                                      
ORDFAXDX EQU   *                                                                
*                                  ** FAX CANCELLED **                          
ORDFAXC  DC    AL1(QFAXCNCL,ORDFAXCX-*+1),CL9'FXERR '                           
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXCX-*)/L'ORDDATA)                                      
ORDFAXCX EQU   *                                                                
*                                  ** MANUAL CONFIRMED **                       
ORDCNFX  DC    AL1(QBYRCNFM,ORDCNFXX-*+1),CL9'BYRCNF'                           
         DC    AL1(0,ORDINSTS+ORDIFTST,ORD2ACFM)                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFXX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
ORDCNFXX EQU   *                                                                
*                                  ** REJECTED **                               
ORDRJCT  DC    AL1(QRJCT,ORDRJCTX-*+1),CL9'RJCTED'                              
         DC    AL1(ORDTRJCT,0,ORD2REJ)                                          
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRJCTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVREJ'                                          
         DC    AL1(MF2VAROR),C'VARREJ'                                          
ORDRJCTX EQU   *                                                                
*                                                                               
ORDAMND  DC    AL1(QRJCT,ORDAMNDX-*+1),CL9'AMEND'                               
         DC    AL1(ORDTAMND,0,ORD2REJ)                                          
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDAMNDX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVAMD'                                          
         DC    AL1(MF2VAROR),C'REVAMD'                                          
ORDAMNDX EQU   *                                                                
*                                  ** EMPTY **                                  
ORDEMPT  DC    AL1(QEMPTY,ORDEMPTX-*+1),CL9'EMPTY'                              
         DC    AL1(0,ORDIXCLD,0)                                                
         DC    C'K'                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDEMPTX-*)/L'ORDDATA)                                      
ORDEMPTX EQU   *                                                                
*                                  ** NOT DARED **                              
ORDNODA  DC    AL1(QNODARE,ORDNODAX-*+1),CL9'NTDARE'                            
         DC    AL1(0,ORDIFTST+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDNODAX-*)/L'ORDDATA)                                      
ORDNODAX EQU   *                                                                
*                                  ** RECALLED **                               
ORDRECA  DC    AL1(QRECALL,ORDRECAX-*+1),CL9'RECALL'                            
         DC    AL1(0,ORDIRCDA+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRECAX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRECAX EQU   *                                                                
*                                  ** RECALLED APPROVED **                      
ORDRCAP  DC    AL1(QRCLAPPR,ORDRCAPX-*+1),CL9'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCAPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCAPX EQU   *                                                                
*                                  ** RECALLED - CONFIRMED **                   
ORDRCCF  DC    AL1(QRCLCONF,ORDRCCFX-*+1),CL9'RECALL'                           
         DC    AL1(0,ORDISENT+ORDISRCL+ORDINSTS,0)                              
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCCFX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCCFX EQU   *                                                                
*                                  ** RECALLED - DELIVERED **                   
ORDRCDE  DC    AL1(QRCLDELN,ORDRCDEX-*+1),CL9'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCDEX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCDEX EQU   *                                                                
*                                  ** RECALLED - REJECTED **                    
ORDRCRJ  DC    AL1(QRCLREJD,ORDRCRJX-*+1),CL9'RECALL'                           
         DC    AL1(0,ORDISENT+ORDISRCL+ORDINSTS,0)                              
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCRJX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCRJX EQU   *                                                                
*                                  ** RECALLED UNKNOWN **                       
ORDRCUK  DC    AL1(QRCLUNKN,ORDRCUKX-*+1),CL9'ERROR'                            
         DC    AL1(0,ORDIXREV+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCUKX-*)/L'ORDDATA)                                      
ORDRCUKX EQU   *                                                                
*                                  ** UNDARED **                                
ORDUNDA  DC    AL1(QUNDARE,ORDUNDAX-*+1),CL9'UNDARD'                            
         DC    AL1(0,ORDIFTST+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDUNDAX-*)/L'ORDDATA)                                      
ORDUNDAX EQU   *                                                                
*                                  ** RECALLED - TRANSMITTED **                 
ORDRCTR  DC    AL1(QRCLTRNS,ORDRCTRX-*+1),CL9'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCTRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCTRX EQU   *                                                                
*                                  ** RECALLED - WIP **                         
ORDRCWP  DC    AL1(QRCLWIP,ORDRCWPX-*+1),CL9'RECALL'                            
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCWPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCWPX EQU   *                                                                
*                                  ** SENT PENDING **                           
ORDSNTP  DC    AL1(QSNTPNDG,ORDSNTPX-*+1),CL9'SENT'                             
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2FAXED),C'FAXSNT'                                          
         DC    AL1(MF2EMAIL),C'EMLSNT'                                          
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORDSNTPX EQU   *                                                                
*                                  ** SENT CANCELLED PCFM **                    
ORDSNTC  DC    AL1(QSNTXCNF,ORDSNTCX-*+1),CL9'**PCFM'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTCX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'**RPCF'                                          
         DC    AL1(MF2VAROR),C'**VPCF'                                          
ORDSNTCX EQU   *                                                                
*                                  ** SENT CANCELLED RJCT **                    
ORDSNTR  DC    AL1(QSNTXREJ,ORDSNTRX-*+1),CL9'RJCTED'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVREJ'                                          
         DC    AL1(MF2VAROR),C'VARREJ'                                          
ORDSNTRX EQU   *                                                                
*                                  ** SENT **                                   
ORD2BSN  DC    AL1(QTOBESNT,ORD2BSNX-*+1),CL9'SENT'                             
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORD2BSNX-*)/L'ORDDATA)                                      
         DC    AL1(MF2FAXED),C'FAXSNT'                                          
         DC    AL1(MF2EMAIL),C'EMLSNT'                                          
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORD2BSNX EQU   *                                                                
*                                                                               
ORDEND   DC    AL1(255,ORDENDX-*+1),CL9'ERR999'                                 
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDENDX-*)/L'ORDDATA)                                       
ORDENDX  EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
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
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
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
       ++INCLUDE SPADAVCOM                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSF9D          (OUR MAINTENANCE SCREEN)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSE9D          (OUR LIST SCREEN)                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSD9D          (OUR DISPLAY SCREEN)                         
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC3D          (OUR STANDARD COMMENTS LIST SCREEN)          
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSF2D          (REPORT SCREEN)                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN        (DARE MAKEGOOD NOTICE RECORD)                
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
FILTERS  DS    X                                                                
FDATE    EQU   X'80'               FILTER FOR A DATE                            
FSENT    EQU   X'40'               FILTER FOR STATUS = SENT                     
FDELIVRD EQU   X'20'               FILTER FOR STATUS = DEILVERED                
FSTATUS  EQU   X'08'               FILTER FOR A STATUS                          
FCITY    EQU   X'04'               FILTER FOR A CITY                            
PAGEBRAK EQU   X'02'               PAGE BREAK AT NEW REP                        
FCLIENT  EQU   X'01'               FILTER FOR A CLIENT                          
*                                                                               
FILTER2  DS    X                   FILTER FOR ORDER                             
F2APPRVD EQU   X'80'               FILTER FOR APPROVED                          
F2CNFM   EQU   X'40'               FILTER FOR CONFIRMED                         
F2ERROR  EQU   X'20'               FILTER FOR ORDER IN ERROR                    
F2FAX    EQU   X'10'               FILTER FOR ORDER WAS FAXED                   
F2RJCT   EQU   X'08'               FILTER FOR REJECTED                          
F2NODARE EQU   X'04'               FILTER FOR NOT DARE                          
F2RECALL EQU   X'02'               FILTER FOR RECALLED                          
F2UNSENT EQU   X'01'               FILTER FOR UNSENT                            
FILTER2A DS    X                   FILTER2 CONT.                                
F2CFMPND EQU   X'80'               FILTER FOR COMFIRM PENDING                   
F2UNDARE EQU   X'40'               FILTER FOR UNDARE                            
F2EMPTY  EQU   X'20'               FILTER FOR EMPTY                             
F2OFFERS EQU   X'10'               FILTER FOR OFFERS                            
F2EMAIL  EQU   X'08'               FILTER FOR EMAIL                             
F2AMEND  EQU   X'04'               FILTER FOR AMENDED                           
*                                                                               
FILTER3  DS    X                 FILTERS FOR MAKEGOODS                          
F3APPRVD EQU   X'80'               FILTER FOR APPROVED                          
F3CANMOR EQU   X'40'               FILTER FOR CANCELLED W/MORE 2 FOLLOW         
F3CANCEL EQU   X'20'               FILTER FOR CANCELLED                         
F3ERROR  EQU   X'10'               FILTER FOR ERROR                             
F3TOBOK  EQU   X'08'               FILTER FOR GOING TO BE OKAYED                
F3ONHOLD EQU   X'04'               FILTER FOR ON HOLD                           
F3AMEND  EQU   X'02'               FILTER FOR AMENDED                           
F3NEW    EQU   X'01'               FILTER FOR NEW                               
FILTER3A DS    X                 MORE FILTERS FOR MAKEGOODS                     
F3OK     EQU   X'80'               FILTER FOR OKAYED                            
F3RJCT   EQU   X'40'               FILTER FOR REJECTED                          
F3APPRV1 EQU   X'20'               FILTER FOR *APPROVED                         
F3RJCT1  EQU   X'10'               FILTER FOR *REJECTED                         
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS FOR ORDER                
MF1VAROR EQU   X'80'                 WE GOT A VAR ORDER HERE                    
MF1CNFCM EQU   X'40'                 WE GOT A CONFIRM WITH COMMENT              
MF1REVOR EQU   X'20'                 WE GOT A REVISED ORDER HERE                
MF1DLVRD EQU   X'10'                 DELIVERY NOTICE RECEIVED                   
MF1OFFER EQU   X'08'                 ORDER WAS AN OFFER                         
*                                                                               
MISCFLG2 DS    X                   MISCELLANEOUS FLAGS FOR MAKEGOODS            
MF2MKGD  EQU   X'80'                 MAKEGOOD REPORT                            
MF2DELNT EQU   X'40'                 GOT DELIVERY NOTICE                        
*                                                                               
BUYRFILT DS    XL3                 BUYER FILTER                                 
CITY     DS    CL2                 FILTER CITY CODE                             
CLTFILT  DS    XL2                 CLIENT FILTER PACKED                         
DATEFILT DS    XL3                 DATE FILTER (PWOS)                           
*                                                                               
BINREP   DS    XL2                 BINARY REP ID NUMBER                         
CHRREP   DS    CL10                CHARACTER REP ID NUMBER                      
REPFLAG  DS    X                                                                
EXMATCH  EQU   X'80'               FULL REP FILTER NAME GIVEN                   
NOMATCH  EQU   X'40'               NOT A SIMILAR REP TO FILTER GIVEN            
FNDMATCH EQU   X'20'               FOUND AT LEAST ONE MATCH                     
*                                                                               
BINORDER DS    XL4                 ORDER NUMBER                                 
SAVEBUYR DS    CL3                 BUYER OF PREVIOUS LINE                       
*                                                                               
SAVEKEY  DS    XL25                                                             
FAKEHDR  DS    XL8                 FAKE HEADER                                  
FAKEFLD  DS    XL8                 FAKE FIELD                                   
PLINE    DS    CL131               PRINT LINE                                   
MYBLOCK  DS    XL56                BLOCK FOR PERVAL                             
R6SAVE   DS    F                   SAVE R6 POSITION                             
NEXTREP  DS    F                   NEXT SPOT TO INT DARE TABLE                  
NEXTID   DS    F                   NEXT SPOT TO INSERT ID IN TABLE              
IDTABLE  DS    XL600               (2:BINARY ID + 10:CHAR ID) X 50              
         DC    X'FF'                                                            
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* EDICT CHUNKY DSECT                                                            
***********************************************************************         
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR REPORT                                                         
***********************************************************************         
SPOOLD   DSECT                                                                  
         ORG   P                   LABEL FOR SPOOL                              
         DS    C                                                                
PBUYER   DS    CL3                 BUYER                                        
         DS    CL4                                                              
PORDRNO  DS    CL8                 ORDER NO.                                    
PSLASH   DS    C                                                                
PGRPCODE DS    CL3                GROUP CODE(ONLY FOR MAKEGOOD REPORTS)         
         DS    CL6                                                              
PREP     DS    CL10                REP                                          
         DS    CL2                                                              
PSTA     DS    CL5                 STATION                                      
         DS    CL3                                                              
PMKT     DS    CL4                 MARKET NO.                                   
         DS    CL2                                                              
PMKN     DS    CL24                MARKET NAME                                  
         DS    C                                                                
PCLT     DS    CL3                 CLIENT                                       
         DS    CL4                                                              
PPRD     DS    CL4                 PRODUCT                                      
PPRD2    DS    CL3                 PIGGY BACK                                   
         DS    CL2                                                              
PEST     DS    CL3                 EST                                          
         DS    CL2                                                              
PSTAT    DS    CL9                 STATUS                                       
         DS    CL2                                                              
PDATE    DS    CL8                 DATE OF LAST STATUS                          
PLINELNQ EQU   *-P                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPOMS02   01/03/07'                                      
         END                                                                    

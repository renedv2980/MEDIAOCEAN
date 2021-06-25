*          DATA SET ACBAT3C    AT LEVEL 069 AS OF 05/07/99                      
*PHASE T61B3CA                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'CTA Input - TYPE 60'                                            
T61B3C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT3C,R8,R4,CLEAR=YES,RR=R2                       
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         L     R1,=V(RIGHT)                                                     
         AR    R1,R2                                                            
         ST    R1,RIGHT                                                         
         ST    R2,RELO1                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
*                                                                               
         CLI   CSACT,ACTDSP                                                     
         BNE   VDT0008                                                          
         GOTO1 =A(SAVBIO),DMCB,(RC),RR=RELO1   SAVE BIO ELEM                    
*                                                                               
         USING TIOBD,RF                                                         
VDT0008  LA    R2,DETMEMBH                                                      
         CLI   PFKEY,X'FF'                                                      
         BNE   VDT0010                                                          
         L     RF,AINP                                                          
         MVC   PFKEY,TIOBAID       USE KEY FROM TRANSLATOR BLOCK                
         DROP  RF                                                               
*                                                                               
VDT0010  CLI   PFKEY,0             NO PFKEY ENTERED                             
         BE    VDT0150                                                          
         CLI   CSSPROG,1           IN DETAIL SCREEN                             
         BNE   VDT0050                                                          
         CLI   PFKEY,PFK10                                                      
         BE    VDT0400                                                          
         CLI   PFKEY,PFK11                                                      
         BE    VDT0025                                                          
         CLI   PFKEY,PFK05         SCROLLING KEYS?                              
         BL    VDT0030                                                          
         CLI   PFKEY,PFK08                                                      
         BH    VDT0030                                                          
         CLI   PFKEY,PFK05         HANDLE SCROLLING NEEDS                       
         BNE   *+12                TOP                                          
         MVI   STCONTR,1           FIRST CONTRACT                               
         B     VDT0029                                                          
*                                                                               
         CLI   PFKEY,PFK06                                                      
         BNE   VDT0023                                                          
         SR    RF,RF                                                            
         IC    RF,NUMCONTR         NUMBER OF CONTRACTS                          
         SH    RF,=Y(SCRNMAX-1)                                                 
         BZ    *+8                                                              
         BP    *+8                 POSITIVE?                                    
         LA    RF,1                                                             
         STC   RF,STCONTR                                                       
         B     VDT0029                                                          
*                                                                               
VDT0023  CLI   PFKEY,PFK07                                                      
         BNE   VDT0024                                                          
         CLI   STCONTR,1                                                        
         BE    VDT0029                                                          
         SR    RF,RF                                                            
         IC    RF,STCONTR                                                       
         BCTR  RF,0                                                             
         STC   RF,STCONTR                                                       
         B     VDT0029                                                          
*                                                                               
VDT0024  CLI   PFKEY,PFK08                                                      
         BNE   BADKEY                                                           
         CLC   STCONTR,NUMCONTR                                                 
         BE    VDT0029                                                          
         SR    RF,RF                                                            
         IC    RF,STCONTR                                                       
         LA    RF,1(RF)                                                         
         STC   RF,STCONTR                                                       
         B     VDT0029                                                          
*                                                                               
         USING CTABD,R7                                                         
VDT0025  SR    R6,R6               ERASE ENTRIES                                
         IC    R6,NUMCONTR                                                      
         LA    R7,CNTRTABL                                                      
VDT0025A ZAP   CTABURET,=P'0'                                                   
         ZAP   CTABTAXS,=P'0'                                                   
         OI    CTABSTAT,CTABSCHA   MARK FOR CHANGE                              
         LA    R7,CTABLNQ(R7)                                                   
         BCT   R6,VDT0025A                                                      
         BAS   RE,TOTCNTR                                                       
         BAS   RE,SHOWTOTS                                                      
         B     VDT0029                                                          
*                                                                               
VDT0029  GOTO1 =A(DETLLIST),DMCB,0,(RC),RR=RELO1                                
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$DDEAM)                                           
         B     EXIT                                                             
*                                                                               
VDT0030  B     VDT0400             RETURN TO MAIN SCREEN                        
*&&DO                                                                           
VDT0030  CLI   PFKEY,PFK12         RETURN TO MAIN SCREEN                        
         BNE   BADKEY                                                           
         BAS   RE,CALLMAIN                                                      
         B     CURSIT                                                           
*&&                                                                             
VDT0050  DS    0H                                                               
         CLI   PFKEY,PFK09         SWITCH TO DETAIL SCREEN                      
         BE    VDT0150                                                          
         CLI   PFKEY,PFK10                                                      
         BE    VDT0150                                                          
         CLI   CSACT,ACTINP                                                     
         BE    BADKEY                                                           
         B     VDT0150                                                          
*                                                                               
VDT0060  DS    0H                                                               
         MVI   STCONTR,1           FIRST CONTRACT                               
         BAS   RE,CALLDETS                                                      
         MVI   TWASCRN,DTLSCRN                                                  
         LA    R2,DETMEMBH                                                      
         ST    R2,FVADDR                                                        
         BAS   RE,SCRSETUP                                                      
         B     POSTDET                                                          
*                                                                               
VDT0100  BAS   RE,SCRSETUP         SETUP SCREEN AFTER SWITCHING                 
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$AOKNX)    HAVE TO RETURN                         
         B     ERROR                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* VENDOR SCREEN                                                                 
*---------------------------------------------------------------------*         
VDT0150  DS    0H                                                               
         CLI   TWASCRN,VNDSCRN     VENDOR SCREEN                                
         BNE   VDT0400                                                          
*                                                                               
         XC    VNDNUM,VNDNUM                                                    
         ZAP   VNDAMT,=P'0'                                                     
*                                                                               
         CLI   VNDPVNDH+5,0        NO PROD VENDOR                               
         BNZ   VDT0152                                                          
         MVC   VNDPVDA,SPACES      WIPE OUT PROD VENDOR AMOUNT                  
         OI    VNDPVDAH+6,X'80'                                                 
         B     VDT0155                                                          
*                                                                               
VDT0152  DS    0H                                                               
         GOTO1 VALVEND,DMCB,(0,VNDPVNDH)                                        
         NI    VNDSTAT,X'FF'-VNDEXPNS      PROD VENDOR USED                     
         MVC   VNDNUM,ACCTNUM                                                   
         MVC   VNDNAME,ACCTNAME                                                 
         MVC   VENDORAC,VNDPVND                                                 
         BAS   RE,FINDCD                                                        
*                                                                               
         LA    R2,VNDPVDAH                                                      
         GOTO1 ANY                                                              
         BAS   RE,VALAMNT          VALIDATE AMOUNT AND DISPLAY IT               
         BNE   VDTX999                                                          
         ZAP   VNDAMT,DUB                                                       
         MVC   VNDPVDA,SPACES                                                   
         EDIT  VNDAMT,(12,VNDPVDA),2,FLOAT=-                                    
         OI    VNDPVDAH+6,X'80'                                                 
*                                                                               
VDT0155  CLI   VNDXVNDH+5,0        NO EXP VENDOR                                
         BNZ   VDT0158                                                          
         MVC   VNDXVDA,SPACES      CLEAR EXPENSE VENDOR AMOUNT                  
         OI    VNDXVDAH+6,X'80'                                                 
         OC    VNDNUM,VNDNUM                                                    
         BNZ   VDT0160                                                          
         LA    R2,VNDPVNDH                                                      
         MVC   FVMSGNO,=AL2(AE$MISIF)     MISSING INPUT                         
         B     VDTX999                                                          
*                                                                               
VDT0158  OC    VNDNUM,VNDNUM      ONLY ONE VENDOR ALLOWED                       
         BNH   VDT0159                                                          
         LA    R2,VNDXVNDH                                                      
         MVC   FVMSGNO,=AL2(AE$ONVND)                                           
         B     VDTX999                                                          
*                                                                               
VDT0159  GOTO1 VALVEND,DMCB,(X'FF',VNDXVNDH)                                    
         OI    VNDSTAT,VNDEXPNS   EXPENSE VENDOR USED                           
         MVC   VNDNUM,ACCTNUM                                                   
         MVC   VNDNAME,ACCTNAME                                                 
         MVC   VENDORAC,VNDXVND                                                 
         BAS   RE,FINDCD                                                        
*                                                                               
         LA    R2,VNDXVDAH                                                      
         GOTO1 ANY                                                              
         BAS   RE,VALAMNT                                                       
         BNE   VDTX999                                                          
         ZAP   VNDAMT,DUB                                                       
         MVC   VNDXVDA,SPACES                                                   
         EDIT  VNDAMT,(12,VNDXVDA),2,FLOAT=-                                    
         OI    VNDXVDAH+6,X'80'                                                 
*                                                                               
VDT0160  DS    0H                                                               
         LA    R2,VNDXACTH                                                      
         GOTO1 ANY                                                              
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SE'     EXPENSE                                      
         SR    RF,RF                                                            
         IC    RF,VNDXACTH+5                                                    
         BCTR  RF,0                                                             
         CLI   VNDXACT,C'*'        U/L OVERRIDE?                                
         BE    *+12                                                             
         EX    RF,VDT0163                                                       
         B     VDT0169                                                          
*                                                                               
         BCTR  RF,0                MINUS 1 FOR C'*'                             
         EX    RF,VDT0165                                                       
         B     VDT0169                                                          
*                                                                               
VDT0163  MVC   KEY+3(0),VNDXACT                                                 
VDT0165  MVC   KEY+1(0),VNDXACT+1                                               
*                                                                               
VDT0169  LA    R6,CLIPROF                                                       
         BAS   RE,GETACC           VALIDATE EXPENSE ACCOUNT                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   FLD,SPACES          PUT NAME ON SCREEN                           
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         OI    4(R2),X'80'                                                      
         MVC   EXPNUM,ACCTNUM                                                   
         MVC   EXPNACC,VNDXACT                                                  
         MVC   EXPNAME,ACCTNAME                                                 
*                                                                               
VDT0170  DS    0H                                                               
         LA    R2,VNDINRFH                                                      
         GOTO1 ANY                                                              
         MVC   INVREF,SPACES                                                    
         SR    RF,RF                                                            
         IC    RF,VNDINRFH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   INVREF(0),VNDINRF                                                
         MVC   INVREFLN,VNDINRFH+5                                              
*                                                                               
         LA    R2,VNDDATEH                                                      
         OC    VNDDATE,SPACES                                                   
         GOTO1 VALDATE,DMCB,VNDDATEH                                            
         BNE   ERROR                                                            
         MVC   INVDTE,SAVEDATE                                                  
         LA    R2,VNDDUEDH                                                      
         OC    VNDDUED,SPACES                                                   
         GOTO1 VALDATE,DMCB,VNDDUEDH                                            
         BNE   ERROR                                                            
         MVC   INVDDTE,SAVEDATE                                                 
*                                                                               
         USING ORDRECD,R5                                                       
         MVC   FVMSGNO,=AL2(AE$NOONO)                                           
         LA    R2,VNDPONH                                                       
         GOTO1 ANY                                                              
         TM    VNDPONH+4,X'80'     VENDOR CHANGED THIS TIME                     
         BZ    *+8                                                              
         OI    VNDSTAT,VNDORDCH                                                 
         XC    KEY,KEY                                                          
         MVC   FVMSGNO,=AL2(AE$ORDNF)                                           
         LA    R5,KEY                                                           
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,VNDPON      MOVE IN ORDER NUMBER                         
         GOTO1 READ                                                             
         MVC   FVMSGNO,=AL2(AE$ORDNC)                                           
         TM    ORDRSTAT,ORDSCON     HAS TO BE A CONTRACT ORDER                  
         BZ    VDTX999                                                          
*                                                                               
         CLI   CSACT,ACTDSP                                                     
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(AE$OFMCH)                                           
         TM    ORDRSTAT,ORDSFMCH    FULLY MATCHED?                              
         BO    VDTX999                                                          
         CLC   INVORDNO,VNDPON                                                  
         BE    *+8                                                              
         OI    VNDSTAT,VNDORDCH                                                 
         MVC   INVORDNO,VNDPON                                                  
*                                                                               
         USING OAMELD,R3                                                        
         L     R3,AIO1                                                          
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    VNDSTAT,X'FF'-VNDOCHCK-VNDOCSBP                                  
         TM    OAMTYPE,OAMTCHK     CHECK?                                       
         BZ    *+8                                                              
         OI    VNDSTAT,VNDOCHCK                                                 
         CLC   OAMWORK,=CL2'CA'    CASH / BILLPAY                               
         BNE   *+8                                                              
         OI    VNDSTAT,VNDOCSBP                                                 
*                                                                               
         BAS   RE,GETCNTR          GET CONTRACTOR                               
         BNE   VDTX999                                                          
*                                                                               
         LA    R2,VNDPARTH                                                      
         MVI   PRGOSTAT,0                                                       
         CLI   VNDPARTH+5,0                                                     
         BE    VDT0180                                                          
         TM    VNDSTAT,VNDOCHCK                                                 
         BO    VDT0179                                                          
         CLI   VNDPART,C'Y'        PARTIAL ORDER                                
         BE    *+8                                                              
         CLI   VNDPART,C'P'        DITTO                                        
         BE    *+14                                                             
VDT0179  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERROR                                                            
         OI    PRGOSTAT,PRGOPART                                                
*                                                                               
         USING CPYELD,R5                                                        
VDT0180  LA    R5,BCCPYEL                                                       
         LA    R2,VNDOFFCH                                                      
         MVC   SVOFFICE,SPACES                                                  
         TM    CPYSTAT4,CPYSOFF2   2 BYTE OFFICE                                
         BO    VDT0185                                                          
         CLI   5(R2),0                                                          
         BZ    VDT0190                                                          
VDT0185  GOTO1 ANY                 OFFICE IS REQUIRED                           
VDT0187  MVC   SVOFFICE,8(R2)                                                   
         GOTO1 AVALOFFC,DMCB,(0,SVOFFICE)                                       
         BE    VDT0190                                                          
         MVC   FVMSGNO,=AL2(AE$OFCNF)                                           
         MVC   FVXTRA(2),SVOFFICE                                               
         B     VDTX999                                                          
         DROP  R5                                                               
*                                                                               
VDT0190  DS    0H                                                               
         CLI   CDSW,C'N'                                                        
         BE    VDT0191                                                          
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         TM    CPYSTAT1,CPYSDISC   DOES COMPANY SUPPORT CD?                     
         BO    VDT0193                                                          
VDT0191  MVI   CDSW,C'N'                                                        
         MVI   VNDCD,C'N'                                                       
         OI    VNDCDH+6,X'80'                                                   
         B     VDT0195                                                          
         DROP  RF                                                               
*                                                                               
VDT0193  MVI   CDSW,C'Y'                                                        
         LA    R2,VNDCDH                                                        
         CLI   VNDCDH+5,0          NO INPUT?                                    
         BE    VDT0195                                                          
         CLI   VNDCD,C'Y'          Y/N VALID                                    
         BE    VDT0195                                                          
         CLI   VNDCD,C'N'                                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     VDTX999                                                          
         MVI   CDSW,C'N'                                                        
*                                                                               
VDT0195  LA    R2,VNDCOM1H                                                      
         BAS   RE,VALCOMM                                                       
         LA    R2,VNDCOM2H                                                      
         BAS   RE,VALCOMM                                                       
         LA    R2,VNDCOM3H                                                      
         BAS   RE,VALCOMM                                                       
*                                                                               
VDT0200  ZAP   VNDTOT,VNDAMT                                                    
*                                                                               
         CLI   CSACT,ACTINP                                                     
         BE    VDT0250                                                          
VDT0230  MVC   GROUPNO,CSGIN       USE INVOICE NUMBER SAVED                     
         MVC   NUMCONTR,CSSHIREC   AND NUMBER OF CONTRACTS SAVED                
         CLI   PFKEY,PFK09                                                      
         BE    VDT0250                                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$AOKNX)                                           
         B     EXIT                                                             
*                                                                               
         USING DLDESCD,R7                                                       
VDT0250  MVI   STCONTR,1                                                        
         BAS   RE,CALLDETS         CALL DEBIT SCREEN                            
         MVI   TWASCRN,DTLSCRN                                                  
         LA    R2,DETMEMBH                                                      
         ST    R2,FVADDR                                                        
         BAS   RE,SCRSETUP                                                      
         CLI   CSACT,ACTINP                                                     
         BNE   VDT0400                                                          
*                                                                               
         GOTO1 =A(UPDMEMO),DMCB,0,(RC),RR=RELO1                                 
         MVC   DETMEMB,DETLMMB                                                  
         OI    DETMEMBH+6,X'80'                                                 
*                                                                               
         B     VDT0400                                                          
         EJECT                                                                  
VDT0300  GOTO1 =A(POSTVEN),DMCB,(RC),RR=RELO1                                   
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         B     VDT0800                                                          
         EJECT                                                                  
*---------------------------------------------------------------------          
* DETAIL SCREEN                                                                 
*   - VALIDATE FIELDS BEFORE POSTING                                            
*---------------------------------------------------------------------          
VDT0400  CLI   TWASCRN,DTLSCRN     DETAIL SCREEN?                               
         BNE   VDT0899                                                          
         CLI   CSACT,ACTDSP                                                     
         BE    VDT0420                                                          
*                                                                               
         LA    R2,DETVNDAH                                                      
         GOTO1 ANY                                                              
         BAS   RE,VALAMNT                                                       
         BNE   VDTX999                                                          
         ZAP   VNDTOT,DUB                                                       
         MVC   DETVNDA,SPACES                                                   
         EDIT  VNDTOT,(12,DETVNDA),2,FLOAT=-                                    
         OI    DETVNDAH+6,X'80'                                                 
*                                                                               
         LA    R2,DETMEMDH                                                      
         OC    MEMDTE,MEMDTE       DO WE HAVE A MEMO BILL DATE?                 
         BNZ   VDT0410                                                          
         CLI   DETMEMDH+5,0                                                     
         BNE   VDT0410                                                          
         MVC   DETMEMD,DETDATE                                                  
         MVC   DETMEMDH+5(1),DETDATEH+5                                         
         MVC   MEMDTE,INVDTE                                                    
         OI    DETMEMDH+6,X'80'                                                 
         B     VDT0413                                                          
*                                                                               
VDT0410  OC    DETMEMD,SPACES                                                   
         GOTO1 VALDATE,DMCB,DETMEMDH                                            
         BNE   ERROR                                                            
         MVC   MEMDTE,SAVEDATE                                                  
*                                                                               
VDT0413  DS    0H                                                               
*                                                                               
         GOTO1 =A(UPDMEMO),DMCB,0,(RC),RR=RELO1                                 
         MVC   DETMEMB,DETLMMB                                                  
         CLI   DETMEMBH+5,0                                                     
         BNZ   *+8                                                              
         MVI   DETMEMBH+5,6                                                     
         OI    DETMEMBH+6,X'80'                                                 
*                                                                               
         LA    R2,DETMEMBH                                                      
         GOTO1 ANY                                                              
         MVC   DETLMMB,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,DETMEMBH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   DETLMMB(0),DETMEMB                                               
*                                                                               
         LA    R2,DETLOCLH                                                      
         ZAP   UTAXRT,=P'0'        INIT USE TAX RATE                            
         MVC   UTAXLOCL,DETLOCL                                                 
         OC    UTAXLOCL,SPACES                                                  
         CLC   UTAXLOCL,SPACES     ANY LOCALITY?                                
         BNH   VDT0415                                                          
         BAS   RE,RDTREC           READ TAX RECORD                              
         BNE   VDTX999                                                          
*                                                                               
         USING ORDRECD,R5                                                       
VDT0415  XC    KEY,KEY             READ ORDER RECORD                            
         LA    R5,KEY                                                           
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,INVORDNO    MOVE IN ORDER NUMBER                         
         GOTO1 READ                                                             
*                                                                               
*        L     R0,AIOA             MOVE ORDER RECORD                            
*        L     RE,AIO1                                                          
*        LA    R1,IOAREALN                                                      
*        LR    RF,R1                                                            
*        MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,GETCNTR          GET CONTRACTOR                               
         BNE   VDTX999                                                          
*                                                                               
         GOTO1 =A(DETLLIST),DMCB,1,(RC),RR=RELO1          VALIDATE              
         BNE   VDTX999                                                          
         LA    R2,DETLOCLH         RESET CURSOR TO LOCALITY                     
         BAS   RE,TOTCNTR                                                       
         BAS   RE,SHOWTOTS                                                      
         MVC   BOWORK1(L'DETLMMB),DETLMMB                                       
*                                                                               
VDT0420  CLI   PFKEY,PFK12         RETURN?                                      
         BNE   VDT0430                                                          
         BAS   RE,CALLMAIN                                                      
         B     CURSIT                                                           
*                                                                               
VDT0430  CLI   PFKEY,PFK10                                                      
         BNE   VDT0439                                                          
         CLI   CSACT,ACTINP                                                     
         BE    POSTDET                                                          
         BAS   RE,CALLMAIN         HAVE TO RESTORE MAIN SCREEN                  
         BAS   RE,TOTCNTR          HAVE TO GET TOTALS                           
         B     VDTX906             JUST PRINT MEMO BILL                         
*                                                                               
VDT0439  MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$AOKNX)                                           
         LA    R2,DETMEMDH                                                      
         ST    R2,FVADDR                                                        
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*====================================================================*          
* DO DETAIL POSTINGS                                                 *          
*====================================================================*          
POSTDET  DS    0H                                                               
         NI    PRGOSTAT,X'FF'-PRGPTAX         RESET                             
         CP    VNDTOT,DETTOT       HAVE TO BE THE SAME                          
         BE    PD0050                                                           
         MVC   FVMSGNO,=AL2(AE$BTDIF)                                           
         B     VDTX999                                                          
*                                                                               
PD0050   DS    0H                                                               
*                                                                               
         GOTO1 =A(UPDMEMO),DMCB,1,(RC),RR=RELO1                                 
         MVC   DETMEMB,DETLMMB                                                  
         OI    DETMEMBH+6,X'80'                                                 
*                                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   *+8                                                              
         BAS   RE,DELOLDGN         DELETE OLD PASSIVES                          
*                                                                               
         LA    R2,DETCTRH          FIRST DETAIL LINE                            
         OC    GROUPNO,GROUPNO                                                  
         BNZ   PD0100                                                           
         GOTO1 AGETGIN             GROUP INVOICE NUMBER                         
         MVC   GROUPNO,CSGIN                                                    
*                                                                               
         USING DLDESCD,R7                                                       
PD0100   LA    RE,IOAREA           CLEAR IOAREA                                 
         LA    RF,3000                                                          
         XCEF                                                                   
*                                                                               
VDT0455  LA    R7,IOAREA+2                                                      
         MVI   DLDSEL,DLDSELQ      X'64' POSTING DESCR ELEMENT                  
         MVC   DLDSREF,DETLMMB                                                  
         MVC   DLDSDATE,MEMDTE     MEMO BILL DATE                               
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         MVI   DLDSLEN,DLDSLN1Q    NO NARRATIVE                                 
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         USING CTABD,R5                                                         
         LA    R3,1                                                             
         MVI   LINENO,1                                                         
         STC   R3,STCONTR          YES, START CONTRACT                          
         GOTO1 =A(DETLLIST),DMCB,0,(RC),RR=RELO1                                
         LA    R5,CNTRTABL                                                      
*--------------------------------------------------------------------           
* DEBIT CONTRACTOR                                                              
*--------------------------------------------------------------------           
VDT0460  CLI   CSACT,ACTCHA        CHECK WE HAVE SOMETHING TO POST              
         BNE   VDT0463                                                          
         TM    VNDSTAT,VNDORDCH    WAS VENDOR CHANGED?                          
         BO    VDT0463             HAVE TO POST EVERYTHING                      
         TM    CTABSTAT,CTABSCHA   WAS THIS CHANGED?                            
         BZ    VDT0470             NO NEED TO WRITE IT OUT                      
*                                                                               
         USING GINELD,R7                                                        
VDT0463  CP    CTABQTY,=P'0'       NONE ORDERED                                 
         BE    VDT0470             SKIP TO NEXT ONE                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         MVI   GINTYP,0                                                         
         STCM  R3,3,GINHISN                                                     
*                                                                               
         IC    RE,1(R7)                                                         
         AR    R7,RE                                                            
         BAS   RE,PSTFFNEL                                                      
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         MVC   CNTNUM,SPACES                                                    
         MVC   CNTNUM(1),COMPANY                                                
         MVC   CNTNUM+1(L'CTABCNTR),CTABCNTR                                    
         OC    CNTNUM+1(L'CTABCNTR),SPACES                                      
         L     R6,AIO2                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CNTNUM),CNTNUM                                             
         BAS   RE,GETACC                                                        
         MVC   CNTNAME,ACCTNAME                                                 
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         AR    R7,RE                                                            
         CLI   LINENO,SCRNMAX      EXCEED A PAGE?                               
         BNH   VDT0465             NO, CONTINUE                                 
         STC   R3,STCONTR          YES, START CONTRACT                          
         GOTO1 =A(DETLLIST),DMCB,0,(RC),RR=RELO1                                
         LA    R2,DETCTRH          FIRST DETAIL LINE                            
         MVI   LINENO,1                                                         
*                                                                               
VDT0465  BAS   RE,BLDSSEL          BUILD SUB-SCREEN ELEMENT                     
*                                                                               
         USING SCIELD,R7           SUBSIDIARY ELEMENT                           
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITSJXP                                                 
         ZAP   DUB,CTABUPRC        ACTUAL COST TO VENDOR BY ITEM                
         MP    DUB,CTABQTY                                                      
         AP    DUB,CTABTAXS        S/H IS PART OF ACTUAL COST                   
         ZAP   SCIAMNT,DUB                                                      
         ZAP   SCINET,=P'0'                                                     
         MVC   SCISUBWC,ORDRCTGY   CATEGORY NEEDED FOR REPORTS                  
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         TM    PRGOSTAT,PRGPTAX    DID WE POST SUBSID ELEM?                     
         BO    VDT0468                                                          
         OI    PRGOSTAT,PRGPTAX                                                 
         CLC   UTAXLOCL,SPACES                                                  
         BNH   VDT0468                                                          
         AR    R7,RE                                                            
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITTTAX                                                 
         ZAP   DUB,TOTUTAX         TOTAL TAX                                    
         ZAP   SCIAMNT,DUB                                                      
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         USING DLPOSTD,R7                                                       
VDT0468  AR    R7,RE                                                            
         MVI   DLPSEL,DLPSECRQ     CREDIT                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CNTRACC    CONTRACTOR                                   
         MVC   DLPSDBNM,CNTRNAME                                                
         MVC   DLPSCRAC,CNTNUM     CONTRACT                                     
         MVC   DLPSCRNM,CNTNAME                                                 
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVOFFICE                                                
*                                                                               
         ZAP   P16,=P'0'                                                        
         CLC   UTAXLOCL,SPACES                                                  
         BNH   VDT0469                                                          
         AP    P16,CTABUPRC                                                     
         MP    P16,CTABQTY                                                      
         MP    P16,UTAXRT                                                       
         SRP   P16,58,5                                                         
*                                                                               
VDT0469  AP    P16,CTABTAXS                                                     
         ZAP   TOTDIFF,P16                                                      
*                                                                               
         ZAP   P16,CTABURET                                                     
         MP    P16,CTABQTY                                                      
         MP    P16,CTABRAT                                                      
         SRP   P16,62,5                                                         
         MP    TOTDIFF,=P'2'                                                    
         AP    TOTDIFF,P16                                                      
         ZAP   DLPSAMNT,TOTDIFF    AMOUNT TO CREDIT                             
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         USING GINELD,R7                                                        
         AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         MVI   GINTYP,1                                                         
         STCM  R3,3,GINISN                                                      
         IC    RE,1(R7)                                                         
*                                                                               
         AR    R7,RE                                                            
         BAS   RE,PSTFFNEL                                                      
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         USING DLPOSTD,R7                                                       
         AR    R7,RE                                                            
         MVI   DLPSEL,DLPSEDRQ     DEBIT                                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CNTRACC    CONTRACTOR                                   
         MVC   DLPSDBNM,CNTRNAME                                                
         MVC   DLPSCRAC,CNTNUM     CONTRACT                                     
         MVC   DLPSCRNM,CNTNAME                                                 
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVOFFICE                                                
         ZAP   DLPSAMNT,TOTDIFF    AMOUNT TO CREDIT                             
         IC    RE,1(R7)                                                         
*                                                                               
         USING GINELD,R7                                                        
VDT0470  LA    R5,CTABLNQ(R5)      BUMP TO NEXT CONTRACT                        
         LA    R2,CURLLNQ(R2)      NEXT DETAIL LINE                             
         LA    R3,1(R3)                                                         
         SR    RE,RE               INCREMENT LINENO                             
         IC    RE,LINENO                                                        
         LA    RE,1(RE)                                                         
         STC   RE,LINENO                                                        
*                                                                               
         CLM   R3,1,NUMCONTR       ARE WE DONE?                                 
         BNH   VDT0460             NOPE, KEEP AT IT                             
*                                                                               
VDT0479  ZAP   BOPL61,=P'0'                                                     
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
VDT0800  AR    R7,RE                                                            
         MVI   0(R7),0                                                          
         LA    R6,IOAREA-1                                                      
         SR    R7,R6                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         CLI   CSSPROG,1                                                        
         BE    *+8                                                              
         BAS   RE,PUTDAY                                                        
         ZAP   TRANSAMT,BOPL61                                                  
         ZAP   DUB,BOPL61                                                       
*                                                                               
         B     VDTX900                                                          
         SPACE 3                                                                
         DROP  R7                                                               
         EJECT                                                                  
VDT0899  DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$IVPFK)                                           
         B     VDTX999                                                          
*                                                                               
VDTX900  DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VDTX905  DS    0H                                                               
         CLI   CSSPROG,1                                                        
         BE    VDTX907                                                          
         XC    BOELEM,BOELEM                                                    
         LA    RF,BOELEM                                                        
         USING GINELD,RF                                                        
         MVI   GINEL,GINELQ        GROUP INVOICE NUMBER ELEMENT                 
         MVI   GINLN,GINLN3Q                                                    
         MVC   GININV,GROUPNO                                                   
         SR    RE,RE                                                            
         IC    RE,NUMCONTR                                                      
         STCM  RE,3,GINHISN                                                     
         OI    CSLSTCUR+(LSTBIND2-LSTTABD),LSTBISIN   BATCH HAS S/INVS          
         MVC   BOWORK1+11(1),CSBTYP                                             
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,(0,BOWORK1),BOELEM         
*                                                                               
         BAS   RE,UPDCPOS          HAVE TO UPDATE CPOELS FOR TODAY              
         GOTO1 AWRITE,AIO1                                                      
         BNE   ERRXIT                                                           
*                                                                               
         XC    GROUPNO,GROUPNO     INIT GROUP NO                                
         CLI   CSACT,ACTINP                                                     
         BNE   VDTX999                                                          
VDTX906  GOTO1 =A(PRTMEMO),DMCB,(RC),RR=RELO1                                   
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$AOKNX)                                           
         LA    R2,CONTABH                                                       
         CLI   CSACT,ACTINP                                                     
         BNE   VDTX999X                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   VNDPON,C'*'         PUT * TO PREVENT ACCIDENTAL ENTRY            
         XC    MEMDTE,MEMDTE       CLEAR MEMO BILL DATE                         
         OI    VNDPONH+6,X'80'                                                  
         MVC   UTAXLOCL,SPACES                                                  
         MVC   DETLMMB,SPACES                                                   
         B     VDTX999X                                                         
*                                                                               
VDTX907  XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(L'DETLMMB),DETLMMB                                       
         MVC   BOWORK1+11(1),CSBTYP                                             
         GOTO1 AADACDAY,BOPARM,IOAREA,BOPL61,(X'80',BOWORK1),0                  
         BAS   RE,CALLMAIN         RETURN TO MAIN SCREEN                        
         LA    R2,VNDPVNDH         SET CURSOR                                   
         CLC   UTAXLOCL,SPACES     LOCAL TAX USED?                              
         BNH   VDT0300                                                          
         BAS   RE,RDTREC           RE-READ TAX RECORD                           
         B     VDT0300             AND POST VENDOR INFO                         
*                                                                               
VDTX999  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
VDTX999X MVI   VNDSTAT,0           RESET STATUS BYTE                            
         B     EXIT                                                             
*                                                                               
BADKEY   MVC   FVMSGNO,=AL2(AE$IVPFK)                                           
         LA    R2,CONTABH                                                       
         B     VDTX999                                                          
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* PUT ORDER NUMBERS IN POSTINGS                                                 
*---------------------------------------------------------------------          
         USING FFNELD,R7                                                        
PSTFFNEL MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNONUM,INVORDNO                                                 
         TM    PRGOSTAT,PRGOPART   PARTIAL ORDER                                
         BZ    *+8                                                              
         MVI   FFNSTAT,FFNSPRTQ                                                 
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* UPDATES THE TODAY FIELDS OF CPOELS                                            
*---------------------------------------------------------------------          
         USING ORDRECD,R5                                                       
UPDCPOS  NTR1                                                                   
         XC    KEY,KEY             FIND ORDER RECORD                            
         LA    R5,KEY                                                           
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,VNDPON      MOVE IN ORDER NUMBER                         
         GOTO1 READ                                                             
         USING OAMELD,R3                                                        
         L     R3,AIO1                                                          
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                 HAS TO HAVE ORDER AMOUNT ELEMENT             
         DC    H'0'                                                             
*                                                                               
         MVC   OAMLAST,TODAYP                                                   
         AP    OAMTVAL,VNDAMT                                                   
*                                                                               
         USING CPOELD,R3                                                        
         L     R3,AIO1                                                          
         MVI   ELCODE,CPOELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                 HAS TO HAVE CONTRACT PO ELEMENTS             
         DC    H'0'                                                             
*                                                                               
         USING CTABD,R5                                                         
         LA    R5,CNTRTABL                                                      
         SR    R6,R6                                                            
         IC    R6,NUMCONTR         NUBMER OF CONTRACTS                          
UPDCP100 CLC   CPOSTOCK,CTABSTKN   STOCK NUMBERS MUST MATCH                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CPOLAST,TODAYP                                                   
         BE    *+16                                                             
         ZAP   CPOQTYTD,=P'0'                                                   
         ZAP   CPOAMTTD,=P'0'                                                   
*                                                                               
         AP    CPOQTYTD,CTABQTY    UPDATE   QUANTITY / AMOUNT / DATE            
         ZAP   BCDUB,CTABQTY                                                    
         MP    BCDUB,CTABUPRC                                                   
         AP    CPOAMTTD,BCDUB                                                   
         MVC   CPOLAST,TODAYP                                                   
*                                                                               
         LA    R5,CTABLNQ(R5)      BUMP TO NEXT                                 
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         BCT   R6,UPDCP100                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* VALIDATES STANDARD COMMENT FIELD IN FOOTLINES                                 
*---------------------------------------------------------------------          
VALCOMM  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,5(R2)            ANY FOOTLINE COMMENTS?                       
         LTR   R1,R1                                                            
         BZ    VCXIT                                                            
         XC    COMMCODE,COMMCODE                                                
         MVC   DUB(5),8(R2)                                                     
         OC    DUB(4),SPACES       SHIFT TO UPPER CASE                          
         CLC   DUB(5),=C'NARR='                                                 
         BNE   VCXIT                                                            
         SH    R1,=H'6'                                                         
         EX    R1,*+4                                                           
         MVC   COMMCODE(0),13(R2)                                               
         EX    R1,*+4                                                           
         OC    COMMCODE(0),SPACES  SHIFT TO UPPER CASE                          
         GOTO1 RIGHT,DMCB,COMMCODE,L'COMMCODE                                   
*                                                                               
         USING SCMRECD,R5                                                       
         MVC   FVMSGNO,=AL2(AE$INVCD)                                           
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         MVI   SCMKTYP,SCMKTYPQ    X'0C'                                        
         MVC   SCMKCPY,COMPANY                                                  
         MVC   SCMKCODE,COMMCODE                                                
         GOTO1 READ                                                             
VCXIT    B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* READS TAX RECORD                                                              
*---------------------------------------------------------------------          
         USING SUTRECD,R5                                                       
RDTREC   NTR1                                                                   
         MVC   KEY,SPACES          READ TAX RECORD                              
         LA    R5,KEY                                                           
         MVI   SUTKTYP,SUTKTYPQ    X'2D'                                        
         MVI   SUTKSUB,SUTKSUBQ    X'01'                                        
         MVC   SUTKCPY,COMPANY                                                  
         MVC   SUTKLOC,UTAXLOCL                                                 
         GOTO1 HIGH                                                             
         LA    R2,DETLOCLH                                                      
         MVC   FVMSGNO,=AL2(AE$ITAX)                                            
         CLC   IOKEYSAV(L'SUTKEY),KEY                                           
         BNE   XITCCNE                                                          
*                                                                               
         XC    UTAXCRAC,UTAXCRAC                                                
         L     R5,AIO1                                                          
         USING SUTELD,R3                                                        
         LA    R3,ACCORFST(R5)     FIRST ELEMENT                                
RDT30    CLI   0(R3),0             EOR?                                         
         BE    RDT50                                                            
         CLI   SUTEL,SUTELQ                                                     
         BNE   RDT40                                                            
         CLC   SUTEFF,INVDTE       GET CLOSEST EFFECTIVE RATE                   
         BH    RDT50                                                            
         CLI   SUTLN,SUTLN1Q                                                    
         BNH   *+10                                                             
         MVC   UTAXCRAC(1),COMPANY                                              
         MVC   UTAXCRAC+1(L'SUTACC),SUTACC     CREDIT ACCOUNT                   
         ZAP   UTAXRT,SUTRTE                                                    
RDT40    ZIC   R0,1(R3)            BUMP TO NEXT                                 
         AR    R3,R0                                                            
         B     RDT30                                                            
*                                                                               
RDT50    OC    UTAXCRAC,UTAXCRAC           ERROR                                
         BZ    XITCCNE                                                          
         MVC   KEY(L'UTAXCRAC),UTAXCRAC                                         
         BAS   RE,GETACC                                                        
         MVC   UTAXCANM,ACCTNAME                                                
         B     XITCCEQ                                                          
         EJECT                                                                  
*---------------------------------------------------------------------          
* SETUP SCREEN WHEN SWITCHING                                                   
*---------------------------------------------------------------------          
SCRSETUP NTR1                                                                   
         MVC   DETVNDT(5),=CL5'Prod.'                                           
         TM    VNDSTAT,VNDEXPNS                                                 
         BZ    *+10                                                             
         MVC   DETVNDT(5),=CL5'EXP.'                                            
         OI    DETVNDTH+6,X'80'                                                 
         MVC   DETVND,VENDORAC                                                  
         MVC   DETVNDN,VNDNAME                                                  
         EDIT  VNDAMT,(12,DETVNDA),2,FLOAT=-                                    
*                                                                               
         MVI   DETVNDAH+5,L'DETVNDA                                             
         MVC   DETXACT,EXPNACC                                                  
         MVC   DETXATN,EXPNAME                                                  
         MVC   DETINRF,INVREF                                                   
         MVC   DETINRFH+5(1),INVREFLN                                           
         GOTO1 DATCON,DMCB,(1,INVDTE),(5,DETDATE)                               
         GOTO1 DATCON,DMCB,(1,INVDDTE),(5,DETDUED)                              
         OC    MEMDTE,MEMDTE                                                    
         BZ    SSET30                                                           
         GOTO1 DATCON,DMCB,(1,MEMDTE),(5,DETMEMD)                               
         MVI   DETMEMDH+5,8                                                     
SSET30   LA    R2,DETVNDH                                                       
*                                                                               
         MVC   DETORDN,INVORDNO                                                 
         MVC   DETORDN+L'DETORDN-2(2),SPACES                                    
         TM    PRGOSTAT,PRGOPART   PARTIAL ORDER?                               
         BZ    *+10                                                             
         MVC   DETORDN+L'DETORDN-2(2),=C',P'                                    
         OI    DETORDNH+6,X'80'                                                 
         CLC   DETLMMB,SPACES                                                   
         BNH   *+8                                                              
         MVI   DETMEMBH+5,L'DETLMMB                                             
         MVC   DETLOCL,UTAXLOCL                                                 
         OI    DETLOCLH+6,X'80'                                                 
*                                                                               
         BAS   RE,PRODETS          PROTECT DETAILS, DISPLAY ONLY                
         CLI   CSACT,ACTINP                                                     
         BE    SSET50                                                           
*                                                                               
SSET40   GOTO1 =A(READGIN),DMCB,(RC),RR=RELO1   READ SAVED CONTRACTS            
         OC    MEMDTE,MEMDTE                                                    
         BNZ   SSET55                                                           
         MVC   DETMEMD,DETDATE                                                  
         MVC   DETMEMDH+5(1),DETDATEH+5                                         
         OI    DETMEMDH+6,X'80'                                                 
         OI    DETMEMDH+1,X'08'    HIGH INTENSITY                               
         MVC   MEMDTE,INVDTE                                                    
         B     *+8                                                              
*                                                                               
SSET50   BAS   RE,RDCNTR           READ CONTRACTS                               
SSET55   CLC   UTAXLOCL,SPACES                                                  
         BNH   *+8                                                              
         BAS   RE,RDTREC                                                        
         GOTO1 =A(DETLLIST),DMCB,0,(RC),RR=RELO1  LIST CONTRACTS                
         BAS   RE,TOTCNTR                                                       
         BAS   RE,SHOWTOTS                                                      
*                                                                               
         LA    R2,DETMEMBH                                                      
*                                                                               
SSET90   B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* DELETE OLD GINPAS FOR VENDOR POSTINGS                                         
*---------------------------------------------------------------------          
         USING GINPASD,RE                                                       
DELGMAIN NTR1                                                                   
         LA    RE,IOKEY                                                         
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO                                                  
         GOTO1 ADELGIN                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* DELETE OLD GINPAS BASED ON SCREEN CHANGES                                     
*---------------------------------------------------------------------          
         USING GINPASD,RE                                                       
         USING CTABD,R5                                                         
DELOLDGN NTR1                                                                   
         LA    R5,CNTRTABL                                                      
         LA    R7,1                                                             
DOLD050  TM    VNDSTAT,VNDORDCH    VENDOR CHANGED?                              
         BO    DOLD060                                                          
         TM    CTABSTAT,CTABSCHA   CHANGED?                                     
         BZ    DOLD100                                                          
*                                                                               
DOLD060  LA    RE,IOKEY                                                         
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO                                                  
         STCM  R7,3,GINPISN                                                     
         GOTO1 ADELGIN                                                          
*                                                                               
DOLD100  LA    R5,CTABLNQ(R5)                                                   
         LA    R7,1(R7)                                                         
         TM    VNDSTAT,VNDORDCH    VENDOR ORDER CHANGED?                        
         BZ    DOLD150                                                          
         CLM   R7,1,OLDNUMCT       COMPARE AGAINST LAST SAVED                   
         BNH   DOLD050                                                          
         B     EXIT                                                             
*                                                                               
DOLD150  CLM   R7,1,NUMCONTR       ARE WE DONE?                                 
         BNH   DOLD050                                                          
         B     EXIT                                                             
         DROP  R5,RE                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
* PROTECT SUB-SCREEN LINES                                                      
*---------------------------------------------------------------------          
         USING CURLINED,R2                                                      
PRODETS  NTR1                                                                   
         TM    VNDSTAT,VNDOCHCK                                                 
         BZ    *+8                                                              
         OI    DETLOCLH+1,FATBPROT         PROTECT                              
         CLI   CSACT,ACTINP                                                     
         BE    PRODETS3                                                         
         OI    DETMEMBH+1,FATBPROT         PROTECT                              
         OI    DETVNDAH+1,FATBPROT         PROTECT                              
         OI    DETMEMDH+1,FATBPROT         PROTECT                              
         OI    DETLOCLH+1,FATBPROT         PROTECT                              
PRODETS3 OI    DETMEMBH+6,FOUTTRN          TRANSMIT                             
         OI    DETVNDAH+6,FOUTTRN          TRANSMIT                             
         OI    DETMEMDH+6,FOUTTRN          TRANSMIT                             
         OI    DETLOCLH+6,FOUTTRN          TRANSMIT                             
*                                                                               
         TM    VNDSTAT,VNDOCSBP            CASH / BILLPAY?                      
         BZ    PRODETS4                                                         
         MVC   DETTTL1+37(L'DETTTL1-37),SPACES                                  
         MVC   DETTTL1+15(25),=CL25'Descript       Amount'                      
         MVC   DETTTL2+37(L'DETTTL2-37),SPACES                                  
         MVC   DETTTL3+13(L'DETTTL3-13),SPACES                                  
         OI    DETTOTRH+1,FATBLOW                                               
         OI    DETTOTTH+1,FATBLOW                                               
         OI    DETTOTCH+1,FATBLOW                                               
         OI    DETTTL1H+6,FOUTTRN          TRANSMIT                             
         OI    DETTTL2H+6,FOUTTRN                                               
         OI    DETTTL3H+6,FOUTTRN                                               
         OI    DETTOTRH+6,FOUTTRN                                               
         OI    DETTOTTH+6,FOUTTRN                                               
         OI    DETTOTCH+6,FOUTTRN                                               
*                                                                               
PRODETS4 LA    RE,SCRNMAX                                                       
         LA    R2,DETCTRH                                                       
PRODETS5 DS    0H                                                               
         TM    VNDSTAT,VNDOCSBP    CASH / BILLPAY?                              
         BZ    PRODETS6                                                         
         OI    CURURETH+1,FATBLOW                                               
         OI    CURTAXH+1,FATBPROT+FATBLOW  PROTECT AND HIDE                     
         OI    CURRATH+1,FATBLOW                                                
         OI    CURCALH+1,FATBLOW                                                
         OI    CURURETH+6,FOUTTRN           TRANSMIT                            
         OI    CURTAXH+6,FOUTTRN                                                
         OI    CURRATH+6,FOUTTRN                                                
         OI    CURCALH+6,FOUTTRN                                                
*                                                                               
PRODETS6 CLI   CSACT,ACTINP                PROTECT QTY/TAX FOR DISPLAY          
         BNE   PRODETS7                                                         
         TM    VNDSTAT,VNDOCHCK                                                 
         BO    PRODETS7                    PROTECT ALL FOR CHECK ORDER          
         TM    PRGOSTAT,PRGOPART           PARTIAL ORDER?                       
         BO    PRODETS9                                                         
         OI    CURQTYH+1,FATBPROT          FULL ORDER, JUST PROTECT QTY         
         B     PRODETS9                                                         
*                                                                               
PRODETS7 OI    CURQTYH+1,FATBPROT          PROTECT                              
         OI    CURTAXH+1,FATBPROT                                               
PRODETS9 OI    CURQTYH+6,FOUTTRN           TRANSMIT                             
         OI    CURTAXH+6,FOUTTRN           TRANSMIT                             
         LA    R2,CURLLNQ(R2)              BUMP TO NEXT DETAIL LINE             
         BCT   RE,PRODETS5                                                      
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* BUILD SUB-SCREEN DETAIL LINES                                                 
*        R2 POINTS TO CURRENT DETAIL LINE                                       
*        R5 POINTS TO CURRENT TABLE ENTRY                                       
*---------------------------------------------------------------------          
         USING CURLINED,R2                                                      
         USING SFSELD,R7                                                        
BLDSSEL  NTR1                                                                   
         MVI   SFSEL,SFSELQ        BUILD SCREEN ELEMENT                         
         MVC   SFSFLDN,DETINRFX    INVOICE REFERENCE                            
         SR    RF,RF                                                            
         IC    RF,DETINRFH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SFSFIELD(0),DETINRF                                              
         LA    RF,SFSLN1Q+1(RF)                                                 
         STC   RF,SFSLN                                                         
         AR    R7,RF                                                            
*                                                                               
         MVI   SFSEL,SFSELQ        BUILD SCREEN ELEMENT                         
         MVC   SFSFLDN,DETMEMBX    MEMO BILL NUMBER                             
         SR    RF,RF                                                            
         IC    RF,DETMEMBH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SFSFIELD(0),DETMEMB                                              
         LA    RF,SFSLN1Q+1(RF)                                                 
         STC   RF,SFSLN                                                         
         AR    R7,RF                                                            
*                                                                               
         MVI   SFSEL,SFSELQ        BUILD SCREEN ELEMENT                         
         MVC   SFSFLDN,DETMEMDX    MEMO BILL DATE                               
         SR    RF,RF                                                            
         IC    RF,DETMEMDH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SFSFIELD(0),DETMEMD                                              
         LA    RF,SFSLN1Q+1(RF)                                                 
         STC   RF,SFSLN                                                         
         AR    R7,RF                                                            
*                                                                               
         CLC   UTAXLOCL,SPACES                                                  
         BNH   BLDSS10                                                          
         MVI   SFSEL,SFSELQ        BUILD SCREEN ELEMENT                         
         MVC   SFSFLDN,DETLOCLX    LOCALITY                                     
         MVC   SFSFIELD(L'UTAXLOCL),UTAXLOCL                                    
         MVI   SFSLN,SFSLN1Q+L'UTAXLOCL                                         
         LA    RF,SFSLN1Q+L'UTAXLOCL                                            
         AR    R7,RF                                                            
*                                                                               
BLDSS10  SR    R0,R0                                                            
         LA    R3,CURCTRH                                                       
         LA    R2,CURCALH                                                       
BLDSS20  TM    FVATRB-FVIHDR(R3),FVAXTND      EXTENDED FIELD HEADER?            
         BZ    BLDSS60                                                          
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R3)                                             
         AR    RF,R3                                                            
         SH    RF,=Y(L'FVIHDR)     RF=A(EXTENDED FIELD HEADER)                  
         CLI   0(RF),0             TEST FIELD NUMBER SET                        
         BE    BLDSS60                                                          
         MVI   SFSEL,SFSELQ        BUILD SCREEN SAVE ELEMENT                    
         MVC   SFSFLDN,0(RF)                                                    
         LA    RE,L'FVIHDR(R3)     RE=A(FIELD DATA)                             
BLDSS40  BCTR  RF,0                LOCATE END OF FIELD                          
         CR    RF,RE                                                            
         BL    BLDSS60                                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         B     BLDSS40                                                          
         SR    RF,RE               RF=L'DATA+1                                  
         CLI   SFSFLDN,130         CONTRACT                                     
         BNE   *+10                                                             
         LA    RF,2(RF)                                                         
         LR    RE,R5                                                            
*                                                                               
         EX    RF,*+4                                                           
         MVC   SFSFIELD(0),0(RE)                                                
BLDSS50  CLI   SFSFIELD,C' '       LEFT PADDED W/ SPACES?                       
         BH    BLDSS55                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SFSFIELD(0),SFSFIELD+1                                           
         B     BLDSS50                                                          
*                                                                               
BLDSS55  LA    RF,SFSLN1Q+1(RF)    RF=L'ELEMENT                                 
         STC   RF,SFSLN                                                         
         AR    R7,RF                                                            
BLDSS60  IC    R0,FVTLEN-FVIHDR(R3) BUMP TO NEXT TWA FIELD                      
         AR    R3,R0                                                            
         CR    R3,R2               TEST END OF TWA                              
         BL    BLDSS20                                                          
         XIT1  REGS=(R7)                                                        
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* READ CONTRACT INTO CONTRACT TABLE                                             
*---------------------------------------------------------------------          
         USING ORDRECD,R5                                                       
RDCNTR   NTR1                                                                   
         XC    KEY,KEY             READ ORDER RECORD                            
         LA    R5,KEY                                                           
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,INVORDNO    MOVE IN ORDER NUMBER                         
         GOTO1 READ                                                             
*                                                                               
         MVC   DETMEMB,DETLMMB                                                  
         OI    DETMEMBH+6,X'80'                                                 
         TM    VNDSTAT,VNDORDCH    DID WE CHANGE ORDER?                         
         BZ    RDC0120             NO, DON'T ERASE TABLE                        
         LA    RE,CNTRTABL         CLEAR CONTRACT TABLE                         
         LA    RF,CNTRTABS         CONTRACT TABLE SIZE                          
         XCEF                                                                   
*                                                                               
         USING OAMELD,R3                                                        
         USING CTABD,R7                                                         
RDC0100  MVI   NUMCONTR,0          INIT                                         
         LA    R2,QTYTABL          QUANTITY TABLE                               
         LA    R7,CNTRTABL                                                      
         SR    R6,R6                                                            
         L     R3,AIO1                                                          
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
RDC0110  BNE   RDC0119                                                          
         CLI   OAMLN,OAMLN2Q       MAKE SURE WE HAVE A CONTRACT                 
         BNL   RDC0115                                                          
RDC0113  BAS   RE,NEXTEL                                                        
         B     RDC0110                                                          
*                                                                               
RDC0115  MVC   ORDRCTGY,OAMWORK                                                 
         TM    VNDSTAT,VNDORDCH                                                 
         BZ    RDC0117                                                          
         MVC   LASTCNTR,OAMCONT                                                 
*                                                                               
         ZIC   R0,OAMLN                                                         
         USING CPOELD,R3                                                        
         AR    R3,R0                                                            
RDC0115A CLI   CPOEL,0                                                          
         BE    RDC0119                                                          
         CLI   CPOEL,CPOELQ                                                     
         BE    RDC0115B                                                         
         IC    R0,CPOLN                                                         
         AR    R3,R0                                                            
         B     RDC0115A                                                         
*                                                                               
RDC0115B MVC   CTABCNTR,LASTCNTR                                                
         ZAP   CTABQTY,CPOQTY                                                   
         SP    CTABQTY,CPOQTYIN            MINUS THOSE INVOICED                 
         CLC   CPOLAST,TODAYP              HAS ANY BEEN CHANGED TODAY?          
         BNE   *+10                                                             
         SP    CTABQTY,CPOQTYTD            YES, ADJUST TOTAL QUANTITY           
         ZAP   0(L'CTABQTY,R2),CTABQTY     PUT IN QUANTITY TABLE                
*                                                                               
         MVC   CTABSTKN,CPOSTOCK                                                
         ZAP   CTABUPRC,CPOUPRI                                                 
         ZAP   CTABURET,CPOURET                                                 
         ZAP   CTABTAXS,=P'0'                                                   
         ZAP   CTABRAT,CPORATIO                                                 
*                                                                               
RDC0117  LA    R7,CTABLNQ(R7)      BUMP TO NEXT TABLE ENTRY                     
         LA    R2,L'CTABQTY(R2)                                                 
         LA    R6,1(R6)                                                         
         LR    RF,R3                                                            
         ZIC   R0,CPOLN                                                         
         AR    R3,R0                                                            
         CLI   CPOEL,CPOELQ        ANOTHER ONE FOR CONTRACT                     
         BE    RDC0115B                                                         
         LR    R3,RF                                                            
         B     RDC0113                                                          
*                                                                               
RDC0119  STC   R6,NUMCONTR         SAVE NUMBER OF CONTRACTS                     
         XC    0(CTABLNQ,R7),0(R7)                                              
RDC0120  NI    VNDSTAT,X'FF'-VNDORDCH                                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* TOTAL CONTRACT TABLE                                                          
*---------------------------------------------------------------------          
         USING CTABD,R7                                                         
TOTCNTR  NTR1                                                                   
         ZAP   TOTUPRC,=P'0'                                                    
         ZAP   TOTURET,=P'0'                                                    
         ZAP   TOTTAXS,=P'0'                                                    
         ZAP   TOTCALC,=P'0'                                                    
         ZAP   DETTOT,=P'0'                                                     
         ZAP   TOTSTAX,=P'0'                                                    
         ZAP   TOTUTAX,=P'0'                                                    
*                                                                               
         LA    R7,CNTRTABL                                                      
         SR    R6,R6                                                            
         IC    R6,NUMCONTR                                                      
*                                                                               
TOTC100  DS    0H                                                               
         CP    CTABQTY,=P'0'                                                    
         BE    TOTC150                                                          
         ZAP   UTAX,=P'0'                                                       
         CLC   UTAXLOCL,SPACES                                                  
         BNH   TOTC105                                                          
*                                                                               
         ZAP   P16,CTABUPRC                                                     
         MP    P16,CTABQTY                                                      
         MP    P16,UTAXRT                                                       
         SRP   P16,58,5                                                         
         ZAP   UTAX,P16                                                         
*                                                                               
TOTC105  ZAP   P16,CTABUPRC                                                     
         MP    P16,CTABQTY                                                      
         AP    TOTUPRC,P16                                                      
         AP    DETTOT,P16                                                       
*                                                                               
         ZAP   P16,CTABURET                                                     
         MP    P16,CTABQTY                                                      
         AP    TOTURET,P16                                                      
*                                                                               
         CP    CTABQTY,=P'0'                                                    
         BE    *+10                                                             
         AP    TOTSTAX,CTABTAXS    ADD UP TAX/SHIP                              
*                                                                               
         ZAP   P16,CTABURET                                                     
         MP    P16,CTABQTY                                                      
         MP    P16,CTABRAT                                                      
         SRP   P16,62,5            GET RID OF 2 DECIMALS                        
         ZAP   TOTDIFF,CTABTAXS    TAX / S&H                                    
         AP    TOTDIFF,UTAX                                                     
         MP    TOTDIFF,=P'2'                                                    
         AP    P16,TOTDIFF                                                      
         AP    TOTCALC,P16         QTY * RET * RATIO + (2 * TAX)                
*                                                                               
TOTC150  LA    R7,CTABLNQ(R7)                                                   
         BCT   R6,TOTC100                                                       
*                                                                               
         CLC   UTAXLOCL,SPACES                                                  
         BNH   TOTC200                                                          
         ZAP   P16,TOTUPRC         CALCULATE USED TAX                           
         MP    P16,UTAXRT                                                       
         SRP   P16,58,5                                                         
         ZAP   UTAX,P16                                                         
         ZAP   TOTUTAX,P16                                                      
*                                                                               
TOTC200  ZAP   TOTTAXS,TOTSTAX     TOTAL TAXES                                  
         AP    TOTTAXS,TOTUTAX                                                  
         ZAP   TOTTAXS2,TOTTAXS                                                 
         MP    TOTTAXS2,=P'2'                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
VALVEND  NTR1                                                                   
         ZIC   R3,0(R1)            0=PROD VENDOR, ELSE EXP VENDOR               
         L     R2,0(R1)            ADDRESS OF FIELD                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    R5,8(R2)                                                         
         LA    R7,KEY+1                                                         
         CLI   8(R2),C'*'          U/L OVERRIDE?                                
         BNE   *+14                                                             
         LA    R5,1(R5)                                                         
         BCTR  R1,0                                                             
         B     VALV010                                                          
*                                                                               
         LA    R7,KEY+3                                                         
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+1(2),ACMPSUPP   PROD                                         
*        MVC   VENDORPU,KEY+1                                                   
         LTR   R3,R3                                                            
         BZ    VALV010                                                          
         MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),ACMPSUPX   EXPENSE                                      
*        MVC   VENDORXU,KEY+1                                                   
*                                                                               
VALV010  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R5)                                                    
*                                                                               
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         TM    ACCTSTAT,X'10'      IS CLIENT LOCKED?                            
         BO    ERROR                                                            
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         OI    4(R2),X'20'                                                      
*                                                                               
         USING RATELD,R1                                                        
         ZAP   CDPRCNT,=P'0'                                                    
         SR    RE,RE                                                            
         L     R1,AIO1                                                          
         LA    R1,ACCORFST(R1)                                                  
VALV020  CLI   0(R1),0             EOR?                                         
         BE    EXIT                                                             
         CLI   RATEL,RATEDSCQ      DISCOUNT ELEMENT?                            
         BE    VALV030                                                          
         IC    RE,RATLN                                                         
         AR    R1,RE                                                            
         B     VALV020                                                          
*                                                                               
VALV030  SR    RF,RF                                                            
         ICM   RF,3,RATRATE        CASH DISCOUNT RATE                           
         CVD   RF,DUB                                                           
         ZAP   CDPRCNT,DUB                                                      
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
FINDCD   NTR1                                                                   
         MVC   KEY,ACCTNUM                                                      
         MVC   KEY+3(L'KEY-3),SPACES   USE LEDGER FROM VENDOR                   
         BAS   RE,READ             READ LEDGER RECORD                           
*                                                                               
         USING LDGELD,R1                                                        
         SR    RE,RE                                                            
         L     R1,AIO1                                                          
         LA    R1,ACCORFST(R1)                                                  
FCD10    CLI   0(R1),0             EOR?                                         
         BE    EXIT                                                             
         CLI   LDGEL,LDGELQ        LEDGER ELEMENT?                              
         BE    FCD20                                                            
         IC    RE,LDGLN                                                         
         AR    R1,RE                                                            
         B     FCD10                                                            
*                                                                               
FCD20    CLC   LDGCDSC,SPACES                                                   
         BH    FCD30                                                            
         MVI   CDSW,C'N'                                                        
         B     EXIT                                                             
*                                                                               
FCD30    MVC   CDACCT(1),COMPANY                                                
         MVC   CDACCT+1(L'CDACCT-1),LDGCDSC                                     
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CDACCT),CDACCT                                             
         BAS   RE,GETACC                                                        
         MVC   CDACCTNM,ACCTNAME                                                
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
VALDATE  NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         CLI   5(R2),0                                                          
         BZ    VDATE010                                                         
         GOTO1 VPERVAL,DMCB,(5(R2),8(R2)),(X'60',PVBLK)                         
         CLI   DMCB+4,4                                                         
         BNE   XITCCNE                                                          
*                                                                               
         USING PERVALD,R5                                                       
         LA    R5,PVBLK                                                         
         MVC   WORK(6),PVALESTA                                                 
         DROP  R5                                                               
         B     *+8                                                              
*                                                                               
VDATE010 BAS   RE,GETODAY                                                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    XITCCNE                                                          
         GOTO1 DATCON,DMCB,(1,SAVEDATE),(5,8(R2))                               
         OI    6(R2),X'80'                                                      
         B     XITCCEQ                                                          
         EJECT                                                                  
*---------------------------------------------------------------                
VALAMNT  NTR1                                                                   
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   0(R1),0             TEST OK                                      
         BNE   XITCCNE                                                          
         ZAP   DUB,4(8,R1)                                                      
XITCCEQ  SR    RE,RE                                                            
XITCCNE  LTR   RE,RE               EXIT W/ ERROR                                
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
* GET CONTRACTOR ACCT AND NAME                                                  
*---------------------------------------------------------------                
         USING ORDELD,R3                                                        
GETCNTR  NTR1                                                                   
         CLI   CSACT,ACTDSP        ACTION = DISPLAY                             
         BE    XITCCEQ             EXIT, WITHOUT READING                        
*                                                                               
         L     R3,AIO1             GET CONTRACTOR                               
         MVI   ELCODE,ORDELQ       MAKE SURE ORDER ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   XITCCNE                                                          
*                                                                               
         L     R6,AIO2             GET CONTRACTOR NAME                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ORDCONTR),ORDCONTR                                         
         BAS   RE,GETACC                                                        
         MVC   CNTRACC,ACCTNUM                                                  
         MVC   CNTRNAME,ACCTNAME                                                
         MVC   CNTRADD1,SPACES                                                  
         MVC   CNTRADD2,SPACES                                                  
         MVC   CNTRADD3,SPACES                                                  
         MVC   CNTRADD4,SPACES                                                  
*                                                                               
         USING ADRELD,R3                                                        
         L     R3,AIO1                                                          
         MVI   ELCODE,ADRELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   XITCCEQ                                                          
         LA    RE,ADRADD1                                                       
         LA    RF,CNTRADD1                                                      
         SR    R1,R1                                                            
         IC    R1,ADRNUM                                                        
GCNTR50  MVC   0(L'CNTRADD1,RF),0(RE)                                           
         LA    RE,L'ADRADD1(RE)                                                 
         LA    RF,L'CNTRADD1(RF)                                                
         BCT   R1,GCNTR50                                                       
         B     XITCCEQ                                                          
         EJECT                                                                  
*---------------------------------------------------------------------          
* SHOW TOTALS ON DETAIL SCREEN                                                  
*---------------------------------------------------------------------          
SHOWTOTS DS    0H                                                               
         ZAP   TOTDIFF,VNDTOT                                                   
         SP    TOTDIFF,DETTOT                                                   
         EDIT  TOTUPRC,(12,DETTOTU),2,FLOAT=-                                   
         EDIT  TOTURET,(12,DETTOTR),2,FLOAT=-                                   
         EDIT  TOTTAXS,(8,DETTOTT),2,FLOAT=-                                    
         EDIT  TOTCALC,(12,DETTOTC),2,FLOAT=-                                   
         EDIT  TOTDIFF,(12,DETDIFF),2,FLOAT=-                                   
         OI    DETTOTUH+6,X'80'                                                 
         OI    DETTOTRH+6,X'80'                                                 
         OI    DETTOTTH+6,X'80'                                                 
         OI    DETTOTCH+6,X'80'                                                 
         OI    DETDIFFH+6,X'80'    TRANSMIT                                     
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*---------------------------------------------------------------------          
*                                                                               
MOVEFLD  ST    RE,SVRE                                                          
         ST    R3,SVR3                                                          
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
MOVEFLDX L     R3,SVR3                                                          
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*---------------------------------------------------------------------          
* CALL DETAIL SCREEN                                                            
*---------------------------------------------------------------------          
CALLDETS NTR1                                                                   
         STC   R1,BOBYTE1                                                       
         MVI   CSSPROG,1                                                        
         OI    CSOIND1,CSOIOVRS    OVERLAY CALLS SUB SCREEN                     
         OI    CSINDSL2,CSIOVKEP   KEEP OVERLAY VALUES                          
         GOTO1 ANTRSES,0                                                        
         GOTO1 AOVRSCR,BOPARM,(X'BC',CONTABH)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CSREC,RECSIT                                                     
         GOTO1 ARECACT,CSREC                                                    
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CALL MAIN SCREEN                                                    *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
CALLMAIN NTR1                                                                   
         STC   R1,BOBYTE1                                                       
         OI    CSINDSL2,CSIOVKEP   KEEP OVERLAY VALUES                          
         GOTO1 AXITSES                                                          
         MVI   CSREC,RECITE                                                     
         GOTO1 ARECACT,CSREC                                                    
         MVI   CSSPROG,0                                                        
         NI    CSOIND1,X'FF'-CSOIOVRS                                           
*                                                                               
         LA    R1,BASOLY2H         SET ALL UNPROTECTED FIELDS VALIDATED         
         USING FLDHDRD,R1                                                       
         LA    RF,OSVALS-1                                                      
         XR    RE,RE                                                            
CMAIN02  TM    FLDATB,FATBPROT                                                  
         BNZ   *+8                                                              
         OI    FLDIIND,FINPVAL                                                  
         ICM   RE,1,FLDLEN                                                      
         BZ    *+8                                                              
         BXLE  R1,RE,CMAIN02                                                    
*                                                                               
         LA    RF,VNDPVDAH                                                      
         TM    VNDSTAT,VNDEXPNS                                                 
         BZ    *+8                                                              
         LA    RF,VNDXVDAH                                                      
*                                                                               
         ZAP   VNDAMT,VNDTOT                                                    
         MVC   8(L'VNDPVDA,RF),SPACES                                           
         EDIT  VNDTOT,(12,8(RF)),2,FLOAT=-                                      
         OI    6(RF),X'80'                                                      
         XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
* SAVE BIO ELEMENT FOR ACTION DISPLAY                                           
*--------------------------------------------------------------                 
*                                                                               
         USING BIOELD,R3                                                        
         USING TBARECD,R4                                                       
SAVBIO   NMOD1 0,**SVBIO                                                        
         L     RC,4(R1)                                                         
         L     R4,AIO1                                                          
         CLI   TBAKTYP,TBAKTYPQ    HAS TO BE BATCH HEADER REC                   
         BNE   SAVBIOX                                                          
         CLC   TBAKCPY,COMPANY     COMPANY HAS TO MATCH                         
         BNE   SAVBIOX                                                          
         SR    R1,R1                                                            
         LA    R3,TBARFST          FIND BIOEL                                   
SAVBIO10 CLI   BIOEL,0             EOR?                                         
         BE    SAVBIOX                                                          
         CLI   BIOEL,BIOELQ                                                     
         BE    SAVBIO20                                                         
         IC    R1,BIOLN                                                         
         AR    R3,R1                                                            
         B     SAVBIO10                                                         
*                                                                               
SAVBIO20 XC    QTYTABL(MAXNCNTR*L'QTYTABL),QTYTABL                              
         ZAP   VNDAMT,BIOAMNT      TOTAL INVOICED AMOUNT                        
         SP    VNDAMT,BIOSTAMT     LESS SALES TAX AND HANDLING                  
         LA    R2,VNDPVDAH                                                      
         CLI   VNDPVNDH+5,0                                                     
         BNE   *+8                                                              
         LA    R2,VNDXVDAH                                                      
         EDIT  VNDAMT,(12,8(R2)),2,FLOAT=-                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R0,R3                                                            
         SR    R1,R1                                                            
         IC    R1,BIOLN                                                         
         AR    R0,R1               R0 IS THE LIMIT                              
*                                                                               
         LA    R3,BIOSTOCK                                                      
         USING BIOSTOCK,R3                                                      
         LA    R2,QTYTABL                                                       
SAVBIO30 CR    R3,R0               ARE WE DONE?                                 
         BNL   SAVBIOX             YES, LEAVE                                   
         CP    BIOSQTY,=P'0'                                                    
         BE    SAVBIO35                                                         
         ZAP   0(L'QTYTABL,R2),BIOSQTY                                          
         LA    R2,L'QTYTABL(R2)                                                 
SAVBIO35 LA    R3,BIOSUBQ(R3)                                                   
         B     SAVBIO30                                                         
*                                                                               
SAVBIOX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* UPDATES THE MEMO BILL NUMBER                                                  
*   R1 = ZERO, JUST GET MEMO BILL NUMBER                                        
*     <> ZERO, GET MEMO BILL NUMBER AND WRITE NEW ONE                           
*---------------------------------------------------------------------          
         USING LDGRECD,R5                                                       
         USING FFTELD,R3                                                        
UPDMEMO  NMOD1 0,**UPMM                                                         
         L     RC,4(R1)                                                         
         L     R4,0(R1)                                                         
*                                                                               
         LA    R5,KEY                                                           
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),=C'S1'   MEMO BILL NUMBER IN S1 LEDGER                
         GOTO1 READ                                                             
*                                                                               
         L     R3,AIO1                                                          
         MVI   ELCODE,FFTELQ       IN FREE FORM TEXT ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DETLMMB(0),FFTDATA  NEXT AVAILABLE MEMO BILL NUMBER              
*                                                                               
         LTR   R4,R4                                                            
         BZ    UPDMXIT                                                          
         PACK  DUB(8),FFTDATA+1(5)                                              
         NI    DUB+7,X'FC'         MAKE POSITIVE PACK                           
         AP    DUB,=P'1'           NEXT AVAILABLE NUMBER                        
         UNPK  FFTDATA+1(5),DUB(8)                                              
         OI    FFTDATA+5,X'F0'                                                  
*                                                                               
         GOTO1 AWRITE,AIO1         SAVE IT                                      
*                                                                               
UPDMXIT  XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
*====================================================================*          
* POSTINGS TO VENDOR                                                 *          
*====================================================================*          
         USING DLDESCD,R7                                                       
POSTVEN  DS    0D                                                               
         NMOD1 0,**PSTV                                                         
         L     RC,0(R1)                                                         
*                                                                               
         LA    RE,IOAREA           CLEAR IOAREA                                 
         LA    RF,3000                                                          
         XCEF                                                                   
*                                                                               
         ZAP   CASHDISC,=P'0'                                                   
         CLI   CDSW,C'Y'                                                        
         BNE   PSTV10                                                           
         OC    CDACCT,CDACCT                                                    
         BZ    PSTV10                                                           
         ZAP   DUB,VNDAMT                                                       
         AP    DUB,TOTSTAX                                                      
         MP    DUB,CDPRCNT                                                      
         SRP   DUB,64-4,5                                                       
         ZAP   CASHDISC,DUB                                                     
*                                                                               
PSTV10   ZAP   BOPL61,VNDAMT       AMOUNT = SUM OF PROD AND EXPENSE             
         AP    BOPL61,TOTSTAX                                                   
*                                                                               
         LA    R7,IOAREA+2                                                      
         MVI   DLDSEL,DLDSELQ      X'64' POSTING DESCR ELEMENT                  
         MVC   DLDSREF,INVREF                                                   
         MVC   DLDSDATE,INVDTE                                                  
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         MVI   DLDSLEN,DLDSLN1Q    NO NARRATIVE                                 
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         AR    R7,RE                                                            
         BAS   RE,PSTFFNL2         POST FFNELS                                  
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         USING GINELD,R7                                                        
         AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         SR    RE,RE                                                            
         STCM  RE,3,GINHISN                                                     
         IC    RE,GINLN                                                         
*                                                                               
         USING SCIELD,R7                                                        
         CLI   CDSW,C'Y'                                                        
         BNE   PSTV20                                                           
         OC    CDACCT,CDACCT       SHOW CD AS MEMO                              
         BZ    PSTV20                                                           
         AR    R7,RE                                                            
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,CASHDISC                                                 
         IC    RE,SCILN                                                         
*--------------------------------------------------------------------           
* CREDIT SUPPLIER                                                               
*--------------------------------------------------------------------           
*                                                                               
         USING DLPOSTD,R7                                                       
PSTV20   AR    R7,RE                                                            
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CNTRACC    CONTRA = CONTRACTOR                          
         MVC   DLPSDBNM,CNTRNAME                                                
         MVC   DLPSCRAC,VNDNUM                                                  
         MVC   DLPSCRNM,VNDNAME                                                 
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVOFFICE                                                
         ZAP   DLPSAMNT,BOPL61     AMOUNT TO CREDIT                             
         CLI   CDSW,C'Y'                                                        
         BNE   PSTV25                                                           
         OC    CDACCT,CDACCT       WAS THERE CD?                                
         BZ    PSTV25                                                           
         SP    DLPSAMNT,CASHDISC   YES, LESS CASH DISCOUNT                      
PSTV25   IC    RE,1(R7)                                                         
         LA    R3,1                                                             
*                                                                               
         USING GINELD,R7                                                        
PSTV30   AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         SR    RE,RE                                                            
         STCM  RE,3,GINHISN                                                     
         STC   R3,GINTYP                                                        
         IC    RE,GINLN                                                         
*                                                                               
         AR    R7,RE                                                            
         BAS   RE,PSTFFNL2                                                      
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*                                                                               
         BAS   RE,ASUBINFO         ADD SUB ITEM INFO INTO FFNEL                 
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
*--------------------------------------------------------------------           
* DEBIT EXPENSE                                                                 
*--------------------------------------------------------------------           
*                                                                               
         USING DLPOSTD,R7                                                       
         AR    R7,RE                                                            
         MVI   DLPSEL,DLPSEDRQ     DEBIT EXPENSE                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,EXPNUM                                                  
         MVC   DLPSDBNM,EXPNAME                                                 
         MVC   DLPSCRAC,CNTRACC    CONTRA = CONTRACTOR                          
         MVC   DLPSCRNM,CNTRNAME                                                
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVOFFICE                                                
         ZAP   DLPSAMNT,BOPL61     AMOUNT TO CREDIT                             
         IC    RE,1(R7)                                                         
         LA    R3,1(R3)                                                         
*--------------------------------------------------------------------           
* CASH DISCOUNT                                                                 
*--------------------------------------------------------------------           
*                                                                               
         CLI   CDSW,C'Y'                                                        
         BNE   PSTV50                                                           
         CLC   CDACCT,SPACES       IS THERE A CASH DISCOUNT?                    
         BNH   PSTV50                                                           
*                                                                               
         USING GINELD,R7                                                        
         AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         XC    GINHISN,GINHISN                                                  
         STC   R3,GINTYP                                                        
         IC    RE,GINLN                                                         
*                                                                               
         USING DLPOSTD,R7                                                       
         AR    R7,RE                                                            
         MVI   DLPSEL,DLPSECRQ     CREDIT INCOME                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CNTRACC    CONTRA = CONTRACTOR                          
         MVC   DLPSDBNM,CNTRNAME                                                
         MVC   DLPSCRAC,CDACCT                                                  
         MVC   DLPSCRNM,CDACCTNM                                                
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVOFFICE                                                
*                                                                               
         ZAP   DLPSAMNT,CASHDISC   AMOUNT TO CREDIT                             
         IC    RE,1(R7)                                                         
         LA    R3,1(R3)                                                         
*--------------------------------------------------------------------           
* LOCALITY TAX                                                                  
*--------------------------------------------------------------------           
*                                                                               
PSTV50   CLC   UTAXLOCL,SPACES                                                  
         BNH   PSTV80                                                           
*                                                                               
         USING GINELD,R7                                                        
         AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         XC    GINHISN,GINHISN                                                  
         STC   R3,GINTYP                                                        
         IC    RE,GINLN                                                         
*                                                                               
         USING DLPOSTD,R7                                                       
         AR    R7,RE                                                            
         MVI   DLPSEL,DLPSEDRQ     DEBIT                                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,EXPNUM                                                  
         MVC   DLPSDBNM,EXPNAME                                                 
         MVC   DLPSCRAC,UTAXCRAC                                                
         MVC   DLPSCRNM,UTAXCANM                                                
         MVC   DLPSANAL,SVOFFICE                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TOTUTAX                                                 
         IC    RE,1(R7)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
         USING GINELD,R7                                                        
         AR    R7,RE                                                            
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         XC    GINHISN,GINHISN                                                  
         STC   R3,GINTYP                                                        
         IC    RE,GINLN                                                         
*                                                                               
         USING DLPOSTD,R7                                                       
         AR    R7,RE                                                            
         MVI   DLPSEL,DLPSECRQ     CREDIT                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,EXPNUM                                                  
         MVC   DLPSDBNM,EXPNAME                                                 
         MVC   DLPSCRAC,UTAXCRAC                                                
         MVC   DLPSCRNM,UTAXCANM                                                
         MVC   DLPSANAL,SVOFFICE                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TOTUTAX                                                 
         IC    RE,1(R7)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
PSTV80   XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(L'INVORDNO),INVORDNO                                     
         XIT1  REGS=(R7)                                                        
         EJECT                                                                  
*---------------------------------------------------------------------          
* PUT ORDER NUMBERS IN POSTINGS                                                 
*---------------------------------------------------------------------          
         USING FFNELD,R7                                                        
PSTFFNL2 MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNONUM,INVORDNO                                                 
         TM    PRGOSTAT,PRGOPART   PARTIAL ORDER                                
         BZ    *+8                                                              
         MVI   FFNSTAT,FFNSPRTQ                                                 
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* ADDS SUB-ITEM INFO FOR FFNEL                                                  
*---------------------------------------------------------------------          
         USING CTABD,R5                                                         
         USING FFNELD,R7                                                        
         USING BIORTAMT,RF                                                      
ASUBINFO NTR1                                                                   
         LR    RF,R7                                                            
         SR    RE,RE                                                            
         IC    RE,1(R7)            CURRENT LENGTH OF ELEMENT                    
         AR    RF,RE                                                            
*                                                                               
         ZAP   BIORTAMT,TOTCALC                                                 
         ZAP   BIOUTAMT,=P'0'                                                   
         CLC   UTAXLOCL,SPACES                                                  
         BNH   *+10                                                             
         ZAP   BIOUTAMT,TOTUTAX                                                 
         ZAP   BIOSTAMT,TOTSTAX    SALES TAX/SHIP AMOUNT                        
         LA    RE,BIOLN2Q-BIOLNQ(RE)                                            
         LA    RF,BIOLN2Q-BIOLNQ(RF)                                            
*                                                                               
         USING BIOSTOCK,RF                                                      
         LA    R5,CNTRTABL                                                      
ASUB10   CLI   0(R5),0             EOT?                                         
         BE    ASUB99                                                           
*                                                                               
         MVC   BIOSTOCK,CTABSTKN                                                
         ZAP   BIOSQTY,CTABQTY                                                  
         ZAP   BIOSAMT,CTABUPRC                                                 
         LA    RE,BIOSUBQ(RE)                                                   
         LA    RF,BIOSUBQ(RF)                                                   
         LA    R5,CTABLNQ(R5)                                                   
         B     ASUB10                                                           
*                                                                               
ASUB99   STC   RE,FFNLN                                                         
         B     EXIT                                                             
         DROP  R5,R7                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* READ CONTRACT INFO FROM GINPAS PNTRS                                          
*---------------------------------------------------------------------          
         USING SFSELD,R3                                                        
         USING GINPASD,R4                                                       
         USING CTABD,R7                                                         
READGIN  DS    0D                                                               
         NMOD1 0,**RDGIN                                                        
         L     RC,0(R1)                                                         
*                                                                               
         XC    MEMDTE,MEMDTE                                                    
         SR    R5,R5               COUNTER OF CONTRACTS                         
         LA    R7,CNTRTABL                                                      
         LR    RE,R7               CLEAR CONTRACT TABLE                         
         LA    RF,CNTRTABS                                                      
         XCEF                                                                   
*                                                                               
         USING TBARECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVI   TBAKTYP,TBAKTYPQ    X'03'   BATCH HEADER RECORD                  
         MVC   TBAKCPY,COMPANY                                                  
*                                                                               
         USING GINPASD,R4                                                       
         LA    R4,IOKEY                                                         
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ    X'1E'                                        
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO     GROUP INVOICE NUMBER                         
         MVI   GINPISN+1,1                                                      
         XC    IODAOVER,IODAOVER                                                
         LA    R1,IOACCDIR+IOHI+IO1                                             
READG100 LA    R4,IOKEY                                                         
         GOTO1 AIO                                                              
READG105 BNL   *+6                                                              
         DC    H'0'                HARDWARE ERROR                               
         TM    IOERR,IOEEOF                                                     
         BZ    *+6                                                              
         DC    H'0'                EOF                                          
         CLC   IOKEYSAV(GINPISN-GINPASD),IOKEY    PASSIVE NOT MADE              
         BE    *+6                                                              
         DC    H'0'                HAVE TO HAVE THEM                            
*                                                                               
         TM    GINPKSTA,TRNSDELT   TEST DELETED                                 
         BZ    READG110                                                         
         LA    R1,IOACCDIR+IOSQD+IO1                                            
         B     READG100                                                         
READG110 MVC   IODAOVER,IODA                                                    
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    DETMEMB,DETMEMB     DID WE GET MEMO BILL NUMBER?                 
         BNZ   READG200                                                         
         L     R3,AIO1                                                          
         AH    R3,=Y(TRNRFST-TRNRECD)                                           
         MVI   ELCODE,SFSELQ       X'E1'                                        
READG115 BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         CLC   SFSFLDN,DETMEMBX    MEMO BILL NUMBER?                            
         BNE   READG117            HAS TO BE ONE                                
         SR    RF,RF                                                            
         IC    RF,SFSLN            LENGTH OF FREEFORM TEXT                      
         SH    RF,=Y(SFSLN1Q+1)                                                 
         CLM   RF,1,L'DETMEMB-1    DID WE EXCEED LENGTH OF FIELD?               
         BNH   *+8                                                              
         LA    RF,L'DETMEMB-1                                                   
         EX    RF,*+4                                                           
         MVC   DETMEMB(0),SFSFIELD    MOVE TEXT TO MEMO BILL NUMBER             
         OI    DETMEMBH+6,X'80'                                                 
         OI    DETMEMBH+1,X'08'    HIGH INTENSITY                               
         MVC   DETLMMB,DETMEMB                                                  
         B     READG115                                                         
*                                                                               
READG117 CLC   SFSFLDN,DETMEMDX    MEMO BILL DATE?                              
         BNE   READG120            HAS TO BE ONE                                
         SR    RF,RF                                                            
         IC    RF,SFSLN            LENGTH OF FREEFORM TEXT                      
         SH    RF,=Y(SFSLN1Q+1)                                                 
         CLM   RF,1,L'DETMEMD-1    DID WE EXCEED LENGTH OF FIELD?               
         BNH   *+8                                                              
         LA    RF,L'DETMEMD-1                                                   
         EX    RF,*+4                                                           
         MVC   DETMEMD(0),SFSFIELD    MOVE TEXT TO MEMO BILL DATE               
         LA    RF,1(RF)                                                         
         STC   RF,DETMEMDH+5                                                    
         OI    DETMEMDH+6,X'80'                                                 
         OI    DETMEMDH+1,X'08'    HIGH INTENSITY                               
         GOTO1 VALDATE,DMCB,DETMEMDH                                            
         MVC   MEMDTE,SAVEDATE                                                  
         B     READG115                                                         
*                                                                               
READG120 CLC   SFSFLDN,DETLOCLX    LOCALITY                                     
         BH    READG200                                                         
         BNE   READG115                                                         
         SR    RF,RF                                                            
         IC    RF,SFSLN                                                         
         SH    RF,=Y(SFSLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   DETLOCL(0),SFSFIELD                                              
         MVC   UTAXLOCL,DETLOCL                                                 
         OI    DETLOCLH+6,X'80'                                                 
*                                                                               
         USING CURLINED,R4                                                      
READG200 LA    R2,DETCTRH          READ IN SAVED CONTRACTS                      
         LR    R4,R2                                                            
         TWAXC CURCTRH,CURCALH,PROT=Y                                           
         L     R3,AIO1                                                          
         AH    R3,=Y(TRNRFST-TRNRECD)                                           
         MVI   ELCODE,SFSELQ                                                    
READG210 BAS   RE,NEXTEL                                                        
         BNE   READG300                                                         
         CLI   SFSFLDN,X'82'       IS IT A DETAIL LINE FIELD?                   
         BL    READG210            NO, SKIP IT                                  
         SR    R1,R1                                                            
         IC    R1,SFSLN                                                         
         SH    R1,=Y(SFSLN1Q)                                                   
         STC   R1,5(R4)            RESTORE INPUT LENGTH                         
         BCTR  R1,0                                                             
         CLI   SFSFLDN,X'82'                                                    
         BE    READG215                                                         
READG213 ZIC   RF,0(R4)                                                         
         SH    RF,=H'8'            MINUS EXTENDED FH LENGTH                     
         AR    RF,R4               POINT TO FIELD HEADER NUMBER                 
         CLC   SFSFLDN,0(RF)       HAVE TO MATCH                                
         BE    READG214                                                         
         MVI   5(R4),0                                                          
         LA    R4,8(RF)                                                         
         B     READG213                                                         
*                                                                               
READG214 EX    R1,*+4                                                           
         MVC   8(0,R4),SFSFIELD                                                 
         B     READG220                                                         
*                                                                               
READG215 EX    R1,*+4                                                           
         MVC   CTABCNTR(0),SFSFIELD                                             
*                                                                               
READG220 OI    6(R4),X'80'                                                      
         IC    R1,0(R4)                                                         
         AR    R4,R1                                                            
         B     READG210                                                         
*                                                                               
READG300 LR    R4,R2                                                            
         ZAP   CTABQTY,=P'0'                                                    
         CLI   CURQTYH+5,0                                                      
         BE    READG310                                                         
         CLI   CURQTY,C'*'                 CHECK ORDER?                         
         BNE   *+14                                                             
         ZAP   CTABQTY,=P'1'                                                    
         B     READG310                                                         
*                                                                               
         LA    R2,CURQTYH                                                       
         BAS   RE,VALAMNT                                                       
         SRP   DUB,62,0                                                         
         ZAP   CTABQTY,DUB                                                      
READG310 MVC   CTABSTKN,CURSTKN                                                 
         OC    CTABSTKN,SPACES                                                  
         LA    R2,CURUPRCH                                                      
         BAS   RE,VALAMNT                                                       
         ZAP   CTABUPRC,DUB                                                     
         LA    R2,CURURETH                                                      
         BAS   RE,VALAMNT                                                       
         ZAP   CTABURET,DUB                                                     
         LA    R2,CURTAXH                                                       
         BAS   RE,VALAMNT                                                       
         ZAP   CTABTAXS,DUB                                                     
*                                                                               
         LA    R2,CURRATH                                                       
         ZIC   R0,CURRATH+5                                                     
         GOTO1 CASHVAL,DMCB,(X'82',CURRAT),(R0)                                 
         ZAP   CTABRAT,4(8,R1)                                                  
*                                                                               
READG350 LA    R5,1(R5)                                                         
         LA    R1,IOACCDIR+IOSQD+IO1                                            
         GOTO1 AIO                 SKIP 2NDARY                                  
         TM    IOKEY+(GINPKSTA-GINPASD),TRNSDELT      DELETED?                  
         BO    READG350                                                         
         GOTO1 AIO                 GET NEXT ONE                                 
         CLC   IOKEYSAV(GINPISN-GINPASD),IOKEY    PASSIVE NOT MADE              
         BNE   READG400                                                         
         LA    R7,CTABLNQ(R7)                                                   
         B     READG105                                                         
*                                                                               
READG400 STC   R5,NUMCONTR                                                      
         OC    QTYTABL(MAXNCNTR*L'QTYTABL),QTYTABL                              
         BZ    READG500                                                         
         LA    R6,QTYTABL                                                       
         LA    R7,CNTRTABL                                                      
READG410 ZAP   CTABQTY,0(L'QTYTABL,R6)                                          
         CP    CTABQTY,=P'0'                                                    
         BH    READG420                                                         
         ZAP   DUB,CTABTAXS                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   CTABTAXS,DUB                                                     
*                                                                               
READG420 LA    R7,CTABLNQ(R7)                                                   
         LA    R6,L'QTYTABL(R6)                                                 
         BCT   R5,READG410                                                      
*                                                                               
READG500 XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R3,R4,R7                                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
* DETAIL LIST - LIST CONTRACTS ON DETAIL SCREEN                                 
*               P1= 0, DISPLAY                                                  
*               P1<>0, VALIDATE     CC=NE WHEN AMT IS BAD                       
*---------------------------------------------------------------------          
         USING CURLINED,R5                                                      
         USING CTABD,R7                                                         
DETLLIST DS    0D                                                               
         NMOD1 0,**DTLLST                                                       
         L     RC,4(R1)                                                         
*                                                                               
         L     R3,0(R1)            NON-ZERO, VALIDATE                           
         LTR   R3,R3                                                            
         BNZ   DTL0050                                                          
         TWAXC DETCTRH,DETTABH,PROT=Y           DISPLAY ONLY                    
*                                                                               
DTL0050  LA    R5,DETCTRH          FIRST LINE                                   
         LA    R4,QTYTABL                                                       
         LA    R7,CNTRTABL                                                      
         SR    R6,R6                                                            
         IC    R6,STCONTR          START CONTRACT                               
         BCTR  R6,0                                                             
         LR    R0,R6                                                            
         LTR   R6,R6               FIRST CONTRACT?                              
         BZ    DTL0100                                                          
         MH    R6,=Y(CTABLNQ)      FIND CONTRACT IN TABLE                       
         AR    R7,R6                                                            
         LR    R6,R0               RESTORE START CONTRACT                       
         MH    R6,=Y(L'CTABQTY)    FIND QTY IN QTY TABLE                        
         AR    R4,R6                                                            
         SR    R6,R6               INIT LINE COUNTER                            
*                                                                               
DTL0100  MVC   CURCTR,CTABCNTR+2                                                
         MVC   CURSTKN,CTABSTKN                                                 
*                                                                               
         LTR   R3,R3                                                            
         BZ    DTL0150                                                          
         TM    CURQTYH+4,X'80'     INPUT THIS TIME?                             
         BZ    DTL0120                                                          
         LA    R2,CURQTYH                                                       
         BAS   RE,VALAMNT                                                       
         BNE   DTLNO                                                            
         SRP   DUB,62,0            GET RID OF 2 DECIMALS, WHOLE NUMBER          
*                                                                               
         CP    DUB,0(L'CTABQTY,R4)   IS IT GREATER THAN MAX?                    
         BNH   DTL0110                                                          
         MVC   FVMSGNO,=AL2(AE$QTYXD)                                           
         EDIT  (P3,(R4)),(3,FVXTRA)                                             
         LA    RE,1                                                             
         B     DTLNO                                                            
*                                                                               
DTL0110  CP    CTABQTY,DUB         IF SAME, DON'T MARK CHANGED                  
         BE    DTL0120                                                          
         ZAP   CTABQTY,DUB                                                      
         OI    CTABSTAT,CTABSCHA   CHANGED                                      
*                                                                               
DTL0120  TM    CURTAXH+4,X'80'     INPUT THIS TIME?                             
         BZ    DTL0150                                                          
         LA    R2,CURTAXH                                                       
         BAS   RE,VALAMNT                                                       
         BNE   DTLNO                                                            
         CP    CTABTAXS,DUB        IF SAME, DON'T MARK CHANGED                  
         BE    DTL0150                                                          
         ZAP   CTABTAXS,DUB                                                     
         OI    CTABSTAT,CTABSCHA   CHANGED                                      
*                                                                               
DTL0150  TM    VNDSTAT,VNDOCHCK    CHECK ORDER                                  
         BZ    *+12                                                             
         MVI   CURQTY+L'CURQTY-1,C'*'                                           
         B     DTL0151                                                          
*                                                                               
         EDIT  CTABQTY,(5,CURQTY),FLOAT=-                                       
DTL0151  EDIT  CTABUPRC,(12,CURUPRC),2,FLOAT=-                                  
         EDIT  CTABURET,(12,CURURET),2,FLOAT=-                                  
         EDIT  CTABTAXS,(8,CURTAX),2,FLOAT=-                                    
         EDIT  CTABRAT,(6,CURRAT),2,FLOAT=-                                     
         ZAP   UTAX,=P'0'                                                       
         CLC   UTAXLOCL,SPACES                                                  
         BNH   DTL0153                                                          
         ZAP   P16,CTABQTY         CALCULATE USE TAX                            
         MP    P16,CTABUPRC                                                     
         MP    P16,UTAXRT                                                       
         SRP   P16,58,5                                                         
         ZAP   UTAX,P16                                                         
*                                                                               
DTL0153  ZAP   TOTCTR,=P'0'                                                     
         CP    CTABQTY,=P'0'                                                    
         BE    DTL0160                                                          
*                                                                               
DTL0155  ZAP   P16,CTABURET        QTY * ITEM AMOUNT * RATIO                    
         MP    P16,CTABQTY                                                      
         MP    P16,CTABRAT                                                      
         SRP   P16,62,5            GET RID OF 2 DECIMALS                        
         ZAP   TOTCTR,CTABTAXS     + TAX * 2                                    
         AP    TOTCTR,UTAX                                                      
         MP    TOTCTR,=P'2'                                                     
         AP    TOTCTR,P16                                                       
DTL0160  EDIT  TOTCTR,(12,CURCAL),2,FLOAT=-                                     
         OI    CURCTRH+6,X'80'                                                  
         OI    CURQTYH+6,X'80'                                                  
         OI    CURSTKNH+6,X'80'                                                 
         OI    CURUPRCH+6,X'80'                                                 
         OI    CURURETH+6,X'80'                                                 
         OI    CURTAXH+6,X'80'                                                  
         OI    CURRATH+6,X'80'                                                  
         OI    CURCALH+6,X'80'                                                  
*                                                                               
         LA    R7,CTABLNQ(R7)                                                   
         LA    R4,L'CTABQTY(R4)                                                 
         CLI   0(R7),0                                                          
         BE    DTLYES                                                           
         LA    R5,CURLLNQ(R5)                                                   
         LA    R6,1(R6)                                                         
         CH    R6,=Y(SCRNMAX)      PASSED MAX ON SCREEN                         
         BL    DTL0100                                                          
*                                                                               
DTLYES   SR    RE,RE                                                            
DTLNO    LTR   RE,RE                                                            
         XIT1  REGS=(R2)                                                        
         DROP  R7                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              WRITE A PRINT QUEUE ENTRY / PRINT MEMO                           
         USING REPD,R6                                                          
PRTMEMO  DS    0D                                                               
         NMOD1 SPOOLEND-SPOOLD,*PMEMO*,CLEAR=YES                                
         L     RC,0(R1)                                                         
         L     R6,AREP                                                          
         MVC   REPDESC(11),=CL11'PRINT MEMO'                                    
         MVC   REPSUBID,=C'CTA'                                                 
         OC    CSREPID,CSREPID     USE REPORT ID IF IT EXISTS                   
         BZ    *+10                                                             
         MVC   REPSUBID,CSREPID                                                 
         LA    RF,HOOK             USER HOOK ROUTINE                            
         ST    RF,REPAUSR                                                       
         MVI   REPHEADH,1                                                       
         MVI   REPACTN,REPAINI     INITIALIZE REPORT                            
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVI   REPACTN,REPAOPN     OPEN REPORT                                  
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LASTCNTR,SPACES                                                  
         MVI   REPACTN,REPAPUT                                                  
         MVC   REPPAGE,=Y(1)                                                    
         LA    RF,T61B3CSP         REPORT SPECS                                 
         ST    RF,REPAPHS                                                       
*                                                                               
         LA    R2,VNDNAR1H                                                      
         OI    REPHEADI,REPHFRCE   NOW PRINT THE MEMO                           
*                                                                               
         USING ORDRECD,R5                                                       
         LA    R5,KEY                                                           
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,INVORDNO                                                 
         GOTO1 READ                                                             
*                                                                               
         USING CPOELD,R3                                                        
         USING PLINED,R4                                                        
         USING CTABD,R5                                                         
         L     R4,REPAPRNT                                                      
         LA    R5,CNTRTABL                                                      
         ZAP   CSHTOT,=P'0'                                                     
         L     R3,AIO1                                                          
         MVI   ELCODE,CPOELQ                                                    
         BAS   RE,GETEL                                                         
PMEMO100 BNE   PMEMO900                                                         
         TM    VNDSTAT,VNDOCHCK    CHECK ORDER?                                 
         BO    PMEMO300                                                         
         CLC   CTABSTKN,CPOSTOCK   STOCK NUMBER MUST MATCH                      
         BNE   PMEMO155                                                         
         CP    CTABQTY,=P'0'       SKIP IF 0                                    
         BE    PMEMO150                                                         
         EDIT  CTABQTY,(7,PLQTY),FLOAT=-                                        
         EDIT  CTABURET,(11,PLUPRICE),2,ZERO=NOBLANK,FLOAT=-                    
         EDIT  CTABRAT,(5,PLRATIO),2,ZERO=NOBLANK,FLOAT=-                       
         ZAP   P16,CTABURET                                                     
         MP    P16,CTABQTY                                                      
         ZAP   BOPL61,P16                                                       
         EDIT  BOPL61,(11,PLSBTOT),2,ZERO=NOBLANK,FLOAT=-                       
         ZAP   P16,BOPL61                                                       
         MP    P16,CTABRAT                                                      
         SRP   P16,62,5            GET RID OF 2 DECIMALS                        
         ZAP   BOPL61,P16                                                       
         EDIT  BOPL61,(11,PLTOT),2,ZERO=NOBLANK,FLOAT=-                         
         MVC   PLSTCKN,CTABSTKN                                                 
         ZIC   RF,CPOLN                                                         
         SH    RF,=Y(CPOLN1Q+1)                                                 
         BM    PMEMO140                                                         
         CH    RF,=H'25'                                                        
         BNH   *+8                                                              
         LA    RF,25                                                            
         EX    RF,*+4                                                           
         MVC   PLDESCR(0),CPODESC                                               
PMEMO140 GOTO1 VREPORT,REPD                                                     
         AP    CSHTOT,BOPL61                                                    
PMEMO150 LA    R5,CTABLNQ(R5)                                                   
PMEMO155 BAS   RE,NEXTEL                                                        
         B     PMEMO100                                                         
*                                                                               
PMEMO300 LTR   R2,R2               FINISHED BEFORE?                             
         BZ    PMEMO310            YES, JUST SHOW AMOUNTS                       
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          ANY NARRATIVE?                               
         BZ    PMEMO310            NO, JUST SHOW AMOUNT                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PLCKDSC(0),8(R2)                                                 
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    RF,VNDNAR3H         DID WE SHOW ALL THE NARRATIVES?              
         CR    R2,RF               YES, MARK IT AS SUCH                         
         BNH   *+6                                                              
         SR    R2,R2                                                            
PMEMO310 ZAP   P16,CTABURET                                                     
         MP    P16,CTABQTY                                                      
         MP    P16,CTABRAT                                                      
         SRP   P16,62,5            GET RID OF 2 DEC PLACES                      
         ZAP   BOPL61,P16                                                       
         EDIT  BOPL61,(11,PLCKTOT),2,ZERO=NOBLANK,FLOAT=-                       
         GOTO1 VREPORT,REPD                                                     
         AP    CSHTOT,BOPL61                                                    
         B     PMEMO150                                                         
*                                                                               
PMEMO900 DS    0H                                                               
         TM    VNDSTAT,VNDOCHCK                                                 
         BZ    PMEMO910                                                         
         LA    RF,VNDNAR3H         LAST NARRATIVE                               
         LTR   R2,R2               DONE ALREADY                                 
         BZ    PMEMO909                                                         
         SR    R1,R1                                                            
PMEMO903 CR    R2,RF                                                            
         BH    PMEMO909                                                         
         ICM   R1,1,5(R2)          ANY NARRATIVE?                               
         BZ    PMEMO907                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PLCKDSC(0),8(R2)    SHOW DESCRIPTION                             
         GOTO1 VREPORT,REPD                                                     
PMEMO907 IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     PMEMO903                                                         
*                                                                               
PMEMO909 GOTO1 VREPORT,REPD                                                     
         LA    R2,PLCKTOT                                                       
         B     PMEMO920                                                         
*                                                                               
PMEMO910 GOTO1 VREPORT,REPD                                                     
         MVC   PLSTCKN(25),=CL25'Shipping and Handling'                         
         EDIT  TOTTAXS2,(11,PLTOT),2,ZERO=NOBLANK,FLOAT=-                       
         GOTO1 VREPORT,REPD                                                     
         AP    CSHTOT,TOTTAXS2                                                  
         LA    R2,PLTOT                                                         
PMEMO920 GOTO1 VREPORT,REPD                                                     
         MVC   PLSTCKN(5),=C'TOTAL'                                             
         EDIT  CSHTOT,(11,(R2)),2,ZERO=NOBLANK,FLOAT=-                          
         GOTO1 VREPORT,REPD                                                     
PMEMO999 GOTO1 VREPORT,REPD        SKIP TWO LINES                               
         GOTO1 VREPORT,REPD                                                     
         LA    R2,VNDCOM1H                                                      
         BAS   RE,FTLINE                                                        
         LA    R2,VNDCOM2H                                                      
         BAS   RE,FTLINE                                                        
         LA    R2,VNDCOM3H                                                      
         BAS   RE,FTLINE                                                        
         MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVC   FVXTRA+3(4),=C'CTA,'                                             
         EDIT  REPREPNO,(5,FVXTRA+7),0,ALIGN=LEFT                               
         B     PMEMOX                                                           
         EJECT                                                                  
*              LINKAGE TO SPOOL                                                 
*                                                                               
MYPRT    NTR1                                                                   
         MVC   DMCB+4(4),=X'D9000A0C'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R6)                                                   
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
PMEMOX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
HOOK     NTR1                                                                   
         L     R2,REPAHEAD                                                      
         MVC   75(L'DETLMMB,R2),DETLMMB                                         
         LA    R2,132(R2)                                                       
         GOTO1 DATCON,DMCB,(1,MEMDTE),(5,75(R2))                                
         LA    R2,132(R2)                                                       
         MVC   75(L'INVREF,R2),INVREF                                           
         LA    R2,132(R2)                                                       
         MVC   10(L'CNTRNAME,R2),CNTRNAME                                       
         MVC   75(L'INVORDNO,R2),INVORDNO                                       
         LA    R2,132(R2)                                                       
         MVC   10(L'CNTRADD1,R2),CNTRADD1                                       
         LA    R2,132(R2)                                                       
         MVC   10(L'CNTRADD2,R2),CNTRADD2                                       
         LA    R2,132(R2)                                                       
         MVC   10(L'CNTRADD3,R2),CNTRADD3                                       
         LA    R2,132(R2)                                                       
         MVC   10(L'CNTRADD4,R2),CNTRADD4                                       
         LA    R2,264(R2)                                                       
*                                                                               
         TM    VNDSTAT,VNDOCHCK              ORDER IS A CHECK                   
         BO    HK010                                                            
         MVC   10(L'VNDNAR1,R2),VNDNAR1                                         
         LA    R2,132(R2)                                                       
         MVC   10(L'VNDNAR1,R2),VNDNAR2                                         
         LA    R2,132(R2)                                                       
         MVC   10(L'VNDNAR1,R2),VNDNAR3                                         
*                                                                               
         L     R2,REPAMIDS                                                      
         MVC   1(10,R2),=CL10'QUANTITY'                                         
         MVC   14(10,R2),=CL10'STOCK #'                                         
         MVC   24(15,R2),=CL15'DESCRIPTION'                                     
         MVC   52(10,R2),=CL10'UNIT PRICE'                                      
         MVC   68(8,R2),=CL8'SUBTOTAL'                                          
         MVC   81(5,R2),=CL5'RATIO'                                             
         MVC   94(10,R2),=CL10'AMOUNT'                                          
         B     PMEMOX                                                           
*                                                                               
HK010    DS    0H                                                               
         L     R2,REPAMIDS                                                      
         MVC   0(15,R2),=CL15'DESCRIPTION'                                      
         MVC   70(10,R2),=CL10'AMOUNT'                                          
         B     PMEMOX                                                           
         EJECT                                                                  
FTLINE   NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1               ANY FOOT COMMENT?                            
         BZ    PMEMOX              NO LEAVE                                     
         MVC   DUB(5),8(R2)                                                     
         OC    DUB(5),SPACES                                                    
         CLC   =C'NARR=',DUB       NARRATIVE CODE?                              
         BE    FTLINE10                                                         
         L     RF,REPAPRNT                                                      
         MVC   0(L'VNDCOM1,RF),8(R2)                                            
         GOTO1 VREPORT,REPD                                                     
         B     PMEMOX                                                           
*                                                                               
FTLINE10 MVC   COMMCODE,SPACES     GET CODE AND RIGHT JUSTIFY                   
         SH    R1,=H'6'                                                         
         EX    R1,*+4                                                           
         MVC   COMMCODE(0),13(R2)                                               
         OC    COMMCODE,SPACES                                                  
         GOTO1 RIGHT,DMCB,COMMCODE,L'COMMCODE                                   
*                                                                               
         USING SCMRECD,R5                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         MVI   SCMKTYP,SCMKTYPQ    X'0C'                                        
         MVC   SCMKCPY,COMPANY                                                  
         MVC   SCMKCODE,COMMCODE                                                
         GOTO1 READ                                                             
*                                                                               
         USING SCMELD,R3                                                        
         L     R3,AIO1                                                          
         MVI   ELCODE,SCMELQ       X'3E'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FTLINE20 BAS   RE,NEXTEL                                                        
         BNE   PMEMOX                                                           
*                                                                               
         L     RF,REPAPRNT                                                      
         ZIC   R1,SCMLN                                                         
         LA    R0,SCMLN1Q+1                                                     
         SR    R1,R0                                                            
         BM    FTLINE20                                                         
         EX    R1,*+4                                                           
         MVC   0(0,RF),SCMNARR                                                  
         GOTO1 VREPORT,REPD                                                     
         B     FTLINE20                                                         
         EJECT                                                                  
T61B3CSP DS    0X                                                               
         SPEC  H1,60,C'Memo Bill No'                                            
         SPEC  H2,60,C'Invoice Date'                                            
         SPEC  H3,60,C'Invoice Ref'                                             
         SPEC  H4,5,C'To:'                                                      
         SPEC  H4,60,C'Order Number'                                            
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LOCAL STORAGE                                                          
*--------------------------------------------------------------                 
*                                                                               
PROGD    DSECT                                                                  
RELO1    DS    A                                                                
RIGHT    DS    V                                                                
*                                                                               
* SCREEN DIRECTORY                                                              
*                                                                               
AORDH    DS    A                   A(ORDER NUMBER HEADER)                       
AOHSTH   DS    A                   A(ORDER HISTORY HEADER)                      
ADOCH    DS    A                   A(DOCUMENT NUMBER HEADER)                    
ADATH    DS    A                   A(TRANSACTION DATE HEADER)                   
ACLIH    DS    A                   A(CLIENT HEADER)                             
ACLINH   DS    A                   A(CLIENT NAME HEADER                         
APROH    DS    A                   A(PRODUCT HEADER)                            
APRONH   DS    A                   A(PRODUCT NAME HEADER)                       
AJOBH    DS    A                   A(JOB HEADER)                                
AJOBNH   DS    A                   A(JOB NAME HEADER)                           
ASUPH    DS    A                   A(SUPPLIER HEADER)                           
ASUPNH   DS    A                   A(SUPPLIER NAME HEADER)                      
AURGH    DS    A                   A(URGENT HEADER)                             
ACDH     DS    A                   A(CASH DISCOUNT HEADER)                      
AWRKH    DS    A                   A(WORK CODE HEADER)                          
AWCNMH   DS    A                   A(WORK CODE NAMES HEADER)                    
AAMTH    DS    A                   A(AMOUNTS HEADER)                            
ANCWKH   DS    A                   A(NON-COMM WORKCODES HEADER)                 
AWNNMH   DS    A                   A(NON-COMM WORKCODE NAMES HEADER)            
ANCAH    DS    A                   A(AMOUNTS HEADER)                            
ACOFH    DS    A                   A(CREDIT OFFICE HEADER)                      
ACOFNH   DS    A                   A(CREDIT OFFICE NAME HEADER)                 
ATYPEH   DS    A                   A(GST TYPE HEADER)                           
ATYPNH   DS    A                   A(GST TYPE NAME FIELD HEADER)                
AGORNH   DS    A                   A(GROSS OR NET HEADER)                       
AGSTXH   DS    A                   A(GST EXTRA DATA HEADER)                     
AGAMTH   DS    A                   A(GST AMOUNTS)                               
APROVH   DS    A                   A(PST PROVINCE)                              
AAOFH    DS    A                   A(ANALYSIS OFFICE HEADER)                    
ADEPH    DS    A                   A(DEPARTMENT HEADER)                         
ASTFH    DS    A                   A(STAFF HEADER)                              
ASTFNH   DS    A                   A(STAFF NAME HEADER)                         
AEXPH    DS    A                   A(EXPENSE HEADER)                            
AEXPNH   DS    A                   A(EXPENSE NAME HEADER)                       
AFOFH    DS    A                   A(FINANCIAL OFFICE HEADER)                   
AFOFNH   DS    A                   A(FINANCIAL OFFICE NAME HEADER)              
ANARH    DS    A                   A(NARRATIVE HEADER)                          
ATOTH    DS    A                   A(TOTAL HEADER)                              
ATOT     DS    A                   A(TOTAL FIELD)                               
*                                                                               
SV3REGS  DS    0F                                                               
SVR2     DS    F                                                                
SVR3     DS    F                                                                
SVR4     DS    F                                                                
SVR6     DS    F                                                                
SVRE     DS    F                                                                
LINES    DS    C                                                                
ELCODE   DS    C                                                                
*                                                                               
CNTNUM   DS    CL15                CONTRACT NUMBER                              
CNTNAME  DS    CL36                CONTRACT NAME                                
*                                                                               
CTRNUM   DS    CL15                CONTRACTOR NUMBER                            
CTRNAME  DS    CL36                CONTRACTOR NAME                              
*                                                                               
VENDORAC DS    CL15                VENDOR ACCOUNT                               
*                                                                               
SAVEDATE DS    CL3                                                              
SECNDSW  DS    CL1                                                              
OFFICE   DS    CL2                                                              
COFFICE  DS    CL2                                                              
*                                                                               
WRKCODE  DS    CL2                 WORKCODE                                     
RETRATIO DS    PL4                 RETAIL RATIO                                 
*                                                                               
CSHTOT   DS    PL6                                                              
CDAMNT   DS    PL6                                                              
*                                                                               
VENDOR   DS    CL1                 VENDOR REQUIRED INDICATOR                    
VENDTYPE DS    CL1                 VENDOR'S GST TYPE                            
VENDPSTT DS    CL1                 VENDOR'S PST TYPE                            
VENDPROV DS    CL2                 VENDOR'S PROVINCE, VNDR REC (LFM)            
*                                                                               
TOTESTA  DS    PL6                                                              
TOTUPRC  DS    PL6                                                              
TOTURET  DS    PL6                                                              
TOTSTAX  DS    PL6                 TOTAL SALES TAX/SHIP                         
TOTTAXS  DS    PL6                                                              
TOTTAXS2 DS    PL6                 TWICE THE TAX                                
TOTCALC  DS    PL6                                                              
*                                                                               
*STSW    DS    CL1                 Y=GST APPLICABLE, N=NOT APPLICABLE           
GORN     DS    CL1                 G=GROSS, N=NET                               
NAMTS    DS    XL1                 N'AMOUNT ENTRIES                             
NGST     DS    XL1                 N'GST AMOUNTS INPUT                          
AMTTAB   DS    8XL(AMTLNQ)                                                      
ALARGEST DS    A                                                                
*        SPACE 1                                                                
SVWCNMS  DS    CL((4*(L'ACANDESC-4+2))-2)                                       
TSTWCNMS DS    CL((4*(L'ACANDESC-4+2))-2)                                       
MULTWCSW DS    X         USED FOR PREFIXING ', ' BEFORE MULT WC NAMES           
FSTWC    EQU   B'10000000'                                                      
SAVWLINE DS    X                                                                
SAVALINE DS    X                                                                
THISLINE DS    X                                                                
DOCLEN   DS    CL1                                                              
DOCSAVE  DS    CL6                                                              
REFSAVE  DS    CL6                                                              
SVSTAT   DS    CL1                                                              
SVSTAT2  DS    CL1                                                              
POSTEL   DS    CL(DLPSLNQ)         POSTING ELEMENT AREA                         
ORDNOEL  DS    XL10                                                             
MEMO4C   DS    XL(L'ACKEYACC-1+2)                                               
MEMO50   DS    XL(TRCSLNQ1)                                                     
TRSELEM  DS    XL(TRSLNQ)                                                       
LPRO     DS    XL1                                                              
DSKDA    DS    A                                                                
DWORK    DS    12D                                                              
CON#     DS    CL5                 CONTRACT NUMBER                              
PCON#    DS    PL3                 9'S COMPLEMENT OF CONTRACT NUMBER            
BIGKEY   DS    CL60                                                             
COMMCODE DS    CL6                 COMMENT CODE                                 
P16      DS    PL16                                                             
*                                                                               
EXCWORK  DS    0D                                                               
VNDSCRN  EQU   X'BD'               VENDOR SCREEN                                
DTLSCRN  EQU   X'BC'               DETAIL SCREEN                                
         DS    CL(EXCELNQ)                                                      
*                                                                               
       ++INCLUDE ACBATSTAX                                                      
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
TMPMODE  DS    CL1                 TEMPORARY MODE                               
PVBLK    DS    CL(L'PVALOUTB)      PERVAL BLOCK                                 
KEY      DS    CL49                                                             
IOAREA   DS    3000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
SPOOLD   DSECT                     ONLINE PRINT CONTROL DSECT                   
SPLAREA  DS    CL4000                                                           
         ORG   SPLAREA                                                          
         DS    D                                                                
LINE     DS    XL1                 PRESET TO 99                                 
ALLOWLIN DS    XL1                 ENSURE THAT N LINES REMAIN ON PAGE           
MAXLINES DS    XL1                 PRESET TO 60                                 
SPACING  DS    XL1                                                              
HEADHOOK DS    V                   USER SUPPLIED A(HEADLINE ROUTINE)            
MIDHOOK  DS    V                   USER SUPPLIED A(MIDLINE ROUTINE)             
CLEARHED DS    CL1                 OPTION TO CLEAR HEADLINES DEFAULT=Y          
FORCEHED DS    CL1                                                              
FORCEMID DS    CL1                                                              
FORCEFUT DS    CL1                                                              
FORCECLR DS    CL1                                                              
SKIPSPEC DS    CL1                                                              
PAGE     DS    XL2                                                              
SUBPAGE  DS    XL2                                                              
SPOOLIND DS    XL1                                                              
SPNSPACE EQU   X'80'               NO SPACE AFTER HEADLINES                     
SPNGAPS  EQU   SPNSPACE+X'20'      NO GAPS BETEEN HEADS AND MIDS                
SPUINIT  EQU   X'40'               ALLOW USER INITIALIZED FIELDS                
SPHHOOK  EQU   X'04'               APPLICATION CALLED WITH HEADHOOK             
SPMHOOK  EQU   X'02'               APPLICATION CALLED WITH MIDHOOK              
SPFHOOK  EQU   X'01'               APPLICATION CALLED WITH FOOTHOOK             
*                                                                               
         DS    XL1                                                              
         SPACE 1                                                                
SPOOLKEY DS    CL48                                                             
SPOOLPAG DS    H                                                                
SPOOLLIN DS    H                                                                
SPOOLDM  DS    A                                                                
SPOOLBUF DS    A                                                                
SPECS    DS    A                                                                
SPOOLID  DS    CL3                                                              
SPOOLRPN DS    CL2                                                              
SPMODE   DS    CL1                                                              
ACTPAGES DS    H                                                                
MAXPAGES DS    H                                                                
RCDATE   DS    CL8                                                              
RCPROG   DS    CL4                                                              
RCSUBPRG DS    CL1                                                              
SPCONSYS DS    CL1                                                              
RCDATCON DS    A                                                                
VPRINT   DS    A                                                                
BUFFALO  DS    V                                                                
SORTER   DS    V                                                                
WORKER   DS    V                                                                
ABOX     DS    V                                                                
FOOTLNS  DS    X                   NUMBER OF FOOTLINES REQUIRED                 
         ORG   FOOTLNS                                                          
FOOTHOOK DS    V                                                                
SPOOLQLK DS    A                   A(EXTENDED SPOOL KEY) (128 BYTES)            
*                                  FIRST BYTE OF SPOOLQLK MUST BE 0             
RCCOMFAC DS    V                                                                
SPOTPROF DS    CL16                                                             
         DS    CL16                                                             
         SPACE 1                                                                
         DS    D                   HEADLINES                                    
HEAD1    DS    CL132                                                            
HEAD2    DS    CL132                                                            
HEAD3    DS    CL132                                                            
HEAD4    DS    CL132                                                            
HEAD5    DS    CL132                                                            
HEAD6    DS    CL132                                                            
HEAD7    DS    CL132                                                            
HEAD8    DS    CL132                                                            
HEAD9    DS    CL132                                                            
HEAD10   DS    CL132                                                            
HEAD11   DS    CL132                                                            
HEAD12   DS    CL132                                                            
HEAD13   DS    CL132                                                            
HEAD14   DS    CL132                                                            
H1       EQU   HEAD1                                                            
H2       EQU   HEAD2                                                            
H3       EQU   HEAD3                                                            
H4       EQU   HEAD4                                                            
H5       EQU   HEAD5                                                            
H6       EQU   HEAD6                                                            
H7       EQU   HEAD7                                                            
H8       EQU   HEAD8                                                            
H9       EQU   HEAD9                                                            
H10      EQU   HEAD10                                                           
H11      EQU   HEAD11                                                           
H12      EQU   HEAD12                                                           
H13      EQU   HEAD13                                                           
H14      EQU   HEAD14                                                           
         SPACE 1                                                                
         DS    CL8                 MID LINES                                    
MID1     DS    CL132                                                            
MID2     DS    CL132                                                            
         DS    CL8                 PRINT LINES                                  
P        DS    0CL132                                                           
P1       DS    CL132                                                            
P2       DS    CL132                                                            
P3       DS    CL132                                                            
P4       DS    CL132                                                            
         DS    CL132               SPACES FIELD                                 
MONTHS   DS    CL36                MONTH TABLE (JAN-DEC)                        
DAYTABL  DS    CL21                DAY TABLE (MON-SUN)                          
USERNAME DS    CL33                                                             
USERADDR DS    CL33                                                             
USERQSTR DS    CL6                                                              
USERQEND DS    CL6                                                              
USERPROF DS    CL16                                                             
USERLANG DS    XL1                                                              
         DS    CL1                                                              
SPOOLEND DS    D                                                                
         EJECT                                                                  
*--------------------------------------------------------------                 
*        CURRENT DETAIL LINE DSECT                                              
*--------------------------------------------------------------                 
*                                                                               
CURLINED DSECT                                                                  
CURCTRH  DS    CL8                 CONTRACT                                     
CURCTR   DS    CL(L'DETCTR)                                                     
         DS    CL8                                                              
CURQTYH  DS    CL8                 QUANTITY                                     
CURQTY   DS    CL(L'DETQTY)                                                     
         DS    CL8                                                              
CURSTKNH DS    CL8                 STOCK NUMBER                                 
CURSTKN  DS    CL(L'DETSTKN)                                                    
         DS    CL8                                                              
CURUPRCH DS    CL8                 UNIT PRICE                                   
CURUPRC  DS    CL(L'DETUPRC)                                                    
         DS    CL8                                                              
CURURETH DS    CL8                 USER RETAIL                                  
CURURET  DS    CL(L'DETURET)                                                    
         DS    CL8                                                              
CURTAXH  DS    CL8                 TAX AND SHIPPING                             
CURTAX   DS    CL(L'DETTAX)                                                     
         DS    CL8                                                              
CURRATH  DS    CL8                 RATIO                                        
CURRAT   DS    CL(L'DETRAT)                                                     
         DS    CL8                                                              
CURCALH  DS    CL8                 CALCULATED AMOUNT                            
CURCAL   DS    CL(L'DETCAL)                                                     
         DS    CL8                                                              
CURLLNQ  EQU   *-CURLINED                                                       
         EJECT                                                                  
CTABD    DSECT                     CONTRACT TABLE DSECT                         
CTABCNTR DS    CL10                CONTRACT                                     
CTABQTY  DS    PL3                 QUANTITY                                     
CTABSTKN DS    CL8                 STOCK NUMBER                                 
CTABUPRC DS    PL6                 UNIT PRICE                                   
CTABURET DS    PL6                 UNIT RETAIL                                  
CTABTAXS DS    PL4                 TAX/SHIP                                     
CTABRAT  DS    PL4                 RATIO                                        
CTABSTAT DS    XL1                 STATUS                                       
CTABSCHA EQU   X'80'               CHANGED                                      
CTABLNQ  EQU   *-CTABD                                                          
         SPACE 3                                                                
PLINED   DSECT                                                                  
PLQTY    DS    CL7                 FOR MERCHANDISE                              
         DS    CL7                                                              
PLSTCKN  DS    CL8                                                              
         DS    CL2                                                              
PLDESCR  DS    CL25                                                             
         DS    CL2                                                              
PLUPRICE DS    CL11                                                             
         DS    CL3                                                              
PLSBTOT  DS    CL11                                                             
         DS    CL5                                                              
PLRATIO  DS    CL5                                                              
         DS    CL3                                                              
PLTOT    DS    CL11                                                             
*                                                                               
         ORG   PLQTY                                                            
PLCKDSC  DS    CL60                FOR NON-MERCHANDISE                          
         DS    CL5                                                              
PLCKTOT  DS    CL11                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        KEY DSECT                                                              
*--------------------------------------------------------------                 
*                                                                               
KEYD     DSECT                                                                  
KVENNUM  DS    CL15                                                             
         DS    CL17                                                             
KDATE    DS    XL3                                                              
KDOC     DS    CL6                                                              
*                                                                               
KEYSAVED DSECT                                                                  
KSVENNUM DS    CL15                                                             
         DS    CL17                                                             
KSDATE   DS    XL3                                                              
KSDOC    DS    CL6                                                              
         SPACE 2                                                                
VALD     DSECT                                                                  
VALWRKH  DS    CL8                                                              
VALWRK   DS    CL11                                                             
         DS    CL8                                                              
VALWNMH  DS    CL8                                                              
VALWNM   DS    CL49                                                             
         DS    CL8                                                              
         DS    CL17                                                             
VALAMTH  DS    CL8                                                              
VALAMT   DS    CL40                                                             
         SPACE 2                                                                
* DSECT TO COVER WORKCODE AMOUNT TABLE                                          
*                                                                               
AMTD     DSECT                                                                  
AMTWC    DS    CL2                 WORKCODE                                     
AMTNET   DS    PL6                 NET                                          
AMTGRS   DS    PL6                 GROSS                                        
AMTGST   DS    PL6                 GST                                          
AMTLNQ   EQU   *-AMTD                                                           
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATBDD - SCREEN DSECT                                                
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATBDD                                                       
         EJECT                                                                  
         ORG   OSVALS                                                           
*                                                                               
USERAREA DS    0F                                                               
         DS    CL64                                                             
CTRAMT   DS    PL6                                                              
TAXAMT   DS    PL6                                                              
TOTCTR   DS    PL6                                                              
*                                                                               
CNTRACC  DS    CL15                CONTRACTOR                                   
CNTRNAME DS    CL36                CONTRACTOR NAME                              
CNTRADD1 DS    CL(L'ADRADD1)       CONTRACTOR ADDRESS LINE 1                    
CNTRADD2 DS    CL(L'ADRADD2)       CONTRACTOR ADDRESS LINE 2                    
CNTRADD3 DS    CL(L'ADRADD3)       CONTRACTOR ADDRESS LINE 3                    
CNTRADD4 DS    CL(L'ADRADD4)       CONTRACTOR ADDRESS LINE 4                    
EXPNACC  DS    CL15                EXPENSE ACCOUNT FROM MAIN SCREEN             
EXPNUM   DS    CL15                EXPENSE ACCOUNT                              
EXPNAME  DS    CL36                EXPENSE ACCOUNT NAME                         
VNDNUM   DS    CL15                VENDOR ACCOUNT                               
VNDNAME  DS    CL36                VENDOR NAME                                  
VNDAMT   DS    PL7                 VENDOR AMOUNT                                
INVREFLN DS    XL1                 LENGTH OF INVOICE REFERENCE                  
INVREF   DS    CL21                INVOICE REFERENCE                            
INVDTE   DS    PL3                 INVOICE DATE                                 
INVDDTE  DS    PL3                 INVOICE DUE DATE                             
MEMDTE   DS    PL3                 MEMO BILL DATE                               
INVORDNO DS    CL6                 ORDER NUMBER                                 
DETLMMB  DS    CL6                 DETAIL MEMO BILL                             
VNDTOT   DS    PL7                 VENDOR SCREEN TOTAL                          
DETTOT   DS    PL7                 DETAIL SCREEN TOTAL                          
TOTDIFF  DS    PL7                 THE DIFFERENCE                               
NUMCONTR DS    XL1                 NUMBER OF CONTRACTS                          
STCONTR  DS    XL1                 START CONTRACT                               
GROUPNO  DS    XL4                 GROUP INVOICE NUMBER                         
LINENO   DS    XL1                 LINE NUMBER                                  
SVOFFICE DS    CL2                 SAVED OFFICE                                 
*                                                                               
VNDSTAT  DS    CL1                 VENDOR STATUS                                
VNDORDCH EQU   X'80'               VENDOR ORDER CHANGED                         
VNDOCHCK EQU   X'40'               VENDOR ORDER IS A CHECK                      
VNDEXPNS EQU   X'20'               EXPENSE VENDOR USED                          
VNDOCSBP EQU   X'10'               ORDER IS CASH/BILLPAY                        
OLDNUMCT DS    XL1                 OLD NUMBER OF CONTRACTS                      
LASTCNTR DS    CL10                LAST CONTRACT                                
SPAGYMED DS    XL1                 SPOT AGENCY/MEDIA                            
*                                                                               
PRGOSTAT DS    CL1                 ORDER STATUS                                 
PRGOPART EQU   X'80'               PARTIAL ORDER                                
PRGPTAX  EQU   X'40'               POSTED SUBSID TAX ELEMENT                    
UTAXLOCL DS    CL8                 USE TAX LOCALITY                             
UTAXRT   DS    PL4                 USE TAX RATE                                 
UTAX     DS    PL6                 USE TAX                                      
UTAXCRAC DS    CL15                USE TAX CREDIT ACCOUNT                       
UTAXCANM DS    CL36                USE TAX CREDIT ACCOUNT NAME                  
TOTUTAX  DS    PL6                 TOTAL USE TAX                                
CDSW     DS    CL1                 CASH DISCOUNT SWITCH                         
CDPRCNT  DS    PL3                 CASH DISCOUNT PERCENT                        
CASHDISC DS    PL6                 CASH DISCOUNT                                
CDACCT   DS    CL15                CASH DISCOUNT ACCOUNT                        
CDACCTNM DS    CL36                CASH DISCOUNT ACCOUNT NAME                   
ORDRCTGY DS    CL2                 ORDER CATEGORY                               
*                                                                               
SCRNMAX  EQU   6                   6 CONTRACT LINES ON SCREEN                   
MAXNCNTR EQU   8                   MAX NUMBER OF CONTRACTS                      
CNTRTABS EQU   MAXNCNTR*CTABLNQ    CONTRACT TABLE SIZE                          
QTYTABL  DS    (MAXNCNTR)PL3       QUANTITY TABLE                               
CNTRTABL DS    (CNTRTABS)C         CONTRACT TABLE                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATBCD - DETAIL SCREEN DSECT                                         
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATBCD                                                       
         EJECT                                                                  
*ACEXCELD                                                                       
       ++INCLUDE ACEXCELD                                                       
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069ACBAT3C   05/07/99'                                      
         END                                                                    

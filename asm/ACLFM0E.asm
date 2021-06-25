*          DATA SET ACLFM0E    AT LEVEL 058 AS OF 07/07/03                      
*PHASE T6030EA,+0                                                               
         TITLE 'MODULE TO HANDLE COMPANY ELEMENT'                               
T6030E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**LFME**,R9,CLEAR=YES                                  
         LR    R8,RC                                                            
         USING LWSD,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VGETTXT,CGETTXT                                                  
         DROP  RF                                                               
*                                                                               
         LA    R2,LOGCOMPH                                                      
         GOTO1 ANY                                                              
*                                                                               
         CLI   CSSCRN,0            FIRST TIME THROUGH                           
         BNE   PFVAL0                                                           
         MVI   CSSCRN,PRI          SET PRIMARY SCREEN                           
         B     BLDK                                                             
*                                                                               
PFVAL0   CLI   CSSCRN,PRI          IF LAST WAS A SECONDARY SCREEN               
         BE    PFVAL1                                                           
         CLI   PFKEY,0                                                          
         BE    BLDK                                                             
         CLI   PFKEY,PFRTN         ONLY VALID PF KEY IS RETURN                  
         BE    *+12                                                             
         LA    RF,AE$PF12V         'ONLY PF12 IS VALID'                         
         B     INVERR                                                           
         MVI   CSSCRN,PRI          RESTORE PRIMARY SCREEN                       
         BAS   RE,RSCRN                                                         
         LA    RF,AI$RECDS         'RECORD DISPLAYED'                           
         XC    WORK,WORK                                                        
         GOTO1 VGETTXT,WORK,(RF),(0,LOGHEADH),(C'I',0)                          
         MVI   ERROR,X'FE'                                                      
         L     R2,RTNCURSE         SET CURSOR TO SAME FIELD                     
         AR    R2,RA                                                            
         B     XIT                                                              
*                                                                               
PFVAL1   CLI   PFKEY,0                                                          
         BE    BLDK                                                             
*                                                                               
         L     RF,ACURSOR                                                       
         SR    RF,RA                                                            
         ST    RF,RTNCURSE         SAVE CURRENT CURSOR ADDRESS                  
*                                                                               
         LA    R1,PFTAB            FIND ENTRY IN PF KEY TABLE                   
PFVAL2   CLC   0(1,R1),CSSCRN      MATCH ON CURRENT SCREEN                      
         BNE   PFVAL3                                                           
         CLC   1(1,R1),PFKEY       AND PF KEY                                   
         BE    PFVAL4                                                           
PFVAL3   LA    R1,PFTABQ(R1)                                                    
         CLI   0(R1),X'FF'                                                      
         BNE   PFVAL2                                                           
         LA    RF,AE$PFINV         'PF KEY INVALID FOR THIS ACTION'             
         B     INVERR                                                           
*                                                                               
PFVAL4   MVC   SVPFKEY,PFKEY       SAVE PFKEY LAST ENTERED                      
         BAS   RE,SSCRN            SAVE THE CURRENT SCREEN                      
         MVC   CSSCRN,2(R1)        NEW SCREEN                                   
         BAS   RE,GSCRN            GET NEW SCREEN                               
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
         MVI   PFKEY,0             CLEAR KEY FOR NEXT TIME                      
         SR    RF,RF               RF TO A(NEW ROUTINE)                         
         ICM   RF,7,3(R1)                                                       
         ST    RF,DSPROUT          SAVE DISPLAY ROUTINE                         
         ICM   RF,7,6(R1)                                                       
         ST    RF,CHGROUT          SAVE CHANGE ROUTINE                          
         B     BLDK                                                             
         EJECT                                                                  
***********************************************************************         
*              BUILD KEY                                              *         
***********************************************************************         
*                                                                               
BLDK     MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   DISR                                                             
         MVC   KEY,SPACES                                                       
         LA    R2,LOGCOMPH                                                      
         GOTO1 ANY                                                              
         MVC   WORK(1),LOGCOMP                                                  
         CLI   5(R2),1                                                          
         BE    CO1                                                              
         GOTO1 VHEXIN,DMCB,LOGCOMP,WORK,2                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   CO1                                                              
         MVI   ERROR,2                                                          
         B     XIT                                                              
*                                                                               
CO1      MVC   KEY(1),WORK                                                      
         CLI   LOGACT,C'N'             NEW COMPANY                              
         BNE   CO5                                                              
*        BAS   RE,SYSWITCH                                                      
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                     COMPANY IS IN ANOTHER SYS.               
*                                                                               
CO5      TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LOCATE AND DISPLAY ELEMENT                             *         
***********************************************************************         
                                                                                
DISR     CLI   MODE,DSPLYREC                                                    
         BNE   CHGR                                                             
         CLI   CSSCRN,PRI                                                       
         BE    *+12                                                             
         L     RF,DSPROUT                                                       
         AR    RF,RB                                                            
         BR    RF                                                               
*                                                                               
         XC    LOGALPH,LOGALPH                                                  
         OI    LOGALPHH+6,X'80'                                                 
         TWAXC LOGADD1H                                                         
         LA    R2,LOGNAMPH                                                      
         GOTO1 NAMOUT                                                           
         LA    R2,LOGADD1H                                                      
         GOTO1 ADDROUT                                                          
*                                                                               
         LA    R4,IO2                                                           
         MVI   ELCODE,CPYELQ       CPY ELEMENT MUST EXIST                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,LOGABBRH                                                      
         USING CPYELD,R4                                                        
         ST    R4,SAVE4                                                         
         MVC   LOGABBR,CPYLOGO                                                  
         MVC   LOGBANK,CPYBANK                                                  
         MVC   LOGPETY,CPYPETY                                                  
         MVC   LOGPROD,CPYPROD                                                  
         MVC   LOGRECV,CPYRECV                                                  
         MVC   LOGSUPP,CPYSUPP                                                  
         MVC   LOGSUPX,CPYSUPX                                                  
         MVC   LOGWKCD,CPYWRKSI                                                 
         MVC   LOGALPH,CPYALPHA                                                 
*                                                                               
         CLI   CPYLN,CPYLN2Q       TEST FOR LARGE ELEMENT                       
         BL    DISR3                                                            
         MVC   LOGTAX,CPYTAX       YES-DISPLAY TAX                              
         MVC   LOGXSUP,CPYXSUPP    EXTRA SUPPLIER LEDGER CODE                   
         TM    CPYSTAT5,CPYSNCST   IF ON NEW COSTING                            
         BNO   *+8                                                              
         NI    CPYSTAT1,X'FF'-CPYSCOST DON'T DISPLAY COST FIELD                 
*                                                                               
DISR3    XC    SAVECOMP,SAVECOMP   SAVE CPY ELEM IN SAVE AREA                   
         SR    R1,R1                                                            
         IC    R1,CPYLN                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   SAVECOMP(0),CPYEL                                                
         MVC   NEWCOMP,SAVECOMP                                                 
*                                                                               
         MVC   LOGREP,SPACES       COMMISSION ONLY REP                          
         OI    LOGREPH+6,X'80'                                                  
         OC    CPYREPC,CPYREPC                                                  
         BZ    DISR5               NO REP-CODE                                  
         SR    R5,R5                                                            
         ICM   R5,3,CPYREPC                                                     
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB+5(3)                                                 
         MVC   LOGREP,WORK+1                                                    
*                                                                               
DISR5    XR    R5,R5                                                            
         LA    R3,LWBLK                                                         
         GOTO1 OUTBLK,COMSTAT      BUILD OUTPUT BLOCK FOR UNSCAN                
         CLI   CPYLN,CPYLN1Q       TEST FOR SMALLER ELEMENT LENGTH              
         BL    DISR23                                                           
         MVC   LOGBSL,CPYBSEC      BATCH SECURITY                               
         OI    LOGBSL,X'F0'                                                     
         GOTO1 OUTBLK,COMSTA2      CONTINUE OUTPUT BLOCK                        
         GOTO1 OUTBLK,COMSTA3                                                   
         GOTO1 OUTBLK,COMSTA4                                                   
         GOTO1 OUTBLK,COMSTA5                                                   
         GOTO1 OUTBLK,COMSTA6                                                   
         GOTO1 OUTBLK,COMSTA7                                                   
         GOTO1 OUTBLK,COMSTA8                                                   
         GOTO1 OUTBLK,COMSTA9                                                   
         GOTO1 OUTBLK,COMSTAA                                                   
         GOTO1 OUTBLK,COMCD                                                     
*                                                                               
         OC    CPYUID,CPYUID                                                    
         BZ    DISR7                                                            
         LA    R5,1(R5)            ADD 1 TO OUTPUT COUNT                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(2,R3),=C'ID'                                                   
         EDIT  (2,CPYUID),(5,10(R3)),ALIGN=LEFT                                 
*                                                                               
DISR7    CLI   CPYLN,CPYLN3Q                                                    
         BL    DISR8                                                            
         CLI   CPYTCMP,0                                                        
         BE    DISR8                                                            
         LA    R5,1(R5)            ADD 1 TO OUTPUT COUNT                        
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(3,R3),=C'TID'                                                  
         GOTO1 VHEXOUT,DMCB,CPYTCMP,WORK,1,0,0                                  
         MVC   10(2,R3),WORK                                                    
*                                                                               
DISR8    TM    CPYSTAT7,CPYSTMSY   COMPANY ON TMS?                              
         BNO   DISR9                                                            
         LA    R5,1(R5)            ADD 1 TO OUTPUT COUNT                        
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(3,R3),=C'TMS'                                                  
         CLI   CPYLN,CPYLN3Q       LONGER EL HAS TMS START DATE                 
         BL    DISR9                                                            
         OC    CPYTMSSD,CPYTMSSD   TMS START DATE?                              
         BZ    DISR9                                                            
         GOTO1 DATCON,DMCB,(2,CPYTMSSD),(17,10(R3))                             
*                                                                               
DISR9    CLI   CPYLN,CPYLN2Q       TEST FOR SMALLER ELEMENT LENGTH              
         BL    DISR15                                                           
         OC    CPYTSD,CPYTSD       DISPLAY TIME SHEET DAY                       
         BZ    DISR13                                                           
         LA    R5,1(R5)            INCREMENT OUTPUT COUNT                       
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(3,R3),=C'TSD'                                                  
*                                                                               
         LA    RF,DAYS                                                          
         LA    R0,DAYSLNQ          # TIMES TO LOOP                              
         USING DAYSD,RF                                                         
*                                                                               
DISR11   CLC   DAYNUM,CPYTSD                                                    
         BNE   *+14                                                             
         MVC   10(L'DAYNAME,R3),DAYNAME                                         
         B     DISR15                                                           
         LA    RF,DAYQ(RF)                                                      
         BCT   R0,DISR11                                                        
         B     DISR15                                                           
         DROP  RF                                                               
*                                                                               
DISR13   OC    CPYCTFIL,CPYCTFIL   ANY AGENCY ID?                               
         BZ    DISR15              NO                                           
         LA    R5,1(R5)            YES, INCREMENT OUTPUT COUNT                  
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(6,R3),=C'CTFILE'                                               
         MVC   10(2,R3),CPYCTFIL                                                
*                                                                               
DISR15   CLI   CPYTENO,C'0'                                                     
         BL    DISR17                                                           
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(2,R3),=C'TE'                                                   
         MVC   10(1,R3),CPYTENO                                                 
         LA    R5,1(R5)                                                         
*                                                                               
DISR17   CLI   CPYDEPTL,0                                                       
         BE    DISR19                                                           
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(4,R3),=C'DPTL'                                                 
         MVC   10(1,R3),CPYDEPTL                                                
         OI    10(R3),X'F0'                                                     
         LA    R5,1(R5)                                                         
*                                                                               
DISR19   CLI   CPYLN,CPYLN2Q                                                    
         BL    DISR21                                                           
         CLI   CPYOFFC,X'40'                                                    
         BNH   DISR21                                                           
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(5,R3),=C'GLOFF'                                                
         MVC   10(2,R3),CPYOFFC                                                 
         LA    R5,1(R5)                                                         
*                                                                               
DISR21   LTR   R5,R5                                                            
         BZ    DISR23                                                           
         GOTO1 VUNSCAN,DMCB,((R5),LWBLK),LOGSTATH,0                             
         CLI   DMCB,0                                                           
         BE    DISR23                                                           
         GOTO1 (RF),(R1),,LOGSTA2H                                              
         CLI   DMCB,0                                                           
         BE    DISR23                                                           
         GOTO1 (RF),(R1),,LOGSTA3H                                              
         CLI   DMCB,0                                                           
         BE    DISR23                                                           
         GOTO1 (RF),(R1),,LOGSTA4H                                              
*                                                                               
DISR23   L     R4,SAVE4                                                         
         OC    CPYSFST,CPYSFST                                                  
         BZ    DISR29                                                           
         LA    R3,MONTHS                                                        
         LA    RE,MONLST                                                        
         LA    RF,12                                                            
*                                                                               
DISR25   CLC   CPYSFST,0(RE)                                                    
         BE    DISR27                                                           
         LA    RE,1(RE)                                                         
         LA    R3,3(R3)                                                         
         BCT   RF,DISR25                                                        
         B     DISR29                                                           
*                                                                               
DISR27   MVC   LOGSTM,0(R3)                                                     
*                                                                               
DISR29   LA    R2,LOGNAMEH                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REBUILD AN ELEMENT                                     *         
***********************************************************************         
*                                                                               
CHGR     CLI   CSSCRN,PRI                                                       
         BE    *+12                                                             
         L     RF,CHGROUT                                                       
         AR    RF,RB                                                            
         BR    RF                                                               
*                                                                               
         LA    R2,LOGNAMEH                                                      
         LA    R2,LOGNAMEH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMIN                                                            
         LA    R2,LOGADD1H                                                      
         GOTO1 ADDRIN                                                           
         LA    R4,ELEMENT                                                       
         XC    CPYELD(CPYLN3Q),CPYELD                                           
         MVI   CPYEL,CPYELQ                                                     
         MVI   CPYLN,CPYLN3Q                                                    
         MVI   CPYGLU,C'G'                                                      
         MVI   CPYBSL,C'B'                                                      
         MVI   CPYPANL,C'P'                                                     
         MVI   CPYANAL,C'2'                                                     
*                                                                               
         LA    RF,SAVECOMP                                                      
         MVC   CPYMOSX,CPYMOSX-CPYELD(RF)    RESTORE MONTH OF SERVICE           
         MVC   CPYSBILL,CPYSBILL-CPYELD(RF)   AND CREATIVE BILL NUMBER          
         LA    R2,LOGBANKH                                                      
         CLI   5(R2),2                                                          
         BNE   CMISS                                                            
         MVC   CPYBANK,8(R2)                                                    
         LA    R2,LOGPETYH                                                      
         CLI   5(R2),2                                                          
         BNE   CMISS                                                            
         MVC   CPYPETY,8(R2)                                                    
         LA    R2,LOGPRODH                                                      
         CLI   5(R2),2                                                          
         BNE   CMISS                                                            
         MVC   CPYPROD,8(R2)                                                    
         LA    R2,LOGRECVH                                                      
         CLI   5(R2),2                                                          
         BNE   CMISS                                                            
         MVC   CPYRECV,8(R2)                                                    
         LA    R2,LOGSUPPH                                                      
         CLI   5(R2),2                                                          
         BNE   CMISS                                                            
         MVC   CPYSUPP,8(R2)                                                    
         LA    R2,LOGSUPXH                                                      
         GOTO1 ANY                                                              
         MVC   CPYSUPX,8(R2)                                                    
         LA    R2,LOGTAXH                                                       
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   CPYTAX,8(R2)                                                     
*                                                                               
         LA    R2,LOGXSUPH                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   CPYXSUPP,8(R2)                                                   
*                                                                               
         LA    R2,LOGWKCDH                                                      
         MVI   CPYWRKSI,C' '                                                    
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   CPYWRKSI,8(R2)                                                   
         LA    R2,LOGSTMH                                                       
         CLI   5(R2),0                                                          
         BE    CHGR9                                                            
         LA    RE,MONLST                                                        
         LA    R3,MONTHS                                                        
         LA    RF,12                                                            
*                                                                               
CHGR5    CLC   LOGSTM,0(R3)                                                     
         BE    CHGR7                                                            
         LA    RE,1(RE)                                                         
         LA    R3,3(R3)                                                         
         BCT   RF,CHGR5                                                         
         B     CMINV                                                            
*                                                                               
CHGR7    MVC   CPYSFST,0(RE)                                                    
*                                                                               
CHGR9    LA    R2,LOGBSLH          BATCH SECURITY                               
         CLI   5(R2),0                                                          
         BE    CHGR11                                                           
         CLI   LOGBSL,C'0'                                                      
         BL    CMINV                                                            
         CLI   LOGBSL,C'3'                                                      
         BH    CMINV                                                            
         MVC   CPYBSEC,LOGBSL                                                   
         NI    CPYBSEC,X'0F'                                                    
*                                                                               
CHGR11   DS    0H                                                               
         LA    R2,LOGREPH          COMMISSION ONLY REP CODE                     
         CLI   5(R2),0                                                          
         BE    CHGR13                                                           
         CLI   5(R2),4             MUST BE 4                                    
         BNE   CMINV                                                            
         CLI   8(R2),C'0'          AND START WITH ZERO                          
         BNE   CMINV                                                            
         GOTO1 PACK                                                             
         LTR   R1,R1                                                            
         BZ    CMINV               CAN'T BE ALL ZEROS                           
         STCM  R1,3,CPYREPC                                                     
*                                                                               
CHGR13   MVI   ERROR,0                                                          
         LA    R2,LOGSTATH         STATUS LINE                                  
         BAS   RE,CMTCH            MATCH TABLE                                  
         CLI   ERROR,0                                                          
         BNE   CMINV               NO, ERROR                                    
         LA    R2,LOGSTA2H         SECOND STATUS LINE                           
         BAS   RE,CMTCH                                                         
         CLI   ERROR,0                                                          
         BNE   CMINV               ERROR INVALID INPUT                          
         LA    R2,LOGSTA3H         THIRD STATUS LINE                            
         BAS   RE,CMTCH                                                         
         CLI   ERROR,0                                                          
         BNE   CMINV               ERROR INVALID INPUT                          
         LA    R2,LOGSTA4H         FOURTH STATUS LINE                           
         BAS   RE,CMTCH                                                         
         CLI   ERROR,0                                                          
         BNE   CMINV               ERROR INVALID INPUT                          
*                                                                               
         OC    CPYUID,CPYUID       IS ID PRESENT                                
         BNZ   CHGR15                                                           
         MVI   ERROR,31            ID MISSING                                   
         B     XIT                                                              
*                                                                               
CHGR15   LA    R5,IO                                                            
         USING CTIKEY,R5                                                        
         XC    CTIKEY,CTIKEY             CLEAR KEY FOR ID REC LOOKUP            
         MVI   CTIKTYP,C'I'              RECORD TYPE I FOR ID REC               
         MVC   CTIKID+8(2),CPYUID        ID NUMBER                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IO,IO,0                       
         CLI   8(R1),0                                                          
         BE    CHGR17                                                           
         LA    R2,LOGCOMPH                   SET CURSOR TO COMPANY              
         MVC   LOGHEAD(28),=C'** CO NOT ON CONTROL FILE **'                     
         MVI   ERROR,X'FE'                   I SET THE ERROR MESSAGE            
         B     XIT                                                              
*                                                                               
CHGR17   LA    R3,CTIDATA       R3=A(FIRST ELEMENT)                             
*                                                                               
CHGR19   CLI   0(R3),0                    END OF RECORD                         
         BE    CHGR31                                                           
         CLI   0(R3),CTDSCELQ             DESCRIP EL FOR ABBR                   
         BE    CHGR23                                                           
         CLI   0(R3),CTAGYELQ             AGENCY ALPHA EL                       
         BE    CHGR25                                                           
         CLI   0(R3),CTSYSELQ             AUTH EL TO CHECK ID NUMBER            
         BE    CHGR27                                                           
CHGR21   ZIC   R1,1(R3)                   LENGTH IF EL INTO R1                  
         AR    R3,R1                      POINT R3 TO NEXT EL                   
         B     CHGR19                                                           
*                                                                               
         USING CTDSCD,R3                                                        
CHGR23   MVC   CPYLOGO,CTDSC              AGENCY SIGN ON INTO CO EL             
         OI    LOGABBRH+6,X'80'           TRANSMIT                              
         MVC   LOGABBR,CPYLOGO            INTO SREEN FIELD                      
         B     CHGR21                                                           
*                                                                               
         USING CTAGYD,R3                                                        
CHGR25   MVC   CPYALPHA,CTAGYID           AGENCY ALPHA                          
         OI    LOGALPHH+6,X'80'           TRANSMIT                              
         MVC   LOGALPH,CPYALPHA           INTO SREEN FIELD                      
         B     CHGR21                                                           
*                                                                               
         USING CTSYSD,R3                                                        
CHGR27   CLI   CTSYSNUM,X'06'             IS IT ACCOUNTING                      
         BNE   CHGR21                     NO - GO LOOK FOR ANOTHER 21           
         LA    R2,LOGCOMPH                                                      
         MVC   WORK(1),LOGCOMP                                                  
         CLI   5(R2),1                    IS LENGTH ALREADY 1 BYTE              
         BE    CHGR29                                                           
         GOTO1 VHEXIN,DMCB,LOGCOMP,WORK,2                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   CHGR29                                                           
         DC    H'0'                       INVALID SOURCE FOR HEXIN              
*                                                                               
CHGR29   CLC   CTSYSAGB,WORK              SAME HEXCOMP AS SCREEN                
         BE    CHGR21                     YES - ITS OK                          
         LA    R2,LOGCOMPH                   SET CURSOR TO COMPANY              
         MVC   LOGHEAD(29),=C'** HEXCOMP NOT ON CON FILE **'                    
         MVI   ERROR,X'FE'                   I SET THE ERROR MESSAGE            
         B     XIT                                                              
*                                                                               
CHGR31   LA    RF,SAVECOMP                                                      
         XC    CPYVATR-CPYELD(CPYVATL,RF),CPYVATR-CPYELD(RF)                    
         MVI   CPYLN-CPYELD(RF),CPYLN3Q                                         
         TM    CPYSTAT3,CPYSOPBM   IS BA=Y ON IN NEW ELEMENT                    
         BZ    *+12                                                             
         OI    SAVECOMP+(CPYSTAT3-CPYELD),CPYSOPBM                              
         B     *+8                                                              
         NI    SAVECOMP+(CPYSTAT3-CPYELD),X'FF'-CPYSOPBM                        
*                                                                               
         TM    CPYSTAT4,CPYSOV12   IS BBD=Y(BACKDATE BATCH)                     
         BZ    *+12                                                             
         OI    SAVECOMP+(CPYSTAT4-CPYELD),CPYSOV12                              
         B     *+8                                                              
         NI    SAVECOMP+(CPYSTAT4-CPYELD),X'FF'-CPYSOV12                        
*                                                                               
CHGR39   TM    CPYSTAT5,CPYSNCST   IF ON NEW COSTING SYSTEM                     
         BNO   *+8                                                              
         OI    CPYSTAT1,CPYSCOST   MUST ALSO BE ON COST                         
         TM    CPYSTAT4,CPYSOFF2   IF ON NEW OFFICES                            
         BNO   *+8                                                              
         OI    CPYSTAT1,CPYSOROE   MUST ALSO BE ON OFFICE                       
*                                                                               
         MVC   HALF(1),CPYSTAT4   TEST CHANGED NEWOFF OPTION                    
         NI    HALF,CPYSOFF2                                                    
         MVC   HALF+1(1),SAVECOMP+(CPYSTAT4-CPYELD)                             
         NI    HALF+1,CPYSOFF2                                                  
         CLC   HALF(1),HALF+1                                                   
         BE    CHGR40                                                           
         MVI   WORK,TBAKTYPQ       READ FOR BATCH RECORDS                       
         MVC   WORK+1(1),KEY       COMPANY CODE                                 
         LA    R5,IO                                                            
         USING TBARECD,R5                                                       
         XC    TBAKEY,TBAKEY                                                    
         MVC   TBAKEY(2),WORK                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',IO,IO,0                      
         CLC   IO(2),WORK          TEST TRANSACTION BATCH RECORDS               
         BNE   CHGR40                                                           
         LA    R2,LOGSTATH                   SET CURSOR TO COMPANY              
         MVC   LOGHEAD(35),=C'** ERROR CAN''T CHANGE NEWOFF OPTION'             
         MVI   ERROR,X'FE'                   I SET THE ERROR MESSAGE            
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
CHGR40   CLC   LOGPASS,TMSPW       TMS CHANGE PASSWORD                          
         BE    CHGR41              PASSWORD CORRECT--CONTINUE                   
         MVC   HALF(1),CPYSTAT7   TEST CHANGED TMS OPTION                       
         NI    HALF,CPYSTMSY                                                    
         MVC   HALF+1(1),SAVECOMP+(CPYSTAT7-CPYELD)                             
         NI    HALF+1,CPYSTMSY                                                  
         CLC   HALF(1),HALF+1     TMS BIT SAME OLD AND NEW?                     
         BE    CHGR40A                                                          
         TM    CPYSTAT7,CPYSTMSY                                                
         BNO   CHGR40B             CAN'T REMOVE TMS                             
         B     CHGR41                                                           
*                                                                               
CHGR40A  CLC   CPYTMSSD,SAVECOMP+(CPYTMSSD-CPYELD)                              
         BE    CHGR41              SAME DATE IS OK                              
         MVC   LOGHEAD(35),=C'**ERROR CAN''T CHANGE TMS START DATE'             
         B     *+10                                                             
CHGR40B  MVC   LOGHEAD(35),=C'** ERROR CAN''T CHANGE TMS OPTION   '             
         LA    R2,LOGSTATH                   SET CURSOR TO COMPANY              
         MVI   ERROR,X'FE'                   I SET THE ERROR MESSAGE            
         B     XIT                                                              
*                                                                               
CHGR41   SR    RE,RE                                                            
         IC    RE,CPYLN            LENGTH OF ELEMENT IN R1                      
         BCTR  RE,0                REDUCE LENGTH BY 1                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CPYEL(0),SAVECOMP   COMPARE NEW TO OLD COMP EL                   
         BE    CHGR43              NOTHING HAS CHANGED                          
         XC    NEWCOMP,NEWCOMP     SAVE AMENDED COMPANY ELEMENT                 
         EX    RE,*+4                                                           
         MVC   NEWCOMP(0),CPYEL                                                 
         CLI   LFMACT,C'N'         IS ACTION 'NEW'                              
         BE    CHGR43              YES - DON'T CHECK PASSWORD                   
         CLC   LOGPASS,=C'XIX'     PASSWORD                                     
         BE    CHGR43              PASSWORD CORRECT--CONTINUE                   
*                                                                               
CHGR43   GOTO1 REMANEL,DMCB,('CPYELQ',0)                                        
         GOTO1 ADDANEL                                                          
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
*                                                                               
CMISS    MVI   ERROR,MISSING                                                    
         B     XIT                                                              
*                                                                               
CMINV    CLI   ERROR,X'FE'         I SET THE MESSAGE                            
         BE    XIT                                                              
         MVI   ERROR,INVALID                                                    
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
***********************************************************************         
*              SCAN STATUS LINES                                      *         
***********************************************************************         
*                                                                               
CMTCH    NTR1                                                                   
         CLI   5(R2),0             SCAN INPUT LINE                              
         BE    CMXIT                                                            
         GOTO1 VSCANNER,DMCB,(R2),(25,LWBLK)                                    
         ZIC   R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BZ    CMINV                                                            
*                                                                               
         LA    R3,LWBLK            MATCH ITEM IN BLOCK TO TABLE                 
CMTCH2   GOTO1 CMPRE,COMSTAT       MATCH AND SET BIT ON                         
         BE    CMTCH30             FOUND A MATCH                                
         GOTO1 CMPRE,COMSTA2                                                    
         BE    CMTCH30             FOUND A MATCH - SO GO TO NEXT ITEM           
         GOTO1 CMPRE,COMSTA3                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMSTA4       MATCH AND SET BIT ON                         
         BNE   *+12                                                             
         BAS   RE,CMNEWOF          SEE IF NEW OFF RECORDS ARE SETUP             
         B     CMTCH30                                                          
         GOTO1 CMPRE,COMSTA5                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMSTA6                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMSTA7                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMSTA8                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMSTA9                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMSTAA                                                    
         BE    CMTCH30                                                          
         GOTO1 CMPRE,COMCD                                                      
         BE    CMTCH30                                                          
*                                                                               
         USING DAYSD,RF                                                         
         CLC   12(3,R3),=C'TSD'    TIME SHEET DAY                               
         BNE   CMTCH3                                                           
         LA    RF,DAYS                                                          
         LA    R0,DAYSLNQ          # TIMES TO LOOP                              
         CLC   DAYNAME,22(R3)                                                   
         BE    *+16                                                             
         LA    RF,DAYQ(RF)                                                      
         BCT   R0,*-14                                                          
         B     CMINV               INVALID IF NOT 'MON' 'TUE' ETC...            
         MVC   CPYTSD,DAYNUM       SAVE DAY # IF A MATCH                        
         B     CMTCH30                                                          
         DROP  RF                                                               
*                                                                               
CMTCH3   CLC   12(2,R3),=C'ID'                                                  
         BNE   CMTCH4                                                           
         OC    8(4,R3),8(R3)                                                    
         BZ    CMINV                                                            
         MVC   CPYUID,10(R3)                                                    
         B     CMTCH30                                                          
*                                                                               
CMTCH4   CLC   12(2,R3),=C'TE'                                                  
         BNE   CMTCH5                                                           
         TM    3(R3),X'80'         NUMERIC                                      
         BZ    CMINV                                                            
         MVC   CPYTENO,22(R3)                                                   
         B     CMTCH30                                                          
*                                                                               
CMTCH5   CLC   12(4,R3),=C'DPTL'   LENGTH OF DEPARTMENT                         
         BNE   CMTCH6                                                           
         TM    3(R3),X'80'         TEST NUMERIC                                 
         BZ    CMINV               INVALID INPUT                                
         CLC   8(4,R3),=F'4'       4 IS THE LONGEST DEPT.                       
         BH    CMINV                                                            
         MVC   CPYDEPTL,11(R3)     LENGTH TO ELEMENT                            
         B     CMTCH30                                                          
*                                                                               
CMTCH6   CLC   12(5,R3),=C'GLOFF'  GENERAL LEDGER OFFICE                        
         BNE   CMTCH7                                                           
         CLC   8(2,R3),=F'2'       2 IS THE LONGEST OFFICE                      
         BH    CMINV                                                            
         MVC   CPYOFFC,22(R3)      OFFICE CODE                                  
         B     CMTCH30                                                          
*                                                                               
CMTCH7   CLC   12(3,R3),=C'TID'                                                 
         BNE   CMTCH8                                                           
         CLI   1(R3),2                                                          
         BNE   CMINV                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(2),22(R3)                                                   
         GOTO1 VHEXIN,DMCB,WORK,WORK+12,2                                       
         MVC   CPYTCMP,WORK+12                                                  
**       LA    R4,IO2                                                           
         MVC   IO(42),SPACES        CLEAR KEY AND READ TO SEE IF THIS           
         MVC   IO(1),WORK+12        COMPANY EXISTS ON FILE                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',IO,IO,0                      
         CLI   8(R1),0                                                          
         BNE   CMINV                                                            
         B     CMTCH30                                                          
*                                                                               
CMTCH8   CLC   12(3,R3),=C'TMS'                                                 
         BNE   CMTCH9                                                           
         OI    CPYSTAT7,CPYSTMSY    SET COMPANY ON TMS                          
         XC    CPYTMSSD,CPYTMSSD                                                
         CLI   1(R3),0              IS THERE A DATE?                            
         BE    CMTCH30              NO                                          
*MN                                                                             
         CLI   LOGACT,C'N'                                                      
         BE    CMTCH30                                                          
*MN                                                                             
         MVC   WORK,SPACES                                                      
         GOTO1 DATVAL,DMCB,(0,22(R3)),WORK  FIRST VALI-DATE                     
         OC    0(4,R1),0(R1)                                                    
         BZ    CMINV                INVALID DATE                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,CPYTMSSD)                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+10)                                    
         LA    RF,SAVECOMP                                                      
         CLC   CPYTMSSD,CPYTMSSD-CPYELD(RF)                                     
         BE    CMTCH8AA                                                         
         CLC   LOGPASS,TMSPW        TMS CHANGE PASSWORD                         
         BE    CMTCH8AA             PASSWORD CORRECT--CONTINUE                  
         CLC   WORK(6),WORK+10      TMS DATE CANNOT BE LOWER THAN TODAY         
         BL    CMINV                                                            
CMTCH8AA MVC   WORK+10(6),SPACES                                                
*                                                                               
*                                                                               
*                                   DATE MUST BE 1ST START DATE OF A            
*                                   CALENDAR RECORD MONTH                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+10)                                 
         USING CASRECD,R6          R6=A(CALENDAR RECORD)                        
         LA    R6,IO                                                            
         MVC   CASPAS,SPACES                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,KEY         HEXCOMP FROM THE COMP REC KEY                
         MVC   KEYSAVE,CASPAS                                                   
         MVC   CASPEDTE,WORK+10                                                 
*                                  READHI FOR CORRECT YEAR                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',IO,IO                         
         CLC   CASPAS(3),KEYSAVE                                                
         BNE   CMTCH8E             CAN'T FIND CALENDAR RECORD                   
         CLC   WORK+10(L'CASPSDTE),CASPSDTE                                     
         BL    CMTCH8E                                                          
         CLC   WORK+10(L'CASPEDTE),CASPEDTE                                     
         BH    CMTCH8E                                                          
         MVC   SAVEDA,CASKDA       SAVE DEFAULT DISK ADDRESS                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',SAVEDA,IO,DMWORK              
         BNE   CMTCH8E             DISK ERROR                                   
         LA    RE,CASRFST          BUMP TO FIRST ELEMENT                        
         XC    DUB,DUB             USE DUB TO SAVE MONTH                        
         USING TMPELD,RE                                                        
CMTCH8A  CLI   TMPEL,0                                                          
         BE    CMTCH8D                                                          
         CLI   TMPEL,TMPELQ                                                     
         BE    CMTCH8C                                                          
CMTCH8B  SR    R1,R1                                                            
         IC    R1,TMPLN                                                         
         AR    RE,R1                                                            
         B     CMTCH8A                                                          
CMTCH8C  CLC   TMPMTH,DUB          ONLY CHECK 1ST OCCURANCE OF MONTH            
         BE    CMTCH8B                                                          
         MVC   DUB(L'TMPMTH),TMPMTH                                             
         CLC   TMPSTART,WORK+10    MUST MATCH 1ST START DATE OF MONTH           
         BNE   CMTCH8B                                                          
         B     CMTCH30                                                          
CMTCH8D  MVC   LOGHEAD(35),=C'** ERROR- TMS ST NOT = TO PERIOD ST'              
         B     *+10                                                             
CMTCH8E  MVC   LOGHEAD(35),=C'** ERROR- MISSING CALENDAR FOR TMS '              
         MVI   ERROR,X'FE'                   I SET THE ERROR MESSAGE            
         B     CMINV                                                            
         DROP  RE,R6                                                            
*                                                                               
CMTCH9   CLC   12(6,R3),=C'CTFILE' AGENCY ALPHA ID                              
         BNE   CMINV                                                            
         CLC   8(2,R3),=F'2'       ID'S ARE 2 BYTES                             
         BNE   CMINV                                                            
*                                                                               
         LA    R6,IO                                                            
         USING CT5REC,R6                                                        
         XC    CT5KEY,CT5KEY             CLEAR KEY FOR ALPHA LOOKUP             
         MVI   CT5KTYP,CT5KTYPQ          RECORD TYPE 5 FOR ALPHA REC            
         MVC   CT5KALPH,22(R3)           ALPHA ID                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IO,IO,0                       
         CLI   8(R1),0                                                          
         BNE   CMINV                                                            
*                                                                               
         MVC   CPYCTFIL,22(R3)     ALPHA ID CODE                                
*                                                                               
CMTCH30  LA    R3,32(R3)                                                        
         BCT   R5,CMTCH2                                                        
*                                                                               
CMXIT    XIT1                                                                   
*                                                                               
CMPRE    SR    R6,R6                                                            
         IC    R6,0(R1)            GET DISP TO DATA                             
         CLM   R6,1,CPYLN          TEST IF ITS OUTSIDE ELEMENT                  
         BH    CMPR6               YES                                          
         LA    R6,CPYELD(R6)                                                    
         LA    R7,1(R1)            POINT AT TABLE DATA                          
*                                                                               
CMPR1    CLI   0(R7),X'FF'         TEST FOR EOT                                 
         BE    CMPR6                                                            
         CLC   12(10,R3),2(R7)     ITEM IN BLOCK - TABLE                        
         BNE   CMPR2                                                            
         CLI   0(R7),1             ONE SIDED ENTRY                              
         BE    *+14                                                             
         CLC   22(10,R3),12(R7)    MUST MATCH RIGHT SIDE OF EQUAL               
         BNE   CMPR2                                                            
         OC    0(1,R6),1(R7)       TURN ON BIT                                  
         CR    R5,R5               FORCE EQUAL FOR RETURN                       
         BR    RE                                                               
*                                                                               
CMPR2    CLI   0(R7),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R7,10(R7)           NEXT TABLE ENTRY                             
         LA    R7,12(R7)                                                        
         B     CMPR1                                                            
*                                                                               
CMPR6    CLI   0(R7),X'00'         FORCE NOT EQUAL                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              NEW OFFICES CHECK                                      *         
***********************************************************************         
*                                                                               
CMNEWOF  NTR1                                                                   
         CLC   12(6,R3),=C'NEWOFF'                                              
         BNE   CMNEWXIT                                                         
         LA    R5,IO                                                            
         USING OFFKEY,R5                                                        
         MVC   OFFKEY,SPACES       MAKE SURE THAT OFFICE RECS EXIST             
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,KEY         HEXCOMP FROM THE COMP REC KEY                
         MVC   KEYSAVE,OFFKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',IO,IO,0                      
         CLC   KEYSAVE(L'OFFKTYP+L'OFFKCPY),OFFKEY                              
         BE    CMNEWXIT                                                         
         LA    R2,LOGCOMPH         SET CURSOR TO COMPANY                        
         MVC   LOGHEAD(L'OFFERROR),OFFERROR                                     
         MVI   ERROR,X'FE'         I SET THE MESSAGE                            
CMNEWXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              BUILD OUTPUT BLOCK FOR UNSCAN                          *         
***********************************************************************         
*                                                                               
OUTBLK   NTR1                                                                   
         SR    R6,R6                                                            
         IC    R6,0(R1)            GET DISP TO STATUS BYTE                      
         CLM   R6,1,CPYLN          TEST IF DATA IS WITHIN ELEMENT               
         BH    OUTBLKX             NO                                           
         LA    R6,CPYELD(R6)       POINT TO STATUS DATA                         
         LA    R7,1(R1)            R7=A(STATUS TABLE DATA)                      
*                                                                               
OUTNXT   CLI   0(R7),X'FF'         TEST FOR EOT                                 
         BE    OUTBLKX             YES                                          
         MVC   WORK(1),1(R7)       BIT FROM TABLE                               
         NC    WORK(1),0(R6)       STATUS BIT                                   
         BZ    OUTXT               NOT ON                                       
         LA    R5,1(R5)            COUNT OUTPUT ITEMS                           
         MVC   0(20,R3),SPACES     SPACES TO BLOCK                              
*                                                                               
         MVC   0(10,R3),2(R7)      LEFT SIDE TO BLOCK                           
         CLI   0(R7),1             ONE SIDED                                    
         BE    *+10                YES                                          
         MVC   10(10,R3),12(R7)    RIGHT SIDE TO BLOCK                          
         LA    R3,20(R3)                                                        
*                                                                               
OUTXT    CLI   0(R7),1                                                          
         BE    *+8                                                              
         LA    R7,10(R7)                                                        
         LA    R7,12(R7)                                                        
         B     OUTNXT                                                           
*                                                                               
OUTBLKX  XIT1  REGS=(R3,R5)                                                     
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE SECONDARY SCREEN                                        *         
***********************************************************************         
         SPACE 1                                                                
DSPSEC   XR    R1,R1               PREVENT CHANGES TO KEY FIELDS                
         LA    RE,LOGRECH                                                       
         LA    RF,SECAGT1H                                                      
         OI    1(RE),X'20'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CR    RE,RF                                                            
         BL    *-12                                                             
*                                                                               
         LA    R4,IO2                                                           
         AH    R4,=Y(ACRECORD-ACKEYD)                                           
         XR    RF,RF                                                            
DSPS02   CLI   0(R4),0                                                          
         BE    DSPSX                                                            
         CLI   0(R4),FFTELQ                                                     
         BE    *+14                                                             
DSPS04   IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     DSPS02                                                           
*                                                                               
         USING FFTELD,R4                                                        
         TM    NEWCOMP+(CPYSTAT7-CPYELD),CPYSAGRP                               
         BO    DSPS06                                                           
         OI    SECAGN1H+(FLDATB-FLDHDRD),FATBPROT                               
         OI    SECAGN2H+(FLDATB-FLDHDRD),FATBPROT                               
         OI    SECAGT1H+(FLDATB-FLDHDRD),FATBLOW                                
         OI    SECAGT2H+(FLDATB-FLDHDRD),FATBLOW                                
         B     DSPS04                                                           
DSPS06   CLI   FFTTYPE,FFTTAAGR    ACCOUNT GROUP NAMES                          
         BNE   DSPS04                                                           
         BAS   RE,DSPSR                                                         
         CLI   FFTSEQ,1                                                         
         BNE   *+14                                                             
         MVC   SECAGN1,WORK                                                     
         B     DSPS04                                                           
         CLI   FFTSEQ,2                                                         
         BNE   DSPS04                                                           
         MVC   SECAGN2,WORK                                                     
         B     DSPS04                                                           
*                                                                               
DSPSR    MVC   WORK,SPACES                                                      
         XR    R1,R1                                                            
         IC    R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),FFTDATA                                                  
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
DSPSX    LA    RF,AI$SECDS         'SECONDARY SCREEN DISPLAYED'                 
         LA    R2,LOGCOMPH                                                      
         CLI   LFMACT,C'I'                                                      
         BE    *+12                                                             
         LA    RF,AI$SECDC          PLUS ' - NOW ENTER CHANGES'                 
         LA    R2,SECAGT1H                                                      
         XC    WORK,WORK                                                        
         GOTO1 VGETTXT,WORK,(RF),(0,LOGHEADH),(C'I',0)                          
         OI    LOGHEADH+1,X'08'    HIGHLIGHT                                    
         MVI   ERROR,X'FE'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHANGE/ADD THE SECONDARY SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
CHGSEC   LA    R2,LOGNAMEH         COMPANY NAME                                 
         GOTO1 NAMIN                                                            
         LA    R4,ELEMENT                                                       
         USING FFTELD,R4                                                        
*                                                                               
CHGS14   TM    NEWCOMP+(CPYSTAT7-CPYELD),CPYSAGRP                               
         BZ    CHGSECX                                                          
         TM    SECAGN1H+4,X'80'    ACCOUNT GROUP NAMES                          
         BO    *+12                                                             
         TM    SECAGN2H+4,X'80'                                                 
         BZ    CHGSECX                                                          
         LA    RF,FFTTAAGR                                                      
         GOTO1 REMANEL,DMCB,('FFTELQ',(RF))                                     
         XC    ELEMENT,ELEMENT                                                  
         LA    R2,SECAGN1H                                                      
         MVI   FFTSEQ,1                                                         
CHGS16   CLI   5(R2),0                                                          
         BE    CHGS18                                                           
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTAAGR                                                 
         XR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         STC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   FFTDATA(0),8(R2)                                                 
         LA    R1,FFTLN1Q+2(R1)                                                 
         STC   R1,FFTLN                                                         
         GOTO1 ADDANEL                                                          
CHGS18   CLI   FFTSEQ,2                                                         
         BE    CHGS20                                                           
         OI    SECAGN1H+6,X'80'                                                 
         XC    ELEMENT,ELEMENT                                                  
         LA    R2,SECAGN2H                                                      
         MVI   FFTSEQ,2                                                         
         B     CHGS16                                                           
CHGS20   OI    SECAGN2H+6,X'80'                                                 
*                                                                               
CHGSECX  MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SAVE THE CURRENT SCREEN TWA0 IN TWA3                                *         
***********************************************************************         
         SPACE 1                                                                
SSCRN    NTR1  ,                                                                
         LA    R5,SCRNTAB          GET ENTRY FOR THIS ACTION                    
         SPACE 1                                                                
         USING SCRD,R5                                                          
SSCRN01  CLC   CSSCRN,SCREQU                                                    
         BE    SSCRN03                                                          
         LA    R5,SCRLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   SSCRN01                                                          
         DC    H'0'                ACTION NOT IN SCREEN TABLE                   
         SPACE 1                                                                
SSCRN03  XC    DMCB(20),DMCB                                                    
         MVC   DMCB+10(2),TWATRM   TERMINAL NO                                  
         MVI   DMCB+8,3            PAGE 3                                       
         MVI   DMCB+9,0                                                         
         LA    RF,LOGHEADH                                                      
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(RF)                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RESTORE SAVED SCREEN                                                *         
***********************************************************************         
         SPACE 1                                                                
RSCRN    NTR1  ,                                                                
         LA    R5,SCRNTAB          GET ENTRY FOR THIS ACTION                    
         SPACE 1                                                                
         USING SCRD,R5                                                          
RSCRN01  CLC   CSSCRN,SCREQU                                                    
         BE    RSCRN03                                                          
         LA    R5,SCRLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   RSCRN01                                                          
         DC    H'0'                ACTION NOT IN SCREEN TABLE                   
         SPACE 1                                                                
RSCRN03  XC    DMCB(24),DMCB                                                    
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
         MVI   DMCB+8,3            PAGE 3                                       
         MVI   DMCB+9,0                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),SCRLEN   LENGTH OF SAVE                               
         LA    RF,LOGHEADH                                                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TEMPSTR                           
         SPACE 1                                                                
         LA    RE,LOGHEADH         TRANSMIT SCREEN                              
         SR    R1,R1                                                            
         SPACE 1                                                                
RSCRN04  OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   RSCRN04                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET A NEW SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
GSCRN    NTR1  ,                                                                
         MVC   LSTCOMP,LOGCOMP     SAVE THE LAST COMPANY                        
         LA    R5,SCRNTAB          GET ENTRY FOR THIS ACTION                    
         SPACE 1                                                                
         USING SCRD,R5                                                          
GSCRN01  CLC   CSSCRN,SCREQU                                                    
         BE    GSCRN03                                                          
         LA    R5,SCRLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   GSCRN01                                                          
         DC    H'0'                ACTION NOT IN SCREEN TABLE                   
         SPACE 1                                                                
GSCRN03  XC    DMCB(20),DMCB                                                    
         MVC   DMCB+4(4),=X'D9060300'                                           
         MVC   DMCB+7(1),SCRCDE    SCREEN NUMBER                                
         XR    RF,RF                                                            
         ICM   RF,3,SCRSTRT        START OF SCREEN SAVE                         
         AR    RF,RA                                                            
         GOTO1 CALLOV,DMCB,(0,(RF))                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CANT READ SCREEN                             
         SPACE 1                                                                
         LA    RE,LOGHEADH         TRANSMIT SCREEN                              
         SR    R1,R1                                                            
         SPACE 1                                                                
GSCRN04  OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   GSCRN04                                                          
         XC    1(2,RE),1(RE)                                                    
         MVC   LOGCOMP,LSTCOMP     RESTORE THE LAST COMPANY                     
         MVI   LOGCOMPH+5,L'LOGCOMP                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
INVERR   XC    WORK,WORK                                                        
         GOTO1 VGETTXT,WORK,(RF),(0,LOGHEADH),(C'E',0)                          
         LA    R2,LOGACTH                                                       
         MVI   ERROR,X'FE'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              BUILD OUTPUT BLOCK FOR UNSCAN                          *         
***********************************************************************         
*                                                                               
SYSWITCH NTR1                                                                   
         USING COMFACSD,RF                                                      
         L     RF,COMFACS                                                       
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VGETFACT,CGETFACT                                                
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         CLI   FASYSID-FACTSD(R1),1 IS THIS THE TEST SYSTEM                     
         BE    SWITCHX              IF SO - EXIT                                
         MVC   CONSYS,FASYS-FACTSD(R1)                                          
         XC    IO(25),IO        BUILD KEY OF SYSTEM LIST RECORD                 
         MVI   IO,C'W'                                                          
         MVI   IO+17,C'S'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IO,IO                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'             SYSTEM LIST RECORD NOT FOUND                    
         LA    RF,IO+28         RF=A(FIRST ELEMENT)                             
         LA    R5,SYSLIST                                                       
         USING SYSLIST,R5                                                       
         LA    R4,SYSMAX                                                        
*                                                                               
SWITCH1  CLI   0(RF),0          END OF RECORD                                   
         BE    SWITCH3                                                          
         CLI   0(RF),X'A4'      COMPANY ELEMENT                                 
         BNE   SWITCH2                                                          
         CLI   10(RF),6         ACCOUNTING SYSTEM                               
         BNE   SWITCH2                                                          
         MVC   SYSLIST(SYSLEN),3(RF)                                            
         LA    R5,SYSLEN(R5)                                                    
         MVI   0(R5),0          SET END OF LIST                                 
         BCT   R4,*+6                                                           
         DC    H'0'             MAKE TABLE BIGGER                               
*                                                                               
SWITCH2  ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     SWITCH1                                                          
         DROP  R5                                                               
*                                                                               
         USING SYSLIST,R5                                                       
SWITCH3  LA    R5,SYSLIST                                                       
*                                                                               
SWITCH4  CLI   0(R5),0          END OF LIST                                     
         BE    SWITCH6                                                          
         CLC   CONSYS,SYSNUM                                                    
         BE    SWITCH5                                                          
         GOTO1 SWITCH,SYSNUM    SWITCH TO SYSTEM                                
         GOTO1 HIGH             READ HIGH FOR COMPANY RECORD                    
         CLC   KEY,KEYSAVE      TEST CODE ALREADY USED                          
         BNE   *+12                                                             
         MVI   ERROR,RECONFLE                                                   
         B     SWITCH6                                                          
         MVC   KEY,KEYSAVE                                                      
*                                                                               
SWITCH5  LA    R5,SYSLEN(R5)                                                    
         B     SWITCH4                                                          
*                                                                               
SWITCH6  GOTO1 SWITCH,CONSYS                                                    
         B     SWITCHX                                                          
         DROP  R5                                                               
*                                                                               
SWITCH   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),0(R1)    SET SWITCH-TO SYSTEM NUMBER                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 VSWITCH,DMCB                                                     
         CLI   4(R1),0          TEST SWITCH OK                                  
         BE    *+6                                                              
         DC    H'0'                                                             
SWITCHX  XIT1                                                                   
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*              STATUS BIT TABLES                                      *         
***********************************************************************         
*                                                                               
*              BYTE 1  01 = ONE SIDED ENTRY                                     
*                      02 = TWO SIDED ENTRY                                     
*              BYTE 2  BIT SETTING                                              
*              BYTE 3-12  LEFT SIDE OR ENTRY                                    
*              BYTE 13-22 RIGHT SIDE OF ENTRY (OPTIONAL)                        
*                                                                               
COMSTAT  DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    X'01',AL1(CPYSIOMR),CL10'MTCHORD   '                             
         DC    X'01',AL1(CPYSCIVE),CL10'CKINV     '                             
         DC    X'01',AL1(CPYSOROE),CL10'OFFICE    '                             
         DC    X'01',AL1(CPYSCOST),CL10'COST      '                             
         DC    X'01',AL1(CPYSDISC),CL10'CD        '                             
         DC    X'01',AL1(CPYSGENA),CL10'GA        '                             
         DC    X'02',AL1(CPYSNOJL),CL10'LBLS      ',CL10'N'                     
         DC    X'02',AL1(CPYSNOET),CL10'ESTA      ',CL10'N'                     
         DC    X'FF'                                                            
*                                                                               
COMSTA2  DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    X'02',AL1(CPYSETPP),CL10'ESTA      ',CL10'PLN'                   
         DC    X'02',AL1(CPYSETAC),CL10'ESTA      ',CL10'ACT'                   
         DC    X'01',AL1(CPYSEBIF),CL10'BILLEST   '                             
         DC    X'02',AL1(CPYSETDO),CL10'ESTA      ',CL10'DIF'                   
         DC    X'01',AL1(CPYSCKDP),CL10'CHKDUP    '                             
         DC    X'01',AL1(CPYSVENR),CL10'VEND      '                             
         DC    X'02',AL1(CPYSCACA),CL10'SJCNTRA   ',CL10'SC'                    
         DC    X'01',AL1(CPYSERTP),CL10'PAYEST    '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA3  DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    X'02',AL1(CPYSSXCC),CL10'SXCNTRA   ',CL10'CL'                    
         DC    X'01',AL1(CPYSWO14),CL10'WO        '                             
         DC    X'02',AL1(CPYSPC1C),CL10'PC        ',CL10'1C'                    
         DC    X'02',AL1(CPYSOPBM),CL10'BA        ',CL10'Y'                     
         DC    X'02',AL1(CPYSCA22),CL10'SECNTRA   ',CL10'SC'                    
         DC    X'02',AL1(CPYSPCSJ),CL10'PC        ',CL10'SJ'                    
         DC    X'01',AL1(CPYSDPST),CL10'DPS       '                             
         DC    X'02',AL1(CPYSBSEC),CL10'BS        ',CL10'Y'                     
         DC    X'FF'                                                            
*                                                                               
COMSTA4  DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    X'02',AL1(CPYSPESK),CL10'%EST      ',CL10'SK'                    
         DC    X'02',AL1(CPYSOV12),CL10'BBD       ',CL10'Y'                     
         DC    X'01',AL1(CPYSNPRD),CL10'NEWPROD   '                             
         DC    X'01',AL1(CPYSMINT),CL10'MI        '                             
         DC    X'01',AL1(CPYSOFF2),CL10'NEWOFF    '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA5  DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    X'02',AL1(CPYSOFPL),CL10'OFF       ',CL10'P&&L'                  
         DC    X'02',AL1(CPYSBAPR),CL10'APP       ',CL10'REG'                   
         DC    X'02',AL1(CPYSBAPE),CL10'APP       ',CL10'EFF'                   
         DC    X'01',AL1(CPYSNCST),CL10'NEWCOST   '                             
         DC    X'01',AL1(CPYSEXPP),CL10'EXPROD    '                             
         DC    X'01',AL1(CPYSVEND),CL10'VENCOP    '                             
         DC    X'01',AL1(CPYAPGS),CL10'APG$'                                    
         DC    X'FF'                                                            
*                                                                               
COMSTA6  DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    X'01',AL1(CPYSADVP),CL10'ADVPTR    '                             
         DC    X'01',AL1(CPYSRAPP),CL10'RAPTR     '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA7  DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    X'01',AL1(CPYSAGRP),CL10'ACTGRPS   '                             
         DC    X'01',AL1(CPYSJTIM),CL10'JTIME     '                             
         DC    X'01',AL1(CPYSL1NA),CL10'PERAN     '                             
         DC    X'01',AL1(CPYSNEWB),CL10'NEWBILL   '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA8  DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    X'01',AL1(CPYSRLOG),CL10'RLOGO     '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA9  DC    AL1(CPYSTAT9-CPYELD)                                             
         DC    X'01',AL1(CPYSEDHO),CL10'MINHOURS '                              
         DC    X'FF'                                                            
*                                                                               
COMSTAA  DC    AL1(CPYSTATA-CPYELD)                                             
         DC    X'01',AL1(CPYSACCT),CL10'ACCENT    '                             
         DC    X'02',AL1(CPYSPOFU),CL10'PXTRDATA  ',CL10'OF'                    
         DC    X'02',AL1(CPYSPOLU),CL10'PXTRDATA  ',CL10'OL'                    
         DC    X'FF'                                                            
*                                                                               
COMCD    DC    AL1(CPYCDC-CPYELD)                                               
         DC    X'02',C'N',CL10'DISC       ',CL10'N'                             
         DC    X'FF'                                                            
*                                                                               
DAYS     DC    AL1(1),C'MON'                                                    
         DC    AL1(2),C'TUE'                                                    
         DC    AL1(3),C'WED'                                                    
         DC    AL1(4),C'THU'                                                    
         DC    AL1(5),C'FRI'                                                    
         DC    AL1(6),C'SAT'                                                    
         DC    AL1(7),C'SUN'                                                    
DAYSLNQ  EQU   (*-DAYS)/4                                                       
*                                                                               
MONTHS   DC    C'JANFEBMARAPRMAYJUN'                                            
         DC    C'JULAUGSEPOCTNOVDEC'                                            
MONLST   DC    C'123456789ABC'                                                  
OFFERROR DC    CL40'** ERROR -- NO OFFICE RECORDS FOUND **  '                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND TABLES                                                *         
***********************************************************************         
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
DMREAD   DC    C'DMREAD '                                                       
*                                                                               
TMSPW    DC    C'@@@'                                                           
*        SCREEN EQUATES                                                         
PRI      EQU   X'01'               PRIMARY SCREEN                               
SEC      EQU   X'02'               SECONDARY SCREEN                             
*                                                                               
*        PFKEY EQUATES                                                          
PFSEC    EQU   1                   SECONDARY SCREEN                             
PFRTN    EQU   12                  RETURN                                       
*                                                                               
*        VALID PF KEYS : SCREEN/KEY/NEW SCREEN/A(DSP/ROUT)/A(CHG/ROUT)          
PFTAB    DC    AL1(PRI,PFSEC,SEC),AL3(DSPSEC-T6030E,CHGSEC-T6030E)              
PFTABQ   EQU   *-PFTAB                                                          
         DC    X'FF'                                                            
*                                                                               
*        SCREEN TABLE                                                           
SCRNTAB  DC   AL1(PRI),X'F1',AL2(LOGTABH-T603FFD),AL2(LOGWORK-LOGHEADH)         
         DC   AL1(SEC),X'C9',AL2(LOGENDH-T603FFD),AL2(SECWORK-LOGHEADH)         
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         SPACE 1                                                                
SCRD     DSECT                     ** SCREEN TABLE **                           
SCREQU   DS    XL1                 SCREEN EQUATE                                
SCRCDE   DS    XL1                 SCREEN CODE                                  
SCRSTRT  DS    XL2                 DISP.  FROM START OF TWA                     
SCRLEN   DS    XL2                 LENGTH OF SAVE                               
SCRLNQ   EQU   *-SCRD                                                           
         EJECT                                                                  
***********************************************************************         
*              DAY TABLE DSECT                                        *         
***********************************************************************         
*                                                                               
DAYSD    DSECT                                                                  
DAYNUM   DS    XL1                 CORRESPONDING DAY NUMBER FROM GETDAY         
DAYNAME  DS    CL3                 ABBREVIATED DAY                              
DAYQ     EQU   *-DAYSD                                                          
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR LOCAL WORKING STORAGE                        *         
***********************************************************************         
*                                                                               
LWSD     DSECT                                                                  
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VSCANNER DS    V                                                                
VUNSCAN  DS    V                                                                
VGETTXT  DS    V                                                                
ELCODE   DS    XL1                                                              
LWBLK    DS    25CL32              SCAN / UNSCAN BLOCK                          
LWSX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*              SCREENS / SAVED STORAGE                                *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF1D                                                       
         ORG   LOGENDH                                                          
       ++INCLUDE ACLFMC9D                                                       
         ORG   LOGTAB+2400                                                      
SAVECOMP DS    CL128               COMPANY ELEMENT SAVE AREA                    
NEWCOMP  DS    CL128               COMPANY ELEMENT AFTER FIRST SCREEN           
*                                                                               
         ORG   T603FFD                                                          
*                                                                               
*                                                                               
TWAMXLEN EQU   6144                MAXIMUM LENGTH OF OLD TWA RECORD             
TWAMAXRL EQU   14336               MAXIMUM LENGTH OF NEW TWA RECORD             
TWAMAX   EQU   18432               MAXIMUM LENGTH OF LATEST  RECORD             
*                                                                               
TWATASK  DS    C         +0        PROCESSING TASK NUMBER                       
TWAOFFC  DS    C         +1        OFFICE CODE                                  
TWATRM   DS    0H        +2        TERMINAL NUMBER                              
TWAFLDNM DS    X         +2        DDREQTWA - SAVE NUMBER OF LAST FIELD         
         DS    X         +3        N/D OFFLINE                                  
TWASAGN  DS    XL2       +4        ACCESS GROUP# (IF PGMIND2=PGMISECA)          
TWAACCS  DS    CL4       +6        LIMIT ACCESS CODE                            
TWAUSRID DS    XL2       +10       CONNECT ID NUM (OLAI)                        
TWAAUTH  DS    XL2       +12       AUTHORIZATION CODE                           
TWAAGY   DS    CL2       +14       EBCDIC AGENCY CODE                           
*                                                                               
TWALEN   EQU   TWASAGN,2           MESSAGE LENGTH                               
TWASMI   EQU   TWAUSRID,2          DISPLACEMENT TO SMI (NON-OLAI)               
*                                                                               
TWAUSER  DS    CL48      +16       AVAILABLE FOR USER                           
*                                                                               
         ORG   TWAUSER                                                          
         DS    XL4                 USED BY BASE (PHASE/SCREEN SEQ.)             
CSSCRN   DS    CL1                 CURRENT SCREEN VALUE                         
SVPFKEY  DS    CL1                 SAVED PFKEY - LAST ENTERED                   
RTNCURSE DS    A                   A(SAVED CURSOR FIELD)                        
DSPROUT  DS    A                   A(DISPLAY ROUTINE)                           
CHGROUT  DS    A                   A(CHANGE ROUTINE)                            
LSTCOMP  DS    CL2                 SAVE COMPANY CODE                            
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
*                                                                               
SAVE4    DS    F                                                                
CONSYS   DS    X                                                                
VSWITCH  DS    V                                                                
VGETFACT DS    V                                                                
SYSLIST  DS    0H                                                               
SYSNAME  DS    CL7                                                              
SYSOVLY  DS    X                                                                
SYSNUM   DS    X                                                                
SYSLOG   DS    X                                                                
SYSLEN   EQU   *-SYSLIST                                                        
         ORG   SYSLIST                                                          
SYSMAX   EQU   10                                                               
         DS    (SYSMAX)XL(SYSLEN)                                               
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACLFM0E   07/07/03'                                      
         END                                                                    

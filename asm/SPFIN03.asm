*          DATA SET SPFIN03    AT LEVEL 052 AS OF 04/07/08                      
*PHASE T20903A,*                                                                
         TITLE 'FIN03 T20903  FINANCIAL INFORMATION - LIST OVERLAY'             
T20903   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20903**,R4,RR=RE                                              
*                                                                               
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         USING WORKD,R7,R8                                                      
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
*                                                                               
         LA    R1,IOHOOK                                                        
         ST    R1,SBIOHOOK           RESET ADD EVERY TIME                       
*                                                                               
         CLI   APMODE,APMFSCR        FIRST FOR SCREEN                           
         BE    FSTSCR                                                           
         CLI   APMODE,APMVALP        VALIDATE KEY FOR LIST/SELECT               
         BE    VALPAR                                                           
         CLI   APMODE,APMGETS        GET RECORD FOR LIST/SELECT                 
         BE    GETSEL                                                           
         CLI   APMODE,APMDISS        DISPLAY RECORD FOR LIST/SELECT             
         BE    DISSEL                                                           
         CLI   APMODE,APMLSCR        LAST FOR SCREEN                            
         BE    LSTSCR                                                           
         B     EXIT                                                             
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*===============*                                                               
* VALPAR  KEY   *                                                               
*===============*                                                               
*                                                                               
VALPAR   LA    R2,APRECKEY        DISPLAY KEY                                   
         USING APRECD,R2                                                        
*                                                                               
         BAS   RE,SETFLD          SET SCREEN FROM LISTFLD                       
         GOTO1 AVALKEYD           VALIDATE KEY FOR LIST/SELECT                  
         BNE   VALPX                                                            
*                                                                               
         SR    RE,RE              # OF LISTS                                    
         OC    APPRD,APPRD        PRODUCT LIST ?                                
         BNZ   VALPAR10           NO                                            
         LA    RE,1(RE)           YES - SET COUNT AND FLAG                      
         MVI   LISTYPE,C'P'                                                     
VALPAR10 CLI   APMOSFLG+1,C'L'    MOS LIST?                                     
         BNE   VALPAR15           YES                                           
         LA    RE,1(RE)                                                         
         MVI   LISTYPE,C'M'                                                     
         CLI   APMOSFLG,C'M'                                                    
         BE    *+8                                                              
         MVI   LISTYPE,C'S'                                                     
*                                                                               
VALPAR15 CLC   =C'LST',APEST                                                    
         BNE   VALPAR18           NO LIST                                       
         LA    RE,1(RE)                                                         
         MVI   LISTYPE,C'E'                                                     
*                                                                               
VALPAR18 LTR   RE,RE                                                            
         BZ    VALPAR30           IF NO LIST -- SWITCH TO DISPLAY               
         MVC   SVOPTS(L'INOLEN),INOSPL                                          
         CH    RE,=H'2'           MORE THAN ONE LIST?                           
         BL    VALPAR20           NO - PROCESS IT                               
         MVC   FVMSGNO,=AL2(FVONELST)                                           
         B     VALPX                                                            
*                                                                               
VALPAR20 TM    INOIND,INOIWK      WEEKLY OPTION REQUESTED?                      
         BNO   VALPAR25                                                         
         MVC   FVMSGNO,=AL2(FVFKINV) WEEKLY NOT ALLOWED WITH LIST               
         LA    R1,FINOPTH                                                       
         STCM  R1,15,APCURSOR                                                   
         B     VALPX                                                            
*                                                                               
VALPAR25 LA    RE,FLTSL1H         SET APPARM FIELDS FOR CONTROLLER              
         ST    RE,APPARM          A(FIRST TWA SELECT FIELD HEADER)              
         LA    RE,NLINES          # NUMBER OF LINES ON SCREEN                   
         STC   RE,APPARM+4                                                      
         LA    RE,FLTSL2H-FLTSL1H                                               
         STCM  RE,3,APPARM+6      LENGTH OF SELECT LINE                         
         B     VALPX                                                            
*                                                                               
VALPAR30 MVI   APMODE,APMSWP      SWITCH TO DISPLAY                             
         LA    R1,RECFIN                                                        
         STC   R1,APPARM                                                        
         LA    R1,ACTDRP                                                        
         STC   R1,APPARM+1                                                      
         NI    TWAMODE,X'FF'-TWAMLSM                                            
*                                                                               
VALPX    B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO SET REQUEST SCREEN FROM LISTFLD                       
SETFLD   NTR1                                                                   
         CLI   LISTYPE,0           IF LISTYPE                                   
         BE    SETFLDX                                                          
         OC    LISTFLD,LISTFLD     AND LIST FIELD SET                           
         BZ    SETFLDX                                                          
         CLI   LISTYPE,C'P'                                                     
         BNE   SETFLD5                                                          
         XC    FINPRD,FINPRD       RESET PRODUCT FIELD                          
         MVC   FINPRD(L'LISTFLD),LISTFLD                                        
         B     SETFLD15                                                         
*                                                                               
SETFLD5  CLI   LISTYPE,C'E'                                                     
         BNE   SETFLD10                                                         
         XC    FINEST,FINEST       RESET ESTIMATE FIELD                         
         MVC   FINEST(L'LISTFLD),LISTFLD                                        
         B     SETFLD15                                                         
*                                                                               
SETFLD10 XC    FINMOS,FINMOS       RESET MOS FIELD                              
         MVC   FINMOS(L'LISTFLD),LISTFLD                                        
*                                                                               
SETFLD15 MVI   LISTYPE,0          RESET THE VALUES                              
         XC    LISTFLD,LISTFLD    SO - THEY DON'T POP UP                        
SETFLDX  B     EXIT                                                             
         EJECT                                                                  
*======================================*                                        
* FSTSCR - DOES ALL INITIALIZATION AND *                                        
*        - CALLS SPOTIO FOR RECORDS    *                                        
*======================================*                                        
*                                                                               
FSTSCR   LA    R2,APRECKEY                                                      
         USING APRECD,R2                                                        
*                                                                               
         BAS   RE,SETLLBL         SET LIST LABEL                                
         BAS   RE,TSARINT         INITIALIZE TSAR                               
         BNE   FSTSCRX            ALLOCATION FAILURE                            
*                                                                               
         GOTO1 AINTSPBK           INITIALIZE SPOTBLOCK                          
         GOTO1 ACLRBLK,TOTLBL     CLEAR TOTAL BLOCK                             
         LA    R1,IOHOOK          SET SPOTIO HOOK HERE                          
         ST    R1,SBIOHOOK                                                      
         MVI   PRTLN,C'Y'         PRINT LINE ON THIS SCREEN(DEFAULT)            
         MVI   TOTREC,C'N'        TOTAL ALL LINE NOT YET PRINTED                
         CLI   LISTYPE,C'M'       FOR MARKET LIST                               
         BNE   FSTSCR8            READ ALL RECORDS ON THE FLY                   
         MVC   REQMKT,SBQMKT      SAVE REQUEST MARKET                           
         MVC   STRTMKT,SBQMKT     START STRTMKT AT REQ MKT TO BEGIN             
         MVI   TOTLN,C'N'                                                       
         B     FSTSCRX                                                          
*                                                                               
FSTSCR8  CLI   LISTYPE,C'P'       IF LISTING PRODUCTS                           
         BNE   FSTSCR50                                                         
         CLI   POLEST,C'Y'        WITH A NON POOL ESTIMATE                      
         BE    FSTSCR50                                                         
         BAS   RE,NONPPRD         CALL SPOTIO FOR EACH PRODUCT IN LIST          
         B     FSTSCR60                                                         
*                                                                               
FSTSCR50 XC    TEMPSTA,TEMPSTA    FOR POL PRD, STA, OR MKT/STA LIST             
         MVI   TBPRD1,0                                                         
         MVI   TBEST,0                                                          
         BAS   RE,SPOTCALL        SPOTIO CALLED ONCE ONLY                       
*                                                                               
FSTSCR60 GOTO1 ATSARTAD                                                         
FSTSCRX  B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET LIST LABELS FOR REQUEST                           
SETLLBL  NTR1                                                                   
         MVC   FLTSLST(8),=CL8'ESTIMATE'                                        
         CLI   LISTYPE,C'E'                                                     
         BE    SETLLBL5                                                         
*                                                                               
         CLI   APMOSFLG+1,C'L'                                                  
         BNE   SETLLBL2                                                         
*                                                                               
         CLI   APMOSFLG,C'S'                                                    
         BNE   *+14                                                             
         MVC   FLTSLST(8),=CL8'STATION'                                         
         B     SETLLBL5                                                         
         CLI   APMOSFLG,C'M'                                                    
         BNE   *+14                                                             
         MVC   FLTSLST(8),=CL8'MKT/STA'                                         
         B     SETLLBL5                                                         
*                                                                               
SETLLBL2 MVC   FLTSLST(8),=CL8'PRODUCT'                                         
*                                                                               
SETLLBL5 MVC   FLTSLST+53(17),SPACES                                            
         TM    INOIND2,INOIBIL     SHOW BILL & BILLABLE                         
         BNO   *+10                                                             
         MVC   FLTSLST+53(17),=CL17'BILLED   BILLABLE'                          
         OI    FLTSLSTH+1,X'20'   PROTECT IT AND                                
         OI    FLTSLSTH+6,X'80'   TRANSMIT IT                                   
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CALL SPOTIO FOR EACH PRODUCT                          
*              NO POL INFORMATION                                               
NONPPRD  NTR1                                                                   
         LA    R3,PRDLIST          R3=A(PRODUCT LIST)                           
NONPPR10 OC    0(4,R3),0(R3)       IF NOT END OF LIST                           
         BZ    EXIT                                                             
         BAS   RE,OPEST            AND ESTIMATE FOR PRODUCT EXISTS              
         BNE   NONPPR25                                                         
         MVC   SBQPRD,0(R3)        CALL SPOTIO WITH PRODUCT                     
         MVC   SBPRD,0(R3)                                                      
         MVC   SBQBPRD,3(R3)                                                    
         MVI   SBEUNALL,C'N'                                                    
         MVC   SBEPRD,SBQBPRD                                                   
         MVI   TBPRD1,0                                                         
         GOTO1 ACLRBLK,BLKLBL          CLEAR BLOCK                              
         GOTO1 VSPOTIO,APPARM,SBLOCK   ** CALL SPOTIO **                        
         XC    SBASVETB,SBASVETB       SPOTIO MUST BUILD EACH TIME              
         CLI   ADDFLG,C'Y'                                                      
         BNE   NONPPR25                                                         
         GOTO1 ATSARCUM                                                         
*                                                                               
NONPPR25 LA    R3,4(R3)           GET NEXT PRODUCT                              
         B     NONPPR10           LOOP                                          
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO CHECK IF ESTIMATE OPENED FOR THIS PRODUCT             
OPEST    NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,0(R3)                                                    
         MVC   EKEYEST,BEST                                                     
         CLI   BEST,0                                                           
         BNE   *+8                                                              
         MVI   EKEYEST,1                                                        
         GOTO1 AIO,DIRHI          CONDITION CODE SET                            
         CLC   IOKEY(6),IOKEYSAV  SAME MED,CLI,PRD                              
         BNE   NO                                                               
         CLI   BEST,0                                                           
         BE    OPEST5                                                           
         CLC   IOKEY+6(1),BEST                                                  
         BE    YES                                                              
         B     NO                                                               
*                                                                               
OPEST5   CLI   IOKEY+6,0          IF ALL ESTIMATE REQUEST                       
         BE    NO                 SOME ESTIMATE MUST EXIST                      
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              LAST SCREEN HOOK                                                 
         SPACE                                                                  
LSTSCR   CLI   TSTART,C'Y'                                                      
         BNE   LSTSCRX                                                          
         MVI   TSTART,C'N'        START LIST FROM THE TOP                       
         MVI   APMODE,APMPFKS                                                   
         MVC   SCPFKEY,ACPFFST                                                  
LSTSCRX  B     EXIT                                                             
         SPACE 2                                                                
*              ROUTIEN TO CALL SPOTIO (CLEARS BLKS FIRST)                       
SPOTCALL NTR1                                                                   
         GOTO1 ACLRBLK,BLKLBL        CLEAR BLOCK                                
         GOTO1 VSPOTIO,APPARM,SBLOCK   ** CALL SPOTIO **                        
         CLI   ADDFLG,C'Y'                                                      
         BNE   SPOTCX                                                           
         GOTO1 ATSARCUM           NEED TO ADD LAST STATION                      
SPOTCX   XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO INITIALIZE TSAR                                       
TSARINT  NTR1                                                                   
         L     R1,ATSARBLK        A(TSAR BLOCK)                                 
         USING TSARD,R1                                                         
         MVC   TSABUF,ATIA        A(6K CORE BUFFER)                             
         MVC   TSACOM,ACOM        A(COMFACS)                                    
         MVI   TSNBUF,1           # OF CORE BUFERS                              
         LA    RE,L'TKEY                                                        
         STC   RE,TSKEYL          KEY LENGTH                                    
         LA    RE,TRECL                                                         
         STCM  RE,3,TSRECL        MAXIMUM RECORD LENGTH                         
         TM    TIND,TINIT         IS TSAR INITIALIZED?                          
         BNO   TSARI5                                                           
*                                 YES - JUST RESTORE                            
         MVI   TSACTN,TSARES                                                    
         MVC   TSPAGL,TSAVE                                                     
         MVC   TSPAGN,TSAVE+1                                                   
         MVC   TSINDS,TSAVE+2                                                   
         OI    TSINDS,TSIREUSE                                                  
         B     TSARI10                                                          
*                                                                               
TSARI5   MVI   TSACTN,TSAINI      INITIALIZE                                    
         MVI   TSINDS,TSIALLOC    ALLOCATE FROM TEMPEST                         
         MVI   TSPAGN,12          12 GOOD ENOUGH WITH 14K PAGES                 
         OI    TSINDS,TSIXTTWA    USE 14K PAGES                                 
*                                                                               
TSARI10  GOTO1 VTSAR                                                            
         BE    TSARI15                                                          
         CLI   TSACTN,TSAINI      TRY TO INITIALIZED?                           
         BE    *+6                                                              
         DC    H'0'               UNSUCCESSFUL RESTORE                          
         MVI   TIND2,0            CLEAR OUT INDICATORS                          
         MVI   TIND,0                                                           
         MVC   FVMSGNO,=X'041F'   ALLOCATION FAILURE                            
         XC    FINREC,FINREC                                                    
         OI    FINRECH+6,X'80'    TRANSMIT & PROTECT                            
         OI    FINRECH+1,X'20'                                                  
         XC    FINACT,FINACT                                                    
         OI    FINACTH+6,X'80'    TRANSMIT & PROTECT                            
         OI    FINACTH+1,X'20'                                                  
         LA    RE,FINSRVH                                                       
         STCM  RE,15,APCURSOR     SET CURSOR TO SERVICE FIELD                   
         B     NO                 FORCES RECONNECT                              
*                                                                               
TSARI15  MVC   TSAVE(1),TSPAGL    SAVE LOW TSAR PAGE NUMBER                     
         MVC   TSAVE+1(1),TSPAGN  SAVE NUMBER OF PAGES ALLOCATED                
         MVC   TSAVE+2(1),TSINDS  SAVE TEMPEST INDICATOR                        
         NI    TSAVE+2,TSIALLOC+TSIXTTWA                                        
         XC    TSAVE+3(2),TSAVE+3                                               
         OI    TIND,TINIT+TRES    TURN ON INIT AND RESTORE BITS                 
*                                 DELETE BUFFER OF PREVIOUS RECORDS             
TSARI20  MVI   TSACTN,TSADEL      IF ANY                                        
         MVC   TSRNUM,=H'1'                                                     
         LA    RE,BLK                                                           
         ST    RE,TSAREC                                                        
         GOTO1 VTSAR                                                            
         BE    TSARI20                                                          
         TM    TSERRS,TSERNF                                                    
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   RECSRD,C'N'        NO RECORDS READ                               
         B     YES                                                              
         EJECT                                                                  
*================*                                                              
* GETSEL  RECORD *                                                              
*================*                                                              
*                                                                               
GETSEL   LA    R2,APRECKEY                                                      
         USING APRECD,R2                                                        
         CLI   LISTYPE,C'M'       MARKET LIST HAS ITS                           
         BE    GETML              OWN GETSEL                                    
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET      NEXT RECORD                                   
         MVC   TSRNUM,TSAVE+3     RECORD NUMBER                                 
         XC    MYIO(256),MYIO                                                   
         LA    RE,MYIO            GET TSAR RECORD                               
         ST    RE,TSAREC          INTO MYIO                                     
*                                                                               
         CLI   APINDS,APILFLST    FIRST TIME AROUND?                            
         BNE   GETSEL20                                                         
         XC    TEMPMKT,TEMPMKT     CLEAR OUT MARKET (FOR DISPLAY PURP)          
         SR    RE,RE               YES - START WITH 1ST RECORD                  
         B     GETSEL40                                                         
*                                                                               
GETSEL20 LH    RE,TSRNUM          LINE NUMBER                                   
         CLI   APINDS,APILNLST    FIRST LINE OF NOT FIRST SCREEN                
         BNE   GETSEL40                                                         
         CLI   APPFKEY,0           YES-ANY PFKEY PRESSED?                       
         BE    GETSEL50                 NO - GET RECORD (DON'T BUMP)            
         CLI   APPFKEY,6          TOP PRESSED?                                  
         BNE   GETSEL30                                                         
         XC    TEMPMKT,TEMPMKT     CLEAR OUT MARKET (FOR DISPLAY PURP)          
         SR    RE,RE              YES - START WITH 1ST RECORD                   
         B     GETSEL40                                                         
*                                                                               
GETSEL30 LA    RF,NLINES          # OF LINES PER SCREEN FULL                    
         CLI   APPFKEY,7          PREVIOUS SCREEN?                              
         BNE   GETSEL50            NO - NEXT SCREEN,NUMBER ALREADY SET          
         SLA   RF,1               # OF LINES ON SCREEN X 2                      
         SR    RE,RF               YES - SUBTRACT SCREEN FULL -                 
         BP    GETSEL45           IF LESS THAN 0                                
         SR    RE,RE              DEFAULT TO TOP                                
*                                                                               
GETSEL40 LA    RE,1(RE)           GET NEXT RECORD                               
GETSEL45 STH   RE,TSRNUM                                                        
         MVC   TSAVE+3(2),TSRNUM                                                
*                                                                               
GETSEL50 GOTO1 VTSAR              GET THE RECORD                                
         BE    GETSEL65                                                         
         TM    TSERRS,TSEEOF      EOF ONLY VALID ERROR                          
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   APMODE,APMEOFS                                                   
         B     GETSELX                                                          
*                                                                               
GETSEL65 CLI   LISTYPE,C'P'                                                     
         BNE   GETSEL70                                                         
         CLC   =C'POL',MYIO       DON'T DISPLAY POL                             
         BNE   GETSEL70           ON A LIST SCREEN                              
         B     GETSEL75                                                         
GETSEL70 OC    MYIO(L'BLKLBL),MYIO                                              
         BNZ   GETSEL80                                                         
GETSEL75 LH    RE,TSRNUM                                                        
         LA    RE,1(RE)                                                         
         B     GETSEL45                                                         
GETSEL80 BAS   RE,SETDTL          SET DETAIL KEY VALUE                          
*                                                                               
GETSELX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================*                                                              
* GETML - FOR MKT*                                                              
*      LIST ONLY *                                                              
*================*                                                              
*                                                                               
GETML    CLI   TOTREC,C'Y'                                                      
         BE    GETML60            JUST PRINTED ALL TOTAL - ALL DONE             
         LA    R2,APRECKEY                                                      
         USING APRECD,R2                                                        
         CLI   APINDS,APILFLST    FIRST TIME AROUND?                            
         BNE   GETML5                                                           
         XC    TEMPMKT,TEMPMKT    CLEAR OUT MARKET (FOR DISPLAY PURP)           
         MVI   DISPCNT,0          COUNT OF LINES DISPLAYED                      
         BAS   RE,RDMOS                                                         
         SR    R3,R3              START WITH 1ST RECORD                         
*                                                                               
GETML5   CLI   APINDS,APILNLST    FIRST LINE OF NOT FIRST SCREEN                
         BNE   GETML8                                                           
         MVI   PRTLN,C'Y'                                                       
         MVI   TOTLN,C'N'                                                       
         MVI   DISPCNT,0                                                        
*                                                                               
GETML8   CLI   PRTLN,C'N'         PRINT ON SCREEN?                              
         BE    GETMLX             NO - WHOLE MARKET WILL NOT FIT                
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET      NEXT RECORD                                   
         MVC   TSRNUM,TSAVE+3     RECORD NUMBER                                 
         XC    MYIO(256),MYIO                                                   
         LA    RF,MYIO            GET TSAR RECORD                               
         ST    RF,TSAREC          INTO MYIO                                     
         CLI   TOTLN,C'Y'                                                       
         BNE   GETML10                                                          
         MVI   TOTLN,C'N'                                                       
         BAS   RE,CHKFIT                                                        
         BNE   GETMLX                                                           
         LH    R3,TSRNUM                                                        
*                                                                               
GETML10  CLI   APINDS,APILFLST    FIRST TIME AROUND?                            
         BE    GETML40            YES- JUST GET THE RECORD                      
         LH    R3,TSRNUM          LINE NUMBER                                   
         CLI   APINDS,APILNLST    FIRST LINE OF NOT FIRST SCREEN                
         BNE   GETML40                                                          
         CLI   APPFKEY,0           YES-ANY PFKEY PRESSED?                       
         BE    GETML50                 NO - GET RECORD (DON'T BUMP)             
         CLI   APPFKEY,6          TOP PRESSED?                                  
         BNE   GETML30                                                          
         XC    TEMPMKT,TEMPMKT     CLEAR OUT MARKET (FOR DISPLAY PURP)          
         SR    R3,R3              YES - START WITH 1ST RECORD                   
         B     GETML40                                                          
*                                                                               
GETML30  LA    RF,NLINES          # OF LINES PER SCREEN FULL                    
         CLI   APPFKEY,7          PREVIOUS SCREEN?                              
         BNE   GETML50            NO - NEXT SCREEN,NUMBER ALREADY SET           
         SLA   RF,1               # OF LINES ON SCREEN X 2                      
         SR    R3,RF               YES - SUBTRACT SCREEN FULL -                 
         BM    GETML35            IF LESS THAN 0                                
         BZ    GETML35            OR ZERO = DEFAULT TO TOP                      
*                                 IF <> 0 - CHECK IF START OF MKT               
         BCTR  R3,0               BY GETTING PREV RECORD                        
         LTR   R3,R3                 IF ZERO - START AT TOP                     
         BZ    GETML35                                                          
GETML33  STH   R3,TSRNUM                                                        
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MYIO+4(5),=5C'9'   AND SEEING IF TOTAL RECORD                    
         BE    GETML40            YES - THEN NEXT IS START OF NEW MKT           
         LA    R3,1(R3)           NO - MUST GO FOWARD IT GET TO NEW MKT         
         B     GETML33                                                          
*                                                                               
GETML35  SR    R3,R3              DEFAULT TO TOP                                
*                                                                               
GETML40  CLI   MKTLIN,C'Y'        THIS MARKET NAME LINE                         
         BNE   GETML42            NO - GO ON                                    
         MVI   MKTLIN,C'N'        YES - RESET                                   
         B     GETML45                 AND DON'T INCREMENT COUNTER              
GETML42  LA    R3,1(R3)           GET NEXT RECORD                               
GETML45  STH   R3,TSRNUM                                                        
         MVC   TSAVE+3(2),TSRNUM                                                
*                                                                               
GETML50  GOTO1 VTSAR              GET THE RECORD                                
         BE    GETML55                                                          
         TM    TSERRS,TSEEOF                                                    
         BO    GETML60            NO DATA AT ALL                                
         DC    H'0'                                                             
GETML55  CLC   MYIO(4),=4X'FF'    LAST REC IN BUFF SO FAR?                      
         BNE   GETML65                                                          
         CLI   RECSRD,C'Y'        YES -ALL RECORDS IN?                          
         BE    GETML60            YES                                           
         BAS   RE,RDMOS                                                         
         MVI   TSACTN,TSAGET      RESET - ACTION                                
         XC    MYIO(256),MYIO                                                   
         LA    RF,MYIO            GET TSAR RECORD                               
         ST    RF,TSAREC          INTO MYIO                                     
         MVC   TSRNUM,TSAVE+3                                                   
         B     GETML50            TRY AGAIN                                     
GETML60  MVI   RECSRD,C'N'        RESET FOR NEXT TIME IN                        
         MVI   RECCNT,0                                                         
         MVI   APMODE,APMEOFS                                                   
         B     GETMLX                                                           
*                                                                               
GETML65  ZIC   RF,DISPCNT         INCREMENT DISPLAY COUNT                       
         LA    RF,1(RF)                                                         
         STC   RF,DISPCNT                                                       
         CLC   MYIO+4(5),=5C'9'    TOTAL LINE                                   
         BNE   GETML70                                                          
         MVC   TEMPSNUM,TSRNUM    YES - TAKE CARE OF IT                         
         MVC   TEMPDNUM,DISPCNT                                                 
         MVI   TOTLN,C'Y'                                                       
         B     GETML80            SET DETAIL KEY & GET OUT                      
*                                                                               
GETML70  OC    MYIO(L'BLKLBL),MYIO                                              
         BNZ   GETSEL80                                                         
GETML75  LH    R3,TSRNUM                                                        
         LA    R3,1(R3)                                                         
         B     GETML45                                                          
GETML80  BAS   RE,SETDTL          SET DETAIL KEY VALUE                          
*                                                                               
GETMLX   CLI   APMODE,APMEOFS                                                   
         BNE   GETMLXX                                                          
         CLI   TOTREC,C'Y'                                                      
         BE    GETMLXX                                                          
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH      READ FOR ALL TOTAL RECORD                     
         XC    MYIO(256),MYIO                                                   
         MVC   MYIO(4),=4X'FF'                                                  
         MVC   MYIO+4(5),=5C'9'                                                 
         LA    RF,MYIO                                                          
         ST    RF,TSAREC                                                        
         GOTO1 VTSAR                                                            
         BNE   GETMLXX            DOESN'T EXIST                                 
         MVI   TOTREC,C'Y'        ALL TOTAL RECORD EXISTS-GO DISPLAY IT         
         MVI   APMODE,APMGETS     RESET MODE                                    
         B     GETML70                                                          
*                                                                               
GETMLXX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*========================================*                                      
* RDMOS - FOR MKT LIST ONLY CALLS SPOTIO *                                      
*         TWICE - FIRST FOR BUYS (READS  *                                      
*         WHOLE MARKETS (AS MANY AS WILL *                                      
*         FIT ON SCREEN -- THEN READS    *                                      
*         BILLS FOR ONLY THOSE MARKETS   *                                      
*========================================*                                      
*                                                                               
RDMOS    NTR1                                                                   
         CLI   RECSRD,C'Y'                                                      
         BE    RDMOSX                                                           
         LA    R0,2                                                             
         MVI   SBQSKIP,0                                                        
         OI    SBQSKIP,SBQSKGL    DON'T READ GOALS                              
         OI    SBQSKIP,SBQSKBIL   DON'T READ BILLS                              
         OI    SBQSKIP,SBQSKMED                                                 
         MVC   SBQMKT,STRTMKT     START MKT                                     
         XC    SBQBMKTX,SBQBMKTX  END MKT                                       
RDMOS5   GOTO1 ACLRBLK,BLKLBL     ONLY WANT BUYS FIRST                          
         GOTO1 ACLRBLK,TOTLBL                                                   
         XC    TEMPSTA,TEMPSTA    CLEAR VALUES                                  
         XC    TEMPMKT,TEMPMKT                                                  
         MVI   RECCNT,0                                                         
         GOTO1 VSPOTIO,APPARM,SBLOCK                                            
         OC    SBASVETB,SBASVETB   IF A(ESTTAB SET BY SPOTIO)                   
         BZ    RDMOS10                                                          
         L     R1,SBASVETB                                                      
         LA    RE,ESTTB                                                         
         MVC   0(256,RE),0(R1)     SAVE IT IN MY ESTIMATE TABLE                 
         ST    RE,SBASVETB         AND HAVE SPOTIO LOOK THERE                   
RDMOS10  GOTO1 ATSARCUM                                                         
         GOTO1 ATSARTCM                                                         
         TM    SBQSKIP,SBQSKBUY   IF JUST READ BUYS                             
         BO    RDMOS20            SET RECSRD                                    
         MVI   RECSRD,C'N'        NOT ALL RECORDS IN BUFFER                     
         CLI   SBMODE,SBSTOP                                                    
         BE    *+8                                                              
         MVI   RECSRD,C'Y'        ALL RECORDS READ IN                           
*                                                                               
RDMOS20  XC    TEMPMKT,TEMPMKT                                                  
         MVC   SBQMKT,STRTMKT     START MARKET                                  
         MVC   SBQBMKTX,ENDMKT    END MARKET                                    
         MVC   STRTMKT,NXTMKT     START MKT FOR NEXT TRANSACTION                
*********TM    INOIND2,INOIBIL                                                  
*********BNO   RDMOSX             DON'T READ BILLS                              
         MVI   SBQSKIP,0                                                        
         OI    SBQSKIP,SBQSKBUY   DON'T READ BUYS                               
         OI    SBQSKIP,SBQSKGL    DON'T READ GOAL                               
         OI    SBQSKIP,SBQSKMED                                                 
         BCT   R0,RDMOS5                                                        
*                                                                               
RDMOSX   XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
* CHKFIT - CHECKS IF NEXT WHOLE MARKET WILL FIT ON CURRENT SCRN *               
*===============================================================*               
*                                                                               
CHKFIT   NTR1                                                                   
         ZIC   RF,DISPCNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,DISPCNT         ONE JUST FOR MKT NAME LINE                    
*                                                                               
CHKFIT1  LH    R3,TSRNUM                                                        
         LA    R3,1(R3)                                                         
         STH   R3,TSRNUM                                                        
         GOTO1 VTSAR                                                            
         BE    CHKFIT20                                                         
         CLI   RECSRD,C'Y'        ALL RECORDS IN?                               
         BE    CHKFIT10           YES                                           
         BAS   RE,RDMOS                                                         
         MVI   TSACTN,TSAGET      RESET - ACTION                                
         XC    MYIO(256),MYIO                                                   
         LA    RF,MYIO            GET TSAR RECORD                               
         ST    RF,TSAREC          INTO MYIO                                     
         MVC   TSRNUM,TEMPSNUM                                                  
         B     CHKFIT1            TRY AGAIN                                     
CHKFIT10 MVI   RECSRD,C'N'        RESET FOR NEXT TIME IN                        
         MVI   RECCNT,0                                                         
         MVI   APMODE,APMEOFS                                                   
         MVC   TSRNUM,TEMPSNUM                                                  
         B     NO                                                               
*                                                                               
CHKFIT20 ZIC   RF,DISPCNT         INCREMENT DISPLAY COUNT                       
         LA    RF,1(RF)                                                         
         STC   RF,DISPCNT                                                       
         CLC   MYIO+4(5),=5C'9'                                                 
         BNE   CHKFIT1                                                          
         LA    RE,NLINES          MAX NUMBER OF LINES ON SCREEN                 
         CR    RF,RE                                                            
         BL    CHKFIT30           GET THIS WHOLE MARKET                         
         MVC   TSRNUM,TEMPSNUM    REACHED SCREEN MAX - RESTORE TSARNUM          
         LH    RE,TSRNUM                                                        
         LA    RE,1(RE)                                                         
         STH   RE,TSRNUM                                                        
         MVC   TSAVE+3(2),TSRNUM                                                
         XC    DISPCNT,DISPCNT    AND GET OUT                                   
         MVI   PRTLN,C'N'          NO MORE ON THIS SCREEN                       
         B     NO                                                               
*                                                                               
CHKFIT30 MVC   TSRNUM,TEMPSNUM    GET THIS WHOLE MARKET                         
         MVC   DISPCNT,TEMPDNUM                                                 
         B     YES                                                              
         EJECT                                                                  
*=======================*                                                       
* SETDTL  - SETS THE    *                                                       
*          DETAIL KEY   *                                                       
*=======================*                                                       
*                                                                               
SETDTL   NTR1                                                                   
         LA    R3,APRECKEY                                                      
         USING APRECD,R3                                                        
*                                                                               
         CLI   LISTYPE,C'P'       FOR PRODUCT LIST                              
         BNE   *+12                                                             
         BAS   RE,SETDPRD         SET DETAIL KEY TO PRODUCT                     
         B     SETDX                                                            
*                                                                               
         CLI   LISTYPE,C'E'       FOR ESTIMATE LIST                             
         BNE   *+12                                                             
         BAS   RE,SETDEST         SET DETAIL KEY TO ESTIMATE                    
         B     SETDX                                                            
*                                                                               
         CLI   LISTYPE,C'S'       FOR STATION LIST                              
         BNE   *+12                                                             
         BAS   RE,SETDSTA          SET DETAIL KEY TO STATION                    
         B     SETDX                                                            
*                                                                               
         BAS   RE,SETDMKT          SET DETAIL KEY TO MARKET                     
*                                                                               
SETDX    XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET PRODUCT IN DETAIL KEY                             
SETDPRD  NTR1                                                                   
         MVI   APFLAG,1           FIRST PRODUCT                                 
         XC    APPRD2,APPRD2      CLEAR OUT SECOND PRODUCT                      
         MVI   APPRDB2,0          IN CASE THERE ISN'T ONE                       
         CLC   =C'FZZZ',MYIO                                                    
         BNE   SETDPR5                                                          
         MVC   APPRD,=C'ALL'                                                    
         MVI   APPRDB1,0                                                        
         B     SETDPRX                                                          
SETDPR5  CLC   =C'CUNA',MYIO                                                    
         BNE   SETDPR10                                                         
         MVC   APPRD,=C'UNA'                                                    
         MVI   APPRDB1,0                                                        
         B     SETDPRX                                                          
*                                                                               
SETDPR10 MVC   TEMPPRD,MYIO+1                                                   
*                                                                               
SETDPR15 LA    RE,PRDLIST                                                       
SETDPR20 OC    0(4,RE),0(RE)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(3,RE),TEMPPRD    FIND FIRST PRODUCT                            
         BE    SETDPR25                                                         
         LA    RE,4(RE)                                                         
         B     SETDPR20                                                         
*                                                                               
SETDPR25 CLI   APFLAG,1                                                         
         BNE   SETDPR30                                                         
         MVC   APPRD,0(RE)                                                      
         MVC   APPRDB1,3(RE)                                                    
         OC    MYIO+4(3),MYIO+4      SECOND PRODUCT?                            
         BZ    SETDPRX                                                          
         MVI   APFLAG,2           LOOK FOR SECOND PRODUCT                       
         MVC   TEMPPRD,MYIO+4                                                   
         B     SETDPR15                                                         
*                                                                               
SETDPR30 MVC   APPRD2,0(RE)                                                     
         MVC   APPRDB2,3(RE)                                                    
SETDPRX  B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET ESTIMATE IN DETAIL KEY                            
SETDEST  NTR1                                                                   
         CLI   MYIO,C'F'                                                        
         BNE   SETDES10                                                         
         CLI   MYIO+1,X'FF'                                                     
         BNE   SETDES10                                                         
         MVI   APESTB1,0          TOTAL LINE                                    
         MVC   APEST,=C'ALL'                                                    
         OC    QSEPFLT,QSEPFLT                                                  
         BL    *+10                                                             
         MVC   APEST,=C'NO '                                                    
         B     SETDESTX                                                         
*                                                                               
SETDES10 MVC   APESTB1,MYIO+1      MOVE IN INDIVIDUAL ESTIMATE                  
         ZIC   R1,APESTB1                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APEST,APDUB                                                      
SETDESTX B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO SET STATION TO DETAIL KEY                             
SETDSTA  NTR1                                                                   
         XC    APSTA,APSTA                                                      
         CLC   MYIO(9),=9C'9'                                                   
         BNE   *+14                                                             
         MVC   APSTA(3),=C'ALL'                                                 
         B     *+10                                                             
         MVC   APSTA,MYIO                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET MARKET IN DETAIL KEY                              
SETDMKT  NTR1                                                                   
         MVC   APSTA,MYIO+4                                                     
         CLC   TEMPMKT,MYIO       NEW MKT                                       
         BE    SETDMK5                                                          
         CLC   MYIO(4),=4X'FF'                                                  
         BE    *+12                                                             
         MVI   MKTLIN,C'Y'        INCIDATE NEW MKT                              
         B     SETDMK10                                                         
*                                                                               
         XC    APSTA,APSTA        ALL MKT/STA TOTAL                             
         MVC   APSTA(3),=C'ALL'                                                 
         MVC   APMKT(2),=C'AL'                                                  
         B     SETDMKTX                                                         
*                                                                               
SETDMK5  CLC   MYIO+4(5),=5C'9'                                                 
         BNE   SETDMK20                                                         
SETDMK10 XC    APSTA,APSTA                                                      
         MVC   APSTA(3),=C'ALL'   MKT TOTAL                                     
*                                                                               
SETDMK20 PACK  APDUB,MYIO(4)                                                    
         CVB   R0,APDUB                                                         
         STCM  R0,3,APMKT         STATION DETAIL LINE                           
*                                                                               
SETDMKTX B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=========================================================*                     
* DISPLAY ONE RECORD INPUT APPARM(4) =A(TWA DISPLAY LINE) *                     
*=========================================================*                     
DISSEL   CLI   PRTLN,C'Y'         PRINT ON SCREEN?                              
         BNE   DISSELX            NO - WHOLE MARKET WILL NOT FIT                
         CLI   LISTYPE,C'M'                                                     
         BNE   DISSEL4                                                          
         CLC   MYIO(4),=4X'FF'                                                  
         BNE   DISSEL4                                                          
         CLI   TOTREC,C'Y'        PRINTING TOTAL LINE NOW?                      
         BNE   DISSELX                                                          
*                                                                               
DISSEL4  LA    R3,MYIO+L'BLKLBL   POINT TO AMTS                                 
         USING BLKD,R3                                                          
         L     R2,APPARM          A(TWA DISPLAY LINE)                           
         USING LDISD,R2                                                         
*                                                                               
         CLI   MKTLIN,C'Y'                                                      
         BNE   DISSEL10                                                         
         MVC   TEMPMKT,MYIO       SET NEW MKT                                   
         MVC   LLABEL(L'TEMPMKT),TEMPMKT                                        
         BAS   RE,GETMKTN                                                       
         MVC   LLABEL+5(L'MKTNM),APWORK                                         
         B     DISSELX                                                          
*                                                                               
DISSEL10 LA    R0,3                                                             
         LA    R1,AMOUNTS                                                       
DISSEL15 ZAP   0(L'AMOUNTS,R1),BLKTOT                                           
         LA    R1,L'AMOUNTS(R1)                                                 
         ZIC   RF,NUMDTS                                                        
         MH    RF,=H'6'                                                         
         AH    RF,=H'18'          RF= ROW LENGTH                                
         AR    R3,RF                                                            
         BCT   R0,DISSEL15                                                      
*                                                                               
         BAS   RE,CLCUNPD         CALCULATE UNPAID AMOUNT                       
         BAS   RE,CLCUNBL         CALCULATE UNBILLED AMOUNT                     
         BAS   RE,CLCPCT          CALCULATE % PAID                              
         BAS   RE,DOLABEL         DISPLAY LABEL                                 
*                                                                               
         EDIT  PERPDAMT,(5,LPERPD),1,DUB=APDUB,WRK=APWORK                       
         SRP   ORDAMT,64-2,5      DIVIDE BY 100                                 
         EDIT  ORDAMT,(10,LORD),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOAT=-         
         SRP   PAIDAMT,64-2,5     DIVIDE BY 100                                 
         EDIT  PAIDAMT,(10,LPAID),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOATX        
               =-                                                               
         EDIT  UNPDAMT,(10,LUNPD),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOATX        
               =-                                                               
         SRP   BILLAMT,64-2,5     DIVIDE BY 100                                 
         EDIT  BILLAMT,(10,LBILL),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOATX        
               =-                                                               
         EDIT  UNBLAMT,(10,LBILBL),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOAX        
               T=-                                                              
DISSELX  B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CALC  UNPAID  ( RND(ORD)-RND(PAID) )                  
CLCUNPD  NTR1                                                                   
         LA    RF,APWORK                                                        
         ZAP   0(L'ORDAMT,RF),ORDAMT                                            
         SRP   0(L'ORDAMT,RF),64-2,5                                            
         ZAP   UNPDAMT,0(L'ORDAMT,RF)                                           
         CP    PAIDAMT,=P'0'                                                    
         BE    CLCUNPDX                                                         
         ZAP   0(L'PAIDAMT,RF),PAIDAMT                                          
         SRP   0(L'PAIDAMT,RF),64-2,5                                           
         SP    UNPDAMT,0(L'PAIDAMT,RF)                                          
CLCUNPDX B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO CALC UNBL (  RND(ORD)-RND(BILLED) )                   
CLCUNBL  NTR1                                                                   
         CLI   BPRD2,0            IF PIGGY BACK REQUESTED                       
         BNE   CLCUNBL5                                                         
         OC    INOREP,INOREP      OR IF SPECIAL REP                             
         BNZ   CLCUNBL5                                                         
         TM    INOIND2,INOIBIL    OR IF NOT ASKING FOR BILL AMTS                
         BNO   CLCUNBL5                                                         
         CLI   INOSPL,C'N'        OR IF SPL=NO                                  
         BNE   CLCUNBL8                                                         
CLCUNBL5 ZAP   UNBLAMT,=P'0'      DON'T SHOW BILLABLE                           
         ZAP   BILLAMT,=P'0'      OR BILLED                                     
         B     CLCUNBLX                                                         
*                                                                               
CLCUNBL8 LA    RF,APWORK                                                        
         ZAP   0(L'ORDAMT,RF),ORDAMT                                            
         SRP   0(L'ORDAMT,RF),64-2,5                                            
         ZAP   UNBLAMT,0(L'ORDAMT,RF)                                           
         CP    BILLAMT,=P'0'                                                    
         BE    CLCUNBLX                                                         
         ZAP   0(L'BILLAMT,RF),BILLAMT                                          
         SRP   0(L'BILLAMT,RF),64-2,5                                           
         SP    UNBLAMT,0(L'BILLAMT,RF)                                          
CLCUNBLX B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CALC %PAID ( ORD/PAID)                                
CLCPCT   NTR1                                                                   
         ZAP   PERPDAMT,ORDAMT    ZERO IT OUT                                   
         ZAP   ORDAMT,ORDAMT                                                    
         BE    CLCPCTX            CAN'T DIVIDE ZERO                             
*                                                                               
         ZAP   APWORK(12),PAIDAMT                                               
         MP    APWORK(12),=P'10000'                                             
         DP    APWORK(12),ORDAMT                                                
         ZAP   PERPDAMT,APWORK(6)                                               
         SRP   PERPDAMT,64-1,5                                                  
CLCPCTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*====================================================*                          
* DOLABEL - PRINTS KEY ON SCREEN AND SETS DETAIL KEY *                          
*====================================================*                          
*                                                                               
DOLABEL  NTR1                                                                   
         LA    R3,APRECKEY                                                      
         USING APRECD,R3                                                        
         LA    RE,LLABEL          PT TO LABEL                                   
*                                                                               
         CLI   LISTYPE,C'P'       PRODUCT LIST?                                 
         BNE   DOL20                                                            
         CLC   =C'ALL',APPRD                                                    
         BE    DOLTOT                                                           
         MVC   0(L'APPRD,RE),APPRD                                              
         CLI   APPRDB2,0          SECOND PRODUCT?                               
         BE    DOLX                                                             
         LA    RE,L'APPRD(RE)                                                   
         MVI   0(RE),C'-'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(L'APPRD2,RE),APPRD2                                            
         B     DOLX                                                             
*                                                                               
DOL20    CLI   LISTYPE,C'E'       ESTIMATE LIST?                                
         BNE   DOL30                                                            
         CLC   =C'ALL',APEST                                                    
         BE    DOLTOT                                                           
         CLC   =C'NO',APEST                                                     
         BE    DOLTOT                                                           
         MVC   0(L'APEST,RE),APEST                                              
         B     DOLX                                                             
*                                                                               
DOL30    CLI   LISTYPE,C'S'       STATION LIST?                                 
         BNE   DOL40                                                            
         CLC   =C'ALL',APSTA      TOTAL LINE                                    
         BE    DOLTOT                                                           
         MVC   0(L'APSTA,RE),APSTA                                              
         CLI   APSTA,C'0'                                                       
         BL    *+8                                                              
         MVI   4(RE),C'/'         INDICATE CABLE STATION                        
         CLI   AGYPRF7,C'C'       IF CANADIAN AGENCY                            
         BNE   DOLX                                                             
         CLI   4(RE),C'N'         IF NETWORK SHOW IT                            
         BE    DOLX                                                             
         CLI   QMED,C'R'          OR IF RADIO - LEAVE IT                        
         BE    DOLX                                                             
         MVI   4(RE),C' '         OTHERWISE LEAVE IT BLANK                      
         B     DOLX                                                             
*                                                                               
DOL40    CLC   =C'ALL',APSTA      MUST BE MARKET LIST                           
         BE    DOLTOT                                                           
         LA    RE,LLABEL+3                                                      
         MVC   0(L'APSTA,RE),APSTA  MOVE STATION                                
         CLI   APSTA,C'0'                                                       
         BL    *+8                                                              
         MVI   4(RE),C'/'         INDICATE CABLE STATION                        
         CLI   AGYPRF7,C'C'       IF CANADIAN AGENCY                            
         BNE   DOLX                                                             
         CLI   4(RE),C'N'         IF NETWORK SHOW IT                            
         BE    DOLX                                                             
         CLI   QMED,C'R'          OR IF RADIO - LEAVE IT                        
         BE    DOLX                                                             
         MVI   4(RE),C' '         OTHERWISE LEAVE IT BLANK                      
         B     DOLX                                                             
*                                                                               
DOLTOT   XC    LLABEL,LLABEL                                                    
         MVC   LLABEL(5),=C'TOTAL'                                              
         CLC   =C'AL',APMKT                                                     
         BNE   DOLX                                                             
         MVC   LLABEL(9),=C'ALL TOTAL'                                          
         B     DOLX                                                             
*                                                                               
DOLX     XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*=========================*                                                     
* GETMKTN - GET MKT NAME  *                                                     
*=========================*                                                     
*                                                                               
GETMKTN  NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING MKTRECD,R2                                                       
         MVC   APWORK(L'MKTNAME),SPACES                                         
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,TEMPMKT                                                  
         MVC   MKTKAGY,CUAALF                                                   
         LA    R2,MYIO2                                                         
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   GETMKTNX                                                         
         MVC   APWORK(L'MKTNAME),MKTNAME                                        
GETMKTNX XIT1                                                                   
         SPACE 2                                                                
*========*                                                                      
* IOHOOK *                                                                      
*========*                                                                      
*                                                                               
IOHOOK   NTR1                                                                   
         GOTO1 ASPHOOK            ALL SPOTIO ROUTINES IN THE 00                 
         B     EXIT                                                             
         EJECT                                                                  
*=============*                                                                 
* LITERAL POOL*                                                                 
*=============*                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SPACES   DC    CL80' '                                                          
*                                                                               
* SPFINWRK                                                                      
       ++INCLUDE SPFINWRK                                                       
*                                                                               
*                                                                               
LOCALD   DSECT                                                                  
TEMPSNUM DS    H                  TEMPORARY SAVED TSAR NUMBER                   
TEMPDNUM DS    XL1                TEMPORARY SAVED DISPLAY #                     
DISPCNT  DS    XL1                DISPLAY #                                     
TEMPPRD  DS    CL3                                                              
MKTLIN   DS    C'N'               NO THIS ISN'T A MKT LINE                      
LENGTH   DS    CL1                                                              
TOTREC   DS    CL1                                                              
SVLBL    DS    CL8                                                              
*                                                                               
         DS    0D                                                               
TEMPBLK  DS    CL8                                                              
         DS    45PL6                                                            
*                                                                               
MYIO     DS    CL(TRECL)           TSAR RECORD                                  
MYIO2    DS    CL(MKTRECLQ)        MARKET RECORD                                
LOCALEND EQU   *                                                                
         EJECT                                                                  
* LISD - DSECT TO COVER LIST SCREEN                                             
*                                                                               
LDISD    DSECT                                                                  
         DS    CL8                HEADER FOR SELECT                             
         DS    CL3                SELECT FIELD                                  
         DS    CL8                HEADER FOR LIST LINE                          
LLABEL   DS    CL9                LABEL = PRD OR MKT AND/OR STA                 
         DS    CL1                                                              
LPERPD   DS    CL5                % PAID                                        
         DS    CL1                                                              
LORD     DS    CL10               ORDERED                                       
         DS    CL1                                                              
LPAID    DS    CL10               PAID                                          
         DS    CL1                                                              
LUNPD    DS    CL10               UNPAID                                        
         DS    CL1                                                              
LBILL    DS    CL10               BILLED                                        
         DS    CL1                                                              
LBILBL   DS    CL10               BILLABLE                                      
         DS    CL1                                                              
         EJECT                                                                  
* TEMPORARY INCLUDES OF FASSB,FASYSLSTD,FAFACTS,DDCOMFACS                       
*                                                                               
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FAFACTS                                                        
*                                                                               
TWAD     DSECT                                                                  
         ORG   FINTABH                                                          
       ++INCLUDE SPFINFDD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPFIN03   04/07/08'                                      
         END                                                                    

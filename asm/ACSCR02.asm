*          DATA SET ACSCR02    AT LEVEL 037 AS OF 09/01/15                      
*PHASE T60C02A,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 37 AS OF 05/09/06         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'HEADING ELEMENT MAINTAINANCE'                                   
T60C02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C02,RA,RR=RE,CLEAR=YES                                       
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
*                                                                               
         USING RESRECD,R2                                                       
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01               NO,  SET CURSOR                              
         CLI   TWALREC,RECHEAD     IF FIRST TIME IN, THEN SET CURSOR            
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,HDLCODEH         FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         EJECT ,                                                                
         SPACE 1                                                                
SCR100   DS    0H                                                               
         CLI   APMODE,APMVALK      VALKEY ?                                     
         BE    SCR200              YES,   SKIP THIS ROUTINE                     
         CLI   APMODE,APMDISK      DISKEY ?                                     
         BE    SCR200              YES,   SKIP THIS ROUTINE                     
         CLI   APACTN,ACTCPY                                                    
         BE    SCR200                                                           
*                                                                               
         L     RE,ACURSOR          CURRENT     CURSOR    LOCATION               
*                                                                               
         LA    R1,HDLCODEH                                                      
         ST    R1,ACURSOR          DEFAULT     LOCATION                         
*                                                                               
*                                  ****** TRY  FOOT LINES                       
         LA    RF,HDLTABH          POINT  PAST FOOT LINE FIELDS                 
         CR    RE,RF               CURSOR PAST FOOT LINE FIELDS ?               
         BNL   SCR200              YES,   DEFAULT   CURSOR    IS   SET          
*                                                                               
         LA    R1,HDLFL1H          ->     1ST  FOOT LINE                        
         LR    R2,R1               ->     FOOT LINE NUMBER                      
         SH    R2,=AL2(HDLFL2H-HDLFL1-L'HDLFL1)                                 
         CR    RE,R2               CURSOR BEFORE    FOOT LINES ?                
         BL    SCR120              YES,   TRY  HEADERS                          
*                                                                               
SCR110   DS    0H                                                               
*                                  LENGTH OF   FOOT LINE AREA                   
         LA    R2,HDLFL2H-HDLFL1H(,R2)                                          
         CR    RE,R2               CURSOR PAST END  OF   FIELD ?                
         BL    SCR190              NO,    SET  CURSOR                           
*                                  LENGTH OF   FOOT LINE AREA                   
         LA    R1,HDLFL2H-HDLFL1H(,R1)                                          
         B     SCR110              TRY    NEXT FOOTER                           
*                                                                               
SCR120   DS    0H                  ****** TRY  HEADERS                          
*                                  POINT  PAST HEADER    FIELDS                 
         LA    RF,HDLRH3H+L'HDLRH3H+L'HDLRH3                                    
         CR    RE,RF               CURSOR PAST HEADER    FIELDS ?               
         BNL   SCR200              YES,   DEFAULT   CURSOR    IS   SET          
*                                                                               
         LA    R1,HDLLH1H          ->     1ST  HEADER    LINE                   
         LR    R2,R1               ->     HEAD LINE NUMBER                      
         SH    R2,=AL2(HDLRH1H-HDLLH1-L'HDLLH1)                                 
         CR    RE,R2               CURSOR BEFORE    HEADERS ?                   
         BL    SCR140              YES,   TRY  CENTERS                          
*                                                                               
SCR130   DS    0H                                                               
*                                  LENGTH OF   HEADER    AREA                   
         LA    R2,HDLRH1H-HDLLH1H(,R2)                                          
         CR    RE,R2               CURSOR PAST END  OF   FIELD ?                
         BL    SCR190              YES,   SET  CURSOR                           
*                                  LENGTH OF   HEADER    AREA                   
         LA    R1,HDLRH1H-HDLLH1H(,R1)                                          
         B     SCR130              TRY    NEXT HEADER                           
*                                                                               
SCR140   DS    0H                  ****** TRY  CENTERS                          
*                                  POINT  PAST CENTER    FIELDS                 
         LA    RF,HDLCNT3H+L'HDLCNT3H+L'HDLCNT3                                 
         CR    RE,RF               CURSOR PAST CENTER    FIELDS?                
         BNL   SCR200              YES,   DEFAULT   CURSOR    IS   SET          
*                                                                               
         LA    RF,HDLCNTRH         ->     CENTER    TEXT                        
         CR    RE,RF               CURSOR BEFORE    TEXT ?                      
         BL    SCR160              YES,   TRY  TITLE                            
*                                                                               
         LA    R1,HDLCNT1H         ->     1ST  CENTER    LINE                   
         LR    R2,R1               ->     CENTER    LINE NUMBER                 
         SH    R2,=AL2(HDLCNT2H-HDLCNT1-L'HDLCNT1)                              
*                                                                               
SCR150   DS    0H                                                               
*                                  LENGTH OF   CENTER    AREA                   
         LA    R2,HDLCNT2H-HDLCNT1H(,R2)                                        
         CR    RE,R2               CURSOR PAST END  OF   FIELD ?                
         BL    SCR190              YES,   SET  CURSOR                           
*                                  LENGTH OF   CENTER    AREA                   
         LA    R1,HDLCNT2H-HDLCNT1H(,R1)                                        
         B     SCR150              TRY    NEXT CENTER    AREA                   
*                                                                               
SCR160   DS    0H                  ****** TRY  TITLE     FIELD                  
*                                  POINT  PAST TITLE     FIELD                  
         LA    RF,HDLTITLH+L'HDLTITLH+L'HDLTITL                                 
         CR    RE,RF               CURSOR PAST TITLE     FIELD ?                
         BNL   SCR200              YES,   DEFAULT   CURSOR    IS   SET          
*                                                                               
         LA    RF,HDLBGNH          ->     TITLE     TEXT                        
         CR    RE,RF               CURSOR BEFORE    TEXT ?                      
         BL    SCR200              YES,   DEFAULT   CURSOR    IS   SET          
*                                                                               
         LA    R1,HDLTITLH         ->     TITLE     LINE                        
*                                                                               
SCR190   DS    0H                                                               
         ST    R1,ACURSOR          SET  CURSOR                                  
*                                                                               
SCR200   DS    0H                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         CLI   APMODE,APMVALR      VALREC ?                                     
         BE    SCR210              YES, CONTINUE                                
         CLI   APMODE,APMDISR      DISREC ?                                     
         BNE   SCR300              NO,  SKIP                                    
*                                                                               
SCR210   DS    0H                                                               
         L     R2,ACURSOR          GET  CURSOR    LOCATION                      
*                                                                               
         CLI   APPFKEY,PFKHLP      HELP PF   KEY  ?                             
         BNE   SCR300              NO,  SKIP                                    
         MVI   SVINSSEQ,0          CLEAR     SAVE INSSEQ                        
         CLI   APACTN,ACTCHA       ARE  WE   UPDATING  THE  RECORD ?            
         BE    SCR220              YES, CONTINUE                                
         CLI   APACTN,ACTADD       ARE  WE   ADDING    A    RECORD ?            
         BNE   SCR300              NO,  SKIP                                    
*                                                                               
SCR220   DS    0H                                                               
         LA    RF,HDLTITLH         IS   CURSOR    BEFORE    ALL  LINES?         
         CR    R2,RF                                                            
         BL    SCR300              YES, SKIP                                    
         LA    RF,HDLTABH          IS   CURSOR    AFTER     ALL  LINES?         
         CR    R2,RF                                                            
         BNL   SCR300              YES, SKIP                                    
*                                                                               
         GOTO1 AFVAL,(R2)          ANY  DATA ?                                  
         BE    SCR300              YES, SKIP                                    
         LR    RE,R2               NO,  SAVE OFFSET                             
         S     RE,ATWA                  FROM START     OF   TWA                 
         ST    RE,CUR@RHLP              TO   ENABLE    PASTE                    
*                                                                               
SCR300   DS    0H                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         MVI   REPMODE,REPHEAD                                                  
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY              DISKEY                                       
         B     DISREC                                                           
         B     DELHDEL             DELETE HEADLINE ELEMENT'S ONLY               
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     CPYHDEL             COPY HEADLINE ELEMENT'S                      
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT95              NO                                           
         XC    ACURDEF,ACURDEF                                                  
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP                                                    
         CLI   TWASWPAC,ACTADD     ACTION = ADD ?                               
         BNE   EXIT90              NO,      CONTINUE                            
         MVI   TWASWPAC,ACTCHA     ACTION = CHANGE                              
         CLI   APPFKEY,PFKHLP      HELP     PF   KEY ?                          
         BNE   *+8                 NO,      SKIP                                
         MVI   TWASWPAC,ACTHLP     ACTION = HELP                                
         CLI   APPFKEY,PFKREQ      REQUEST  PF   KEY ?                          
         BNE   *+8                 NO,      SKIP                                
         MVI   TWASWPAC,ACTREQ     ACTION = REQUEST                             
         CLI   APPFKEY,PFKPRVW     PREVIEW  PF   KEY ?                          
         BNE   *+8                 NO,      SKIP                                
         MVI   TWASWPAC,ACTPRVW    ACTION = PREVIEW                             
                                                                                
EXIT90   MVC   APPARM(1),TWASWPRE  SWAP     TO   NEW  RECORD                    
         MVC   APPARM+1(1),TWASWPAC         TO   NEW  ACTION                    
         MVI   APPFKEY,0 CLEAR    PF   KEY                                      
                                                                                
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         CLI   APMODE,APMDISR      MODE IS   DISPLAY RECORD                     
         BNE   XIT                 NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY IS HAPPY                        
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
VALKEY   MVI   NEWKEY,NO           RESET TO NO                                  
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,HDLCODEH                                                   
         MVC   RESKFORM,FVIFLD     FORMAT                                       
         BE    VALKEY15                                                         
         TM    HDLCODEH+4,FVITHIS                                               
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   HDLCODE,SAVFORM                                                  
         OI    HDLCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM        SAVE FORMAT FOR PF SWITCHING             
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20            NOT FOUND                                    
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
*                                                                               
VALKEY19 MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY ?                    
         BZ    VALKEY98                                                         
         CLI   APACTN,ACTCPY       ACTION COPY?                                 
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN ON ADD TO TRICK ACTION COPY             
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         MVI   NEWKEY,YES          WE ARE ADDING A NEW RECORD                   
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
CPYHDEL  DS    0H                  ONLY COPY HEADING ELEMENTS                   
         CLI   NEWKEY,YES          ADDING A NEW RECORD?                         
         BE    *+8                                                              
         NI    APINDS,X'FF'-APIOKADD      TURN OFF TO TRICK ACTION COPY         
*                                                                               
DELHDEL  DS    0H                         ONLY DELETE HEADING ELEMENTS          
*                                                                               
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                REMOVE HEADLINE ELEMENTS              
         GOTO1 AFVAL,HDLNMEH                                                    
         BNE   VALREC03                        NAME HAS NOT BEEN INPUT          
         GOTO1 ADDNAME,APPARM,(R2),HDLNMEH     GET  FORMAT NAME                 
         BNE   VALREC99                        ON   ERROR, EXIT                 
*                                                                               
VALREC03 MVC   RESKEY,APRECKEY                                                  
         MVI   APELCODE,RHDELQ     DELETE HEADLINE ELEMENTS                     
         GOTO1 GETEL,(R2)                                                       
         BNE   VALREC09            NOTHING TO DELETE                            
*                                                                               
         USING RHDELD,R1                                                        
*                                                                               
VALREC04 CLI   RHDEL,RHDELQ        IS IT A HEADING TYPE ELEMENT                 
         BNE   VALREC06            NO, SO DON'T DELETE                          
         CLI   RHDTYPE,RHDLIST     IS IT A LIST TYPE                            
         BE    *+8                 DON'T DELETE LIST TYPE HEADLINES             
         MVI   RHDEL,X'FF'         MANUALLY MARK FOR DELETION                   
*                                                                               
VALREC06 GOTO1 NEXTEL                                                           
         BE    VALREC04                                                         
*                                                                               
VALREC08 MVI   APELCODE,X'FF'      DELETE MARKED HEADLINE ELEMENTS              
         GOTO1 DELEL,(R2)                                                       
         BNE   VALREC99                                                         
*                                                                               
         DROP  R1                                                               
*                                                                               
VALREC09 CLI   APACTN,ACTDEL       DELETE  ONLY                                 
         BE    VALREC95                                                         
         CLI   NEWKEY,YES          ADD     ONLY                                 
         BNE   VALREC10                                                         
         MVC   APREPNUM,APREPUL+1  RESET   DEFAULT LEDGER                       
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON      ERROR, EXIT                          
*                                                                               
         USING RFLELD,R3                                                        
*                                                                               
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RFLEL,RFLELQ        X'C5'   FILTER TYPE                          
         MVI   RFLLN,RFLLNQ+2                                                   
         MVI   RFLTYPE,RFLLDG      SET     LEDGER ON RECORD                     
         MVC   RFLDATA,APREPUL     DEFAULT UNIT AND LEDGER                      
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99            ON      ERROR, EXIT                          
*                                                                               
         DROP  R3                                                               
*                                                                               
VALREC10 LA    R3,HEADFLD          LIST OF FIELD HEADERS                        
*                                                                               
VALREC12 XR    R4,R4                                                            
         ICM   R4,7,0(R3)                                                       
         AR    R4,R5               R4=CURRENT FIELD HEADER                      
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC90            NO INPUT,CHECK END                           
         XC    APELEM,APELEM                                                    
         LA    RF,APELEM                                                        
*                                                                               
         USING RHDELD,RF                                                        
         MVI   RHDEL,RHDELQ        ELEMENT CODE                                 
         MVC   RHDTYPE,3(R3)       TYPE OF DATA                                 
         MVC   RHDSEQ,4(R3)        SEQUENCE                                     
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FVXLEN           INPUT DATA LENGTH                            
         EXMVC R1,RHDDATA,FVIFLD                                                
         LA    R1,RHDLNQ+1(,R1)    ADD STANDARD LENGTH TO DATA LENGTH           
         STC   R1,RHDLN            ELEMENT LENGTH                               
         CLI   FVIFLD,C'&&'        DATA DEFINITION                              
         BE    VALREC30                                                         
         OI    RHDFRM,RHDFREE      FREE FORM                                    
         B     VALREC40                                                         
*                                                                               
VALREC30 SR    R1,R1                                                            
         IC    R1,RHDLN                                                         
         SH    R1,=Y(RHDLNQ+1)                                                  
         EXOC  R1,RHDDATA,SPACES          CAPS ON                               
         OI    RHDFRM,RHDDEF              DEFINITION                            
*                                                                               
         USING DEFTABD,R6                                                       
VALREC33 GOTO1 VALDEF,(R4)         INVALID INPUT                                
         BNE   IVALKYWD            CODE DOESN'T MATCH                           
         LR    R6,R1                                                            
         CLI   3(R3),RHDFTLN       IS IT A FOOTLINE?                            
         BNE   VALREC40                                                         
         TM    DEFIND,DEFNOFUT     INVALID FOOT                                 
         BNZ   IVALFOOT                                                         
         DROP  R6                                                               
*                                                                               
VALREC40 L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC90 LA    R3,5(,R3)           R3 TO NEXT HEADFLD ENTRY                     
         CLI   4(R3),EOT           END OF HEAD LIST?                            
         BNE   VALREC12            NO, SO LOOP                                  
*                                                                               
VALREC95 GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON ERROR, EXIT                               
         L     R2,AIOAREA1                                                      
*                                                                               
VALREC96 LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADDING A RECORD?                             
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGING A RECORD?                           
         BO    VALREC97                                                         
         DC    H'0'                                                             
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    VALREC98                                                         
         TM    IOERR,IOEDUP        DELETED RECORD (DUPLICATE) ON FILE           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
*                                                                               
VALREC98 DS    0H                                                               
         CLI   APACTN,ACTDEL       DELETE ONLY                                  
         BE    DISREC                                                           
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    DISREC                                                           
         CLI   APMODE,APMNEWK      ACTION COPY?                                 
         BNE   DISREC              NO                                           
         CLI   NEWKEY,YES          IS IT REALY A NEW KEY?                       
         BNE   IVALHCPY            NO UPDATE RECORD WITH NEW HEADINGS           
         B     EXIT                ADD NEW RECORD WITH HEADING ELEMENTS         
*                                                                               
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY HEADLINE DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         HEADLINE ELEMENTS                            
         LA    R3,HEADFLD          LIST OF FIELD HEADERS                        
         TWAXC HDLNMEH,HDLTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),HDLNMEH                                      
         GOTO1 GETPER,APPARM,(R2),HDLOWNH                                       
*                                                                               
         OC    KWDPASTE,KWDPASTE   ANY  KEYWORD   FROM HELP FACILITY ?          
         BZ    DISREC10            NO,  SKIP                                    
         L     R3,CUR@RHLP         ->   ADDR RETURNED  FROM HELP                
         A     R3,ATWA             PLUS ADDR OF   TWA                           
         ST    R3,ACURSOR          SET  CURSOR    LOCATION                      
         MVI   8(R3),C'&&'         INSERT    "&"                                
*                                  INSERT    KEYWORD                            
         MVC   9(L'KWDPASTE,R3),KWDPASTE                                        
         OI    4(R3),FVITHIS       SAY  DATA INPUTED                            
         MVI   5(R3),L'KWDPASTE+1  LENGTH    OF   KEYWORD                       
         OI    6(R3),FVOXMT        TRANSMIT                                     
*                                                                               
DISREC10 XC    KWDPASTE,KWDPASTE   CLEAR     KEYWORD                            
         CLI   APPFKEY,PFKHLP      HELP PF   KEY  ?                             
         BE    DISREC15            YES, SKIP                                    
*                                  CLEAR     CURSOR    ADDR FOR  RETURN         
         XC    CUR@RHLP,CUR@RHLP             FROM HELP                          
         MVI   SVINSSEQ,0          CLEAR     SAVE INSSEQ                        
*                                                                               
DISREC15 MVI   APELCODE,RHDELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC90                                                         
*                                                                               
         USING RHDELD,R1                                                        
*                                                                               
DISREC20 LA    R3,HEADFLD          LIST OF FIELD HEADERS                        
*                                                                               
DISREC22 CLI   RHDTYPE,RHDLIST     IS IT A LIST TYPE?                           
         BE    DISREC25                                                         
         CLC   RHDTYPE,3(R3)       MATCH TYPE OF DATA                           
         BNE   DISREC30                                                         
         CLC   RHDSEQ,4(R3)        AND SEQUENCE                                 
         BNE   DISREC30                                                         
         SR    R4,R4                                                            
         ICM   R4,7,0(R3)                                                       
         AR    R4,R5               R4=CURRENT FIELD HEADER                      
         SR    RF,RF                                                            
         IC    RF,RHDLN                                                         
         LA    R0,RHDLNQ                                                        
         AH    R0,=H'1'                                                         
         SR    RF,R0               R1=DATA LENGTH FOR EX INSTR.                 
         EXMVC RF,8(R4),RHDDATA                                                 
         OI    6(R4),FVOXMT                                                     
*                                                                               
DISREC25 GOTO1 NEXTEL              R1 = A(ELEMENT FOUND)                        
         BNE   DISREC90                                                         
         B     DISREC20                                                         
*                                                                               
DISREC30 LA    R3,5(,R3)           R3=NEXT HEADFLD ENTRY                        
         CLI   4(R3),EOT                                                        
         BNE   DISREC22                                                         
         DC    H'0'                BAD ELEMENT                                  
*                                                                               
         DROP  R2                  KEEP   IT     CLEAN                          
*                                                                               
DISREC90 CLI   APMODE,APMDELR      DELETE ONLY                                  
         BE    IVALHDEL            YES,   ERROR                                 
*                                                                               
         CLI   APACTN,ACTCPY                                                    
         BE    DISREC99                                                         
*                                                                               
         MVC   APCURSOR,ACURSOR    SET    CURSOR                                
*                                                                               
DISREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ERROR MESSAGES TO DISPLAY ON TOP OF SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
IVALKYWD MVC   FVMSGNO,=AL2(FVFKYWD)         KEYWORD IS NOT RECOGNIZED          
         MVC   FVXTRA(L'APKEYWRD),APKEYWRD                                      
         LTR   R1,R1                                                            
         BZ    IVALEXIT                                                         
         MVC   FVMSGNO,=AL2(ACEKYWDI)        KEYWORD IS NOT VALID               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALFOOT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEKEY MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     IVALEXIT                                                         
         SPACE 2                                                                
IVALHDEL MVC   FVMSGNO,=AL2(ACIHDDEL)                                           
         B     IVALINFX                                                         
         SPACE 1                                                                
IVALHCPY MVC   FVMSGNO,=AL2(ACIHDCPY)                                           
         B     IVALINFX                                                         
         SPACE 1                                                                
IVALINFX MVI   FVOMTYP,GTMINF                                                   
         MVC   FVADDR,AACTHDR                                                   
         SPACE 2                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  TABLE OF HEADLINE DATA FIELDS                                      *         
*        DISPLACEMENT TO FIELD/ DATA TYPE/ SEQUENCE NUMBER            *         
***********************************************************************         
         SPACE 1                                                                
HEADFLD  DC    AL3(HDLTITLH-TWAD),AL1(RHDTITL),AL1(1)                           
         DC    AL3(HDLCNT1H-TWAD),AL1(RHDCNTR),AL1(1)                           
         DC    AL3(HDLCNT2H-TWAD),AL1(RHDCNTR),AL1(2)                           
         DC    AL3(HDLCNT3H-TWAD),AL1(RHDCNTR),AL1(3)                           
         DC    AL3(HDLLH1H-TWAD),AL1(RHDLFTH),AL1(1)                            
         DC    AL3(HDLLH2H-TWAD),AL1(RHDLFTH),AL1(2)                            
         DC    AL3(HDLLH3H-TWAD),AL1(RHDLFTH),AL1(3)                            
         DC    AL3(HDLRH1H-TWAD),AL1(RHDRHTH),AL1(1)                            
         DC    AL3(HDLRH2H-TWAD),AL1(RHDRHTH),AL1(2)                            
         DC    AL3(HDLRH3H-TWAD),AL1(RHDRHTH),AL1(3)                            
         DC    AL3(HDLFL1H-TWAD),AL1(RHDFTLN),AL1(1)                            
         DC    AL3(HDLFL2H-TWAD),AL1(RHDFTLN),AL1(2)                            
         DC    AL3(0),AL1(0),AL1(EOT)                                           
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
*ACSCRWRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR HEADLINE DEFINITIONS                                     *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRFDD                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACSCR02   09/01/15'                                      
         END                                                                    

*          DATA SET ACSCR15    AT LEVEL 041 AS OF 02/25/15                      
*PHASE T60C15A                                                                  
         TITLE 'STEREO UP/DOWN/VALIDATE MAINTENANCE'                            
***********************************************************************         
*  STEREO UPLOAD, DOWNLOAD AND VALIDATE ROUTINES, THE TABLE OF        *         
***********************************************************************         
         SPACE 1                                                                
T60C15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C15,RA,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         L     R6,=A(KEYTAB)                                                    
         A     R6,APRELO                                                        
         ST    R6,AKEYTAB                                                       
         L     R6,=A(NEWKYW)                                                    
         A     R6,APRELO                                                        
         ST    R6,AKDWTAB                                                       
         L     R6,=A(ELMTAB)                                                    
         A     R6,APRELO                                                        
         ST    R6,AELMTAB                                                       
*                                                                               
SCR10    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     PRCREC              PROCESS RECORD (DISPLAY)                     
         B     EXIT                                                             
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
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*                                                                               
EXIT30   TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
         BZ    EXIT95              NO                                           
*        NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVI   APMODE,APMSWP           SWAP                                     
         MVC   APPARM(1),TWASWPRE      SWAP RECORD                              
         MVC   APPARM+1(1),TWASWPAC    SWAP ACTION                              
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         LA    R1,STOKEYH                                                       
         ST    R1,FVADDR                                                        
         CLI   APMODE,APMDISR      MODE IS   DISPLAY RECORD                     
         BNE   XIT                 NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY IS HAPPY                        
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
VALKEY   DS    0H                                                               
         GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   VALKEY01                                                         
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     VALKEY99                                                         
*                                                                               
VALKEY01 L     R1,0(,R1)           GET   LOAD MODULE    ADDRESS                 
         LA    R1,0(,R1)           CLEAR HIGH ORDER     BYTE                    
         ST    R1,AELMBLK          SAVE  LOAD MODULE    ADDRESS                 
*                                                                               
         SR    R1,R1                                                            
         LA    RE,STOLSTH                                                       
         IC    R1,STOLSTH          GET LENGTH OF FIELD                          
         AR    RE,R1               ADD TO LAST FIELD                            
         ST    RE,AEOFSRN          SAVE END OF SCREEN ADDRESS                   
         MVI   SAVEELM,NO                                                       
*                                                                               
         LA    RF,STORECH                                                       
VALKEY02 OI    6(RF),FVOXMT                                                     
         IC    R1,0(,RF)           LENGTH OF FIELD                              
         AR    RF,R1                                                            
         C     RF,AEOFSRN                                                       
         BL    VALKEY02                                                         
*                                                                               
         TM    INOPT4,INOFMT+INOINI+INOTXT                                      
         BZ    VALKEY04                                                         
         MVC   STOCDE,SPACES                                                    
         OI    STOCDEH+6,FVOXMT                                                 
         MVC   STOKEY,SPACES                                                    
         OI    STOKEYH+6,FVOXMT                                                 
*                                                                               
         TM    INOPT4,INOFMT     SPECIAL OPTIONS                                
         BZ    *+16                                                             
         MVC   STOCDE,=C'RES'                                                   
         MVC   STOKEY(8),SAVFORM                                                
*                                                                               
         TM    INOPT4,INOINI                                                    
         BZ    *+10                                                             
         MVC   STOCDE,=C'INI'                                                   
*        MVC   STOKEY(1),SAVFORM                                                
*                                                                               
         TM    INOPT4,INOTXT                                                    
         BZ    *+16                                                             
         MVC   STOCDE,=C'TXT'                                                   
         MVC   STOKEY(8),SAVFORM                                                
*                                                                               
         MVI   STOCDEH+5,3         SET INPUT LENGTH FOR SCANNER                 
         GOTO1 AFVAL,STOKEYH                                                    
         BNE   VALKEY04                                                         
         MVC   STOKEYH+5(1),FVILEN                                              
*                                                                               
VALKEY04 MVI   ONEXONLY,NO                                                      
         OI    STOKEYH+6,FVOXMT    ALWAYS TRANSMIT                              
         GOTO1 AFVAL,STOCDEH                                                    
         BE    VALKEY05                                                         
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         TWAXC STORECH,STOLSTH                                                  
         B     EXIT                                                             
*                                                                               
VALKEY05 L     R6,=A(MONTAB)                                                    
         A     R6,APRELO                                                        
         GOTO1 VDICTAT,APPARM,C'LU  ',(R6),MONLIST                              
*                                                                               
         USING OFFALD,R1                                                        
         USING KEYD,R6                                                          
         L     R6,AKEYTAB                                                       
VALKEY10 CLI   0(R6),EOT           END OF TABLE                                 
         BNE   VALKEY11                                                         
         MVC   FVMSGNO,=AL2(1)     ? INVALID CODE FOR KEY                       
         B     EXIT                                                             
*                                                                               
VALKEY11 CLC   KEYCODE,FVIFLD                                                   
         BNE   VALKEY12                                                         
         CLI   KEYID,15            ONE BYTE OFFICE?                             
         BNE   VALKEY15                                                         
         L     R1,AOFFBLK                                                       
         TM    OFFACST4,X'01'      NEW OFFICE                                   
         BZ    VALKEY15                                                         
*                                                                               
VALKEY12 SR    RF,RF                                                            
         ICM   RF,3,KEYNTRY        NUMBER OF ENTRIES                            
         MH    RF,=Y(KEYLNQ2)                                                   
         LA    R6,KEYLNQ1(RF,R6)                                                
         B     VALKEY10                                                         
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* ======================  DYNAMIC KEY BUILD  =========================*         
***********************************************************************         
         SPACE 1                                                                
VALKEY15 ST    R6,AKEYNTRY         SAVE KEY ENTRY                               
         LA    R2,IOKEY            KEY PARTS                                    
         MVC   IDKEY#,KEYID                                                     
         MVC   INDKEY,KEYIND                                                    
         GOTO1 VSCANNER,APPARM,STOKEYH,(10,BLOCK),0                             
         MVC   NPARMS,APPARM+4     NUMBER OF PARAMETERS                         
         TM    KEYIND,KEYSPC                                                    
         BZ    *+10                                                             
         MVC   IOKEY,SPACES        CLEAR TO SPACES                              
         TM    KEYIND,KEYNULL                                                   
         BZ    *+10                                                             
         XC    IOKEY,IOKEY         CLEAR TO NULLS                               
         CLI   APACTN,ACTUPL                                                    
         BNE   VALKEY18                                                         
         XC    LASTELN,LASTELN     SAVE LENGTH OF DATA                          
         CLI   STOTAB,C'X'                                                      
         BNE   *+8                                                              
         MVI   SAVEELM,YES         YES SAVE AN ELEMENT TODAY                    
         TM    KEYIND,KEYWRT       CAN WE UPLOAD THIS RECORD TYPE?              
         BZ    IVALUPL             IVALID RECORD TO UPLOAD                      
*                                                                               
VALKEY18 MVI   NEWKEY,NO                                                        
         SR    R1,R1                                                            
         ICM   R1,3,KEYNTRY        NUMBER OF KEY COMPONENTS                     
         LR    R0,R1                                                            
         LA    R6,KEYLNQ1(,R6)     BEGINNING OF ELEMENT COMPONENTS              
*                                                                               
         USING KEYSCDE,R6                                                       
VALKEY20 SR    R2,R2                                                            
         ICM   R2,1,KEYDISP                                                     
         LA    R2,IOKEY(R2)        POINT TO KEY DATA                            
         SR    RF,RF                                                            
         IC    RF,KEYBLD           ROUTINE NUMBER                               
         SLL   RF,2                MULTIPLY BY 4                                
         LA    RE,VALKEY40         RETURN ADDRESS                               
         B     *(RF)                                                            
*                                                                               
         B     VALEQU              MOVE IN KEYEQU                               
         B     VALCPY              MOVE IN COMPANY CODE                         
         B     VALCHR              MOVE CHARACTERS IN BASE ON FIELD #           
         B     VALINI              INITIALIZATION REC FOR REP TYPE              
         B     VALCHR              SAME AS VALCHR EXCEPT SPACE FILLED           
         B     VALKEY40            ?                                            
         B     VALKEY40            ?                                            
         B     VALKEY40            ?                                            
*                                                                               
VALKEY40 LA    R6,KEYLNQ2(,R6)     NEXT COMPONENT                               
         BCT   R0,VALKEY20                                                      
*                                                                               
         USING KEYD,R6                                                          
VALKEY42 L     R6,AKEYNTRY                                                      
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA1                                                      
         BAS   RE,GETIO1           RESTORE IO1 FROM TWA                         
         MVC   APINDS,SVAPINDS     RESTORE APINDS                               
         CLI   APACTN,ACTDWL       IF NOT DOWNLOAD THEN CHECK MORE              
         BNE   VALKEY44                                                         
         CLI   STOTAB,C'X'         TELLS ME THERE IS MORE                       
         BE    *+10                                                             
         XC    SAVEKEY,SAVEKEY     FORCE TO START OVER ALWAYS                   
*                                                                               
VALKEY44 CLC   APACTN,TWALACT      PREVIOUS ACTION                              
         BE    *+10                                                             
         XC    SAVEKEY,SAVEKEY     FORCE TO READ RECORD                         
         CLC   SAVEKEY(L'RESKEY),IOKEY                                          
         BNE   VALKEY50                                                         
         CLC   0(L'RESKEY,R2),IOKEY  DO I REALLY HAVE IT IN TWA4?               
         BNE   VALKEY50              NO SO INITIALIZE                           
         CLI   APACTN,ACTUPL       UPLOAD DATA                                  
         BNE   VALKEY98                                                         
         TM    GENIND,GENRSTEL     RESTORE ELEMENT?                             
         BZ    VALKEY98                                                         
         L     RE,AIOAREA3         SEE IF NEED TO RESTORE LAST ELEMENT          
         OC    0(4,RE),0(RE)       IF LEN=0 THEN NO DATA                        
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    R0,4(,RE)           SOURCE                                       
         L     R1,0(,RE)           LENGTH OF SOURCE                             
         ST    R1,LASTELN          SAVE LENGTH OF DATA                          
         L     RE,AELMBLK          DESTINATION                                  
         L     RF,=A(MAXELMBQ)     LENGTH OF DESTINATION                        
         MVCL  RE,R0                                                            
         B     VALKEY98            ALREADY HAVE RECORD IN AIOAREA1              
*                                                                               
VALKEY50 MVI   ONEXONLY,NO                                                      
         MVI   APINDS,0            RESET APINDS                                 
         XC    SVSECLVL,SVSECLVL                                                
         XC    SAVEKEY,SAVEKEY                                                  
         NI    GENIND,TURNOFF-GENRSTEL                                          
         L     R2,AIOAREA1                                                      
*                                                                               
         USING RESRECD,R2                                                       
         CLI   KEYID,01                                                         
         BE    VALKEY68                                                         
         LA    R1,IORDD+IOACCFIL+IO1                                            
         TM    KEYIND,KEYRDHI                                                   
         BZ    *+8                                                              
         LA    R1,IOHIGH+IOACCFIL+IO1                                           
         CLI   APACTN,ACTUPL       UP-LOAD                                      
         BNE   *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BE    VALKEY60                                                         
*                                                                               
*        ERROR = EOF/NOF/DELETED                                                
*                                                                               
         CLI   APACTN,ACTUPL       UP-LOAD                                      
         BE    VALKEY52                                                         
         TM    IOERR,IOEDEL        MARK DELETE                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=X'FF38'                                                 
         TM    IOERR,IOERNF+IOEEOF RECORD NOT FOUND ?                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         TWAXC STORECH,STOLSTH                                                  
         B     VALKEY99                                                         
*                                                                               
VALKEY52 XC    0(256,R2),0(R2)     RESET AIO AREA                               
         MVC   RESKEY,IOKEY        SET KEY                                      
         LH    R1,DATADISP                                                      
         LA    R1,1(,R1)           ADD ONE FOR EOR                              
         STH   R1,RESRLEN          SET RECORD LENGTH                            
         MVC   SVAPINDS,APINDS                                                  
         MVI   NEWKEY,YES                                                       
         MVI   APINDS,APIOKDIS+APIOKADD                                         
         CLI   KEYID,03            FORMAT RECORD?                               
         BE    VALKEY61                                                         
         B     VALKEY98                                                         
         EJECT ,                                                                
***********************************************************************         
*  AMEND TEXT ELEMENTS TO FORMAT RECORD OR SET UP IO2 FOR TEXT ELEMS  *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
VALKEY60 DS    0H                                                               
         CLI   KEYID,03            FORMAT RECORD?                               
         BNE   VALKEY68            NO SO DON'T WORRY                            
         GOTO1 GETTYPE,(R2)                                                     
         CLI   APACTN,ACTUPL       UP-LOAD                                      
         BNE   VALKEY62            UP-LOAD DIFFERENT PROBLEM                    
*                                                                               
VALKEY61 L     R2,AIOAREA2                                                      
         L     R3,AIOAREA1         FORMAT KEY                                   
         XC    0(256,R2),0(R2)                                                  
         MVC   RESKEY,0(R3)        MOVE IN KEY                                  
         MVI   RESKSEQ,RESKSTXT    BUILD KEY FOR TEXT RECORD                    
         LH    R1,DATADISP                                                      
         LA    R1,1(,R1)           ADD ONE FOR EOR                              
         STH   R1,RESRLEN          SET RECORD LENGTH                            
         AH    R2,DATADISP                                                      
         L     R2,AIOAREA1         RESET R2                                     
         CLI   NEWKEY,YES                                                       
         BNE   VALKEY68                                                         
         B     VALKEY98                                                         
*                                                                               
VALKEY62 SR    R1,R1                                                            
         LR    R3,R2                                                            
         AH    R3,DATADISP         GET X'25' ELEMENT                            
*                                                                               
         USING STYELD,R3                                                        
VALKEY64 CLI   0(R3),0             EOR                                          
         BE    VALKEY68            SHOULD REALY NEVER HAPPEN                    
         CLI   0(R3),STYELQ        X'25'                                        
         BE    VALKEY66                                                         
         IC    R1,1(,R3)                                                        
         AR    R3,R1                                                            
         B     VALKEY64                                                         
*                                                                               
VALKEY66 TM    STYSTAT,STYSTXT     IS THERE A TEXT RECORD ATTACHED?             
         BZ    VALKEY68                                                         
         DROP  R3                                                               
*                                                                               
         LA    R2,IOKEY            CHANGE TO GET TEXT SCRIBE RECORD             
         MVI   RESKSEQ,RESKSTXT    SCRIBE TEXT RECORD                           
         GOTO1 AIO,IORDD+IOACCFIL+IO3                                           
         BNE   VALKEY68            SHOULDN'T HAPPEN IF BIT WAS ON               
*                                                                               
DES      USING RESRECD,RE                                                       
*                                                                               
         L     R2,AIOAREA3         R2 = SOURCE                                  
         L     RE,AIOAREA1         RE = DESTINATION                             
         LH    R3,RESRLEN          GET LENGTH OF RECORD                         
         AH    R2,DATADISP         POINT TO ELEMENT TO MOVE                     
         SH    R3,DATADISP         LESS KEY LENGTH = ELEMENTS ONLY              
         LH    RF,DES.RESRLEN      LENGTH OF CURRENT RECORD                     
         LR    R1,RF                                                            
         AR    R1,R3               MERGE LENGTH OF BOTH RECORDS                 
         STH   R1,DES.RESRLEN      SAVE NEW RECORD LENGTH                       
         AR    RE,RF               POINT TO END OF RECORD                       
         BCTR  RE,0                LESS ONE FOR EOR MARKER                      
         LR    RF,R3               LENGTH OF DESTINATION                        
         MVCL  RE,R2                                                            
         LA    RE,IOKEY                                                         
         MVI   DES.RESKSEQ,RESKSREG   SWITCH BACK TO NORMAL RECORD              
         DROP  R2                                                               
         DROP  DES                                                              
*                                                                               
VALKEY68 MVC   DISPADDR,DATADISP   SET TO POINT AT ELEMENTS                     
         MVC   PREVDISP,DATADISP                                                
         MVC   SCRTYP(L'APREPCDE),APREPCDE      MAKE TYPE                       
         OI    SCRTYPH+6,FVOXMT                                                 
         NI    RESRSTA-RESKEY(R2),TURNOFF-X'80' MARK UNDELETE                   
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         CLI   KEYID,01                                                         
         BNE   *+8                                                              
         MVI   APINDS,APIOKDIS                                                  
         MVC   SVAPINDS,APINDS                                                  
*                                                                               
         CLI   APACTN,ACTUPL       UP-LOAD                                      
         BNE   VALKEY80                                                         
         SR    R1,R1                                                            
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
*                                                                               
VALKEY70 CLI   0(R4),0             END OF RECORD                                
         BNE   VALKEY72                                                         
         GOTO1 DELEL,(R2)          DELETE ELEMENTS AND SAVE IN TWA              
         B     VALKEY80                                                         
*                                                                               
VALKEY72 CLI   0(R4),PTRELQ        POINTER TO REQUESTING                        
         BE    *+8                                                              
         CLI   0(R4),DTSELQ        DATE LAST REQUESTED                          
         BE    *+8                                                              
         MVI   0(R4),X'FF'         MARK FOR DELETION                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     VALKEY70                                                         
*                                                                               
VALKEY80 L     R2,AIOAREA1                                                      
         BAS   RE,SAVIO1           SAVE IO1 TO TWA 4                            
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
         L     R2,AIOAREA1                                                      
         MVC   SAVEKEY(L'RESKEY),0(R2)                                          
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
         USING KEYSCDE,R6                                                       
VALEQU   MVC   0(1,R2),KEYEQU#     MOVE IN EQUATE                               
         BR    RE                                                               
*                                                                               
VALCPY   MVC   0(1,R2),CUABIN      COMPANY CODE                                 
         BR    RE                                                               
*                                                                               
VALCHR   NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,KEYFLD#          FIELD 3 INTO SCAN BLOCK                      
         SH    R1,=H'01'                                                        
         BM    XIT                                                              
         MH    R1,=Y(L'BLOCK)                                                   
         LA    RE,BLOCK(R1)        POINT INTO SCAN BLOCK                        
         SR    RF,RF                                                            
         CLC   KEYFLDLN,0(RE)      IS IT TOO BIG?                               
         BL    XIT                 YES                                          
         IC    RF,KEYFLDLN                                                      
         CLI   KEYBLD,5            USE FULL LENGTH OF FIELD                     
         BE    *+8                                                              
         IC    RF,0(,RE)           GET INPUT LENGTH OF DATA                     
         SH    RF,=H'01'                                                        
         BM    XIT                 NO DATA?                                     
         EX    RF,*+4                                                           
         MVC   0(0,R2),12(RE)      MOVE IN DATA FROM SCAN BLOCK                 
         B     XIT                                                              
         EJECT ,                                                                
VALINI   NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,10            THIS IS A FAKE KEY                           
         MVC   IOKEY+1(3),=C'INI'                                               
         CLC   IOKEY(L'RESKEY),SAVEKEY                                          
         BE    VALINIX                                                          
         L     R3,=A(LDGINI)       LEDGER(S) TO SEND DOWN                       
         A     R3,APRELO                                                        
         CLI   BLOCK,1             MUST BE LENGTH ONE                           
         BH    VALINIX                                                          
         LA    RE,BLOCK+12                                                      
*                                                                               
VALINI10 CLI   0(R3),0                                                          
         BE    VALINIX                                                          
         CLC   0(1,R3),0(RE)                                                    
         BE    VALINI12                                                         
         SR    R1,R1                                                            
         IC    R1,1(,R3)                                                        
         AR    R3,R1                                                            
         B     VALINI10                                                         
*                                                                               
VALINI12 SR    R0,R0                                                            
         IC    R0,1(,R3)                                                        
         SH    R0,=H'02'                                                        
         SRL   R0,1                R1 = # OF LEDGERS TO READ FOR                
         LA    R3,2(,R3)           POINT TO LIST OF LEDGERS                     
         L     R6,AIOAREA1                                                      
         AH    R6,DATADISP                                                      
         MVI   APINDS,APIOKDIS     OK TO DISPLAY                                
         MVC   DISPADDR,DATADISP   SET TO POINT AT ELEMENTS                     
         MVC   PREVDISP,DATADISP   SET TO POINT AT ELEMENTS                     
         XC    DISPAKYW,DISPAKYW                                                
         XC    PREVAKYW,PREVAKYW                                                
         MVC   HIGHVER#(3),OPTVER#                                              
*                                                                               
         MVC   WORK(8),=CL8'YYYYYYYY'                                           
         OC    TWASAGN,TWASAGN     ANY SECURITY                                 
         BZ    VALINI14                                                         
         MVC   WORK(8),=CL8'NNNNNNNN'                                           
         LA    R4,APHALF                                                        
         MVI   APBYTE,ACTCHA                                                    
         GOTO1 VSECRET,APPARM,('SECPRACT',ACASEC),('RECFRM',APBYTE)             
         BNE   *+8                                                              
         MVI   WORK,C'Y'                                                        
         MVI   APBYTE,ACTREQ                                                    
         GOTO1 VSECRET,APPARM,('SECPRACT',ACASEC),('RECRPT',APBYTE)             
         BNE   *+8                                                              
         MVI   WORK+1,C'Y'                                                      
*                                                                               
         USING SECELD,R1                                                        
VALINI14 LA    R1,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   SECEL,SECELQ        LOCAL SECURITY ELEMENT                       
         MVI   SECLN,SECLNQ                                                     
         MVC   SECSVAL,WORK                                                     
         BAS   RE,PUTELM                                                        
*                                                                               
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUABIN                                                   
         GOTO1 AIO,IOREAD+IO3+IOACCFIL                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R1,AIOAREA3                                                      
         MVI   APELCODE,NAMELQ     X'20' COMPANY NAME                           
         GOTO1 GETEL                                                            
         BNE   *+8                                                              
         BAS   RE,PUTELM                                                        
         MVI   APELCODE,ADRELQ     X'22' COMPANY ADDRESS                        
         GOTO1 NEXTEL                                                           
         BNE   *+8                                                              
         BAS   RE,PUTELM                                                        
*                                                                               
         USING LDGRECD,R2                                                       
*                                                                               
VALINI24 MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(2),0(R3)   MOVE IN UNIT/LEDGER                           
         GOTO1 AIO,IOREAD+IO3+IOACCFIL                                          
         BNE   VALINI28                                                         
*                                                                               
         USING NAMELD,R1                                                        
         L     R1,AIOAREA3                                                      
         MVI   APELCODE,NAMELQ     X'20' LEDGER NAME                            
         GOTO1 GETEL                                                            
         BNE   VALINI26                                                         
         MVC   APWORK,SPACES                                                    
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK(0),NAMEREC                                                
*                                                                               
         USING ACLELD,R1                                                        
VALINI26 L     R1,AIOAREA3                                                      
         MVI   APELCODE,ACLELQ     X'16' LEDGER ELEMENT                         
         GOTO1 GETEL                                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         ST    R6,SVR6             LOCATION OF NEW ELEMENT                      
         BAS   RE,PUTELM                                                        
         L     R1,SVR6                                                          
         SR    RF,RF                                                            
         IC    RF,ACLLN                                                         
         LA    RF,38(,RF)          INCREASE ELEMENT LENGTH TO NEW SIZE          
         STC   RF,ACLLN                                                         
         MVC   0(2,R6),0(R3)       LEDGER                                       
         MVC   2(36,R6),APWORK     SAVED LEDGER NAME                            
         LA    R6,38(,R6)                                                       
*                                                                               
VALINI28 LA    R3,2(,R3)           NEXT LEDGER IF ANY                           
         BCT   R0,VALINI24                                                      
*                                                                               
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,CUUSER                                                   
         GOTO1 AIO,IOREAD+IO3+IOCTFILE                                          
         BNE   VALINI50                                                         
         L     R1,AIOAREA3                                                      
         LA    R1,CTIDATA-CTIREC(,R1) POINT TO FIRST ELEMENT                    
*                                                                               
VALINI30 CLI   0(R1),0             EOR                                          
         BE    VALINI40                                                         
         CLI   0(R1),CTDSTELQ      X'30'                                        
         BNE   VALINI35                                                         
         BAS   RE,PUTELM                                                        
         B     VALINI40                                                         
*                                                                               
VALINI35 SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     VALINI30                                                         
*                                                                               
         USING CTVALD,R1                                                        
VALINI40 GOTO1 VGETIDS,APPARM,(C'D',AIOAREA3),0,VDMGR                           
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BZ    VALINI50                                                         
         XC    ELEMENT,ELEMENT                                                  
         L     R3,4(,R1)           LIST OF DESTINATIONS                         
         LA    R1,ELEMENT                                                       
         MVI   CTVALEL,CTVALELQ    X'34' VALID DESTINATIONS                     
         MVI   CTVALLEN,12                                                      
*                                                                               
VALINI42 MVC   CTVALDST,0(R3)                                                   
         BAS   RE,PUTELM                                                        
         LA    R3,12(,R3)          NEXT DESTINATION                             
         BCT   R0,VALINI42                                                      
         DROP  R1                                                               
*                                                                               
         USING CAHRECD,R2                                                       
VALINI50 MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,CUABIN                                                   
         LA    R1,IOHIGH+IO3+IOACCFIL                                           
*                                                                               
VALINI52 GOTO1 AIO                                                              
         L     R3,AIOAREA3                                                      
         CLC   CAHKEY(3),0(R3)                                                  
         BNE   VALINI90                                                         
         AH    R3,DATADISP                                                      
*                                                                               
         USING METELD,R1                                                        
         LA    R1,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   METEL,METELQ            MODIFIED X'82' ELEMENT                   
         MVI   METLN,METCODE-METELD    DEFAULT LENGTH                           
*                                                                               
         USING NAMELD,R3                                                        
VALINI53 CLI   0(R3),0             END OF RECORD                                
         BE    VALINI58                                                         
         CLI   0(R3),NAMELQ        X'20' NAME OF METHOD                         
         BNE   VALINI54                                                         
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    VALINI56                                                         
         EXMVC RF,METCODE+3,NAMEREC                                             
         LA    RF,METCODE-METELD+4(RF)    LENGTH OF ELEMENT                     
         STC   RF,METLN                   NEW LENGTH                            
         DROP  R3                                                               
*                                                                               
REAL     USING METELD,R3                                                        
*                                                                               
VALINI54 CLI   0(R3),METELQ        X'82' REAL METHOD ELEMENT                    
         BNE   VALINI56                                                         
         MVC   METNUM,REAL.METNUM                                               
         MVC   METCODE,REAL.METCODE                                             
         DROP  REAL                                                             
*                                                                               
VALINI56 SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     VALINI53                                                         
*                                                                               
VALINI58 BAS   RE,PUTELM                                                        
         LA    R1,IOSEQ+IO3+IOACCFIL                                            
         B     VALINI52                                                         
*                                                                               
VALINI90 LA    R6,1(,R6)                                                        
         L     RE,AIOAREA1                                                      
         SR    R6,RE               LENGTH OF RECORD                             
         STH   R6,ACTRLEN-ACTRECD(,RE)                                          
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,10            THIS IS A FAKE KEY                           
         MVC   IOKEY+1(3),=C'INI'                                               
         MVC   ACTKEY-ACTRECD(L'ACTKEY,RE),IOKEY                                
         BAS   RE,SAVIO1                                                        
*                                                                               
VALINIX  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  SAVE OFF ALL IO AREAS, BUILD RECORD AND SAVE IN TWA                *         
***********************************************************************         
         SPACE 1                                                                
GETIO1   STM   RE,R1,SVRE                                                       
         MVC   APPARM+20(2),=C'L='  READ TWA2 INTO AIO1                         
         MVC   APPARM+22(2),=Y(L'IOAREA1*3)                                     
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(4,0),AIOAREA1                       
         LM    RE,R1,SVRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
SAVIO1   STM   RE,R1,SVRE                                                       
         MVC   APPARM+20(2),=C'L='  READ TWA2 INTO AIO1                         
         MVC   APPARM+22(2),=Y(L'IOAREA1*3)                                     
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),AIOAREA1                      
         LM    RE,R1,SVRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
PUTELM   SR    RF,RF                                                            
         IC    RF,1(,R1)           GET ELEMENT LENGTH                           
         BCTR  RF,0                                                             
         EXMVC RF,0(R6),0(R1)      MOVE IN ELEMENT                              
         LA    R6,1(RF,R6)                                                      
         MVI   0(R6),0                                                          
         BR    RE                                                               
         EJECT ,                                                                
PRCREC   L     R2,AIOAREA1                                                      
*        MVC   RESKEY,APRECKEY                                                  
         CLI   APACTN,ACTUPL       UP-LOAD                                      
         BE    UPLREC                                                           
         CLI   APACTN,ACTDWL       DOWN-LOAD                                    
         BE    DWLREC                                                           
         CLI   APACTN,ACTVAL       VALIDATE                                     
         BE    VALACT                                                           
         B     EXIT                                                             
         EJECT ,                                                                
         USING ELMSUBEL,R6                                                      
UPLREC   DS    0H                                                               
         L     R2,AELMBLK                                                       
         XC    APELEM,APELEM                                                    
         XC    ELEMENT,ELEMENT                                                  
         BAS   RE,BLDBLOCK         MOVE DATA FROM SCREEN TO ELMBLK              
         CLI   0(R2),0             END OF SCAN BLOCK?                           
         BE    UPLREC97            NO INPUT AT ALL, DELETE RECORD               
*                                                                               
UPLREC20 GOTO1 =A(GETCODE),APPARM,(R2),RR=APRELO                                
         L     R2,APPARM           NEXT POSITION IN SCAN BLOCK                  
         ICM   R6,15,APPARM+4      ELEMENT ENTRY                                
         BNZ   UPLREC30            SKIP THIS DATA                               
         CLI   APPARM,X'FF'        END OF DATA                                  
         BNE   UPLREC20            SKIP THIS DATA                               
         B     UPLREC70            ABORT THIS ELEMENT FOR NOW                   
*                                                                               
UPLREC30 TM    UPLFLG,UPLELM       START OF ELEMENT, SET UP IN ELEMENT          
         BO    UPLREC40                                                         
         SR    R3,R3                                                            
         IC    R3,ELMDISP          DISPLACEMENT INTO ELEMENT                    
         LA    R3,ELEMENT(R3)      POINT TO POSITION                            
         SR    RF,RF                                                            
         ICM   RF,1,ELMUPL         ROUTINE NUMBER                               
         BZ    UPLREC40            NO ROUTINE                                   
         SLL   RF,2                MULTIPLY BY 4                                
         LA    RE,UPLREC40         RETURN ADDRESS                               
         B     *(RF)                                                            
*                                                                               
         B     UPLCHR           01 MOVE CHARACTER                               
         B     UPLN2X           02 NUMBER TO HEX                                
         B     UPLN2P           03 NUMBER TO PACKED                             
         B     UPLDTP           04 DD MMM YY TO PACKED YYMMDD                   
         B     UPLSPC           05 SPECIAL ADD LENGTH + DISP TO ELEM            
         B     UPLRFL           06 SPECIAL FOR RFELD                            
         B     UPLCDT           07 SPECIAL FOR COLUMN DATES                     
         B     UPLFLT           08 SPECIAL FOR FILTERS                          
         B     UPLRNK           09 SPECIAL FOR FILTERS                          
         B     UPLKYW           10 VALIDATE SECURITY OF KEYWORDS                
         B     UPLRCL           11 RCLNDATA, RCLHD1, RCLHD2                     
         B     UPLRRW           12 RRWNDATA, RRWPRFX                            
*                                                                               
UPLREC40 CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS FOUND ?                          
         BNE   UPLRECER            YES, RETURN                                  
         OC    APELEM,APELEM       FIRST TIME IN?                               
         BZ    UPLREC20            YES, SO KEEP BUILDING ELEMENT                
         L     R6,AIOAREA1                                                      
         CLI   APELEM,RFLELQ       X'C5'                                        
         BNE   UPLREC41                                                         
         CLI   APELEM+1,RFLLNQ     IS THERE DATA                                
         BE    UPLREC52            NO, BAD ELEMENT, SO DON'T ADD                
*                                                                               
EL       USING RRWELD,APELEM                                                    
*                                                                               
UPLREC41 CLI   APELEM,RRWELQ       X'C2' ROW                                    
         BNE   *+8                                                              
         OI    EL.RRWOPT,RRWNEWEL                                               
*                                                                               
EL       USING RCLELD,APELEM                                                    
         CLI   APELEM,RCLELQ       X'C3' COL                                    
         BNE   *+8                                                              
         OI    EL.RCLOPT,RCLNEWEL                                               
         DROP  EL                                                               
*                                                                               
UPLREC42 CLI   IDKEY#,03           FORMAT RECORD                                
         BNE   UPLREC50                                                         
         CLI   APELEM,PACELQ       X'A1'                                        
         BE    UPLREC52            DON'T UPLOAD THIS ELEMENT                    
         CLI   APELEM,FFTELQ       X'DB' TEXT DATA                              
         BNE   *+8                 SWITCH AREA TO BUILD TEXT RECORD             
         L     R6,AIOAREA2                                                      
*                                                                               
UPLREC50 GOTO1 ADDEL,(R6)                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   APELEM,STYELQ       X'25'                                        
         BNE   UPLREC52            SET TYPE BASED ON SCRIBE TYPE ELEM           
         GOTO1 GETTYPE,(R6)        TYPE VARIABLES CAN CHANGE, SO SET            
*                                                                               
UPLREC52 XC    APELEM,APELEM                                                    
         LR    RE,R2                                                            
         SH    RE,=H'07'           BACKUP TO BEGINING OF ELEMENT                
         ST    RE,ALASTEL          WHERE WE ARE UP TO IN BLOCK                  
         L     RF,AEOFBLK          END OF BLOCK                                 
         SR    RF,RE               CALCULATE LENGTH OF DATA LEFT                
         ST    RF,LASTELN          LENGHT OF LAST ELEMENT DATA                  
         B     UPLREC20                                                         
*                                                                               
UPLREC70 L     R6,AIOAREA1                                                      
         MVC   APELEM,ELEMENT      MOVE IN LAST ELEMENT                         
         CLI   APELEM+1,0          ANY LENGTH                                   
         BE    UPLREC74            NO SO SKIP                                   
         CLI   APELEM,RFLELQ       X'C5'                                        
         BNE   UPLREC71                                                         
         CLI   APELEM+1,RFLLNQ     IS THERE DATA                                
         BE    UPLREC74            NO, BAD ELEMENT, SO DON'T ADD                
*                                                                               
UPLREC71 CLI   STOTAB,C'X'         ANY MORE DATA                                
         BE    UPLREC74            NOT FINISHED SO HOLD UP ELEMENT              
         CLI   IDKEY#,03           FORMAT RECORD                                
         BNE   UPLREC72                                                         
         CLI   APELEM,FFTELQ       X'DB' TEXT DATA                              
         BNE   *+8                 SWITCH AREA TO BUILD TEXT RECORD             
         L     R6,AIOAREA2                                                      
*                                                                               
UPLREC72 GOTO1 ADDEL,(R6)          PUT OUT LAST ELEMENT                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
UPLREC74 CLI   STOTAB,C'X'         ANY MORE DATA                                
         BNE   UPLREC76            IF EQUAL THEN SAVE OFF ELEMENT               
         OI    GENIND,GENRSTEL     RESTORE ELEMENT?                             
         L     RE,AIOAREA3                                                      
         LA    RE,4(,RE)           VARIABLE LENGTH TYPE RECORD                  
         LH    RF,=Y(L'IOAREA3)                                                 
         L     R0,ALASTEL          LAST ELEMENT IN AELMBLK                      
         L     R1,LASTELN          LAST ELEMENT LENGTH                          
         MVCL  RE,R0                                                            
         L     RE,AIOAREA3                                                      
         MVC   0(4,RE),LASTELN     SAVE LENGTH OF DATA                          
         BAS   RE,SAVIO1           SAVE IO1-IO3 TO TWA 4                        
         B     UPLREC97                                                         
*                                                                               
UPLREC76 NI    GENIND,TURNOFF-GENRSTEL  RESTORE ELEMENT?                        
         CLI   IDKEY#,03                FORMAT RECORD                           
         BNE   UPLREC78                                                         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
UPLREC78 L     R2,AIOAREA1                                                      
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         MVI   TEXTREC,NO                                                       
         CLI   0(R4),0                    ANY ELEMENTS?                         
         BNE   UPLREC80                   ELEMENTS PRESENT                      
         OI    RESRSTA-RESKEY(R2),X'80'   MARK DELETED                          
         B     UPLREC90                                                         
*                                                                               
UPLREC80 CLI   IDKEY#,03           FORMAT RECORD                                
         BNE   UPLREC90                                                         
         MVI   APELCODE,STYELQ     X'25' SCRIBE TYPE                            
         GOTO1 GETEL,(R2)                                                       
         BE    *+6                                                              
         DC    H'00'               MUST HAVE THIS ELEMENT                       
*                                                                               
         USING STYELD,R4                                                        
         LR    R4,R1                                                            
         NI    STYSTAT,TURNOFF-STYSTXT                                          
         OI    STYSTAT,STYSSTO     AMEMDED THROUGH STEREO                       
         MVC   STYSEC#1,SVSECLVL   SET KEYWORD SECURITY                         
*                                                                               
         USING RPFELD,R1                                                        
         MVI   APELCODE,RPFELQ     X'C4' GENERAL PROFILE ELEMENT                
         GOTO1 GETEL,(R2)                                                       
         BNE   UPLREC82                                                         
         CLI   STYCODE,REP#CST     PERSON WRITER?                               
         BNE   UPLREC82            NO, SO DOESN'T MATTER                        
         CLI   RPFCLIOF,C'C'                                                    
         BE    *+8                                                              
         CLI   RPFCLIOF,C'B'                                                    
         BNE   UPLREC82                                                         
         OI    STYSEC#5,STYSOFF    SPECIAL OFFICE SECURITY                      
         DROP  R1                                                               
*                                                                               
UPLREC82 L     R2,AIOAREA2                                                      
         AH    R2,DATADISP                                                      
         CLI   0(R2),0             ANY TEXT ELEMENTS ADDED?                     
         BE    UPLREC90            NO                                           
         OI    STYSTAT,STYSTXT     SET TO SAY TEXT RECORD                       
         MVI   TEXTREC,YES                                                      
         DROP  R4                                                               
*                                                                               
UPLREC90 LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD A RECORD                                 
         BO    UPLREC94                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A RECORD                              
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
UPLREC94 GOTO1 AIO                                                              
         BE    UPLREC95                                                         
         TM    IOERR,IOEDUP                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
         XC    SAVEKEY,SAVEKEY     FORCE TO BE ABLE TO UPLOAD AGAIN             
         B     UPLREC97                                                         
*                                                                               
         USING RESRECD,R2                                                       
UPLREC95 CLI   IDKEY#,03           FORMAT RECORD                                
         BNE   UPLREC97                                                         
         L     R3,AIOAREA1                                                      
         LA    R2,IOKEY                                                         
         MVC   RESKEY,0(R3)        SEE IF RECORD ON FILE ALREADY                
         MVI   RESKSEQ,RESKSTXT    READ FOR TEXT RECORD                         
         LA    R1,IORDD+IOACCFIL+IO3+IOLOCK                                     
         GOTO1 AIO                                                              
         BL    VALKEY99                  IO ERROR                               
         LA    R1,IOWRITE+IOACCFIL       TO WRITE BACK TEXT RECORD              
         BE    UPLREC96                  SO WRITE BACK OVER IT                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    IOERR,IOEDEL              WAS     IT DELETED?                    
         BO    *+8                       YES                                    
         LA    R1,IOADD+IOACCFIL         NO SO   ADD RECORD                     
         CLI   TEXTREC,YES               DO      WE HAVE RECORD TO ADD?         
         BNE   UPLREC97                  NO, SO OK SINCE DELETE OR              
*                                                        NOT FOUND              
UPLREC96 L     R2,AIOAREA3               OLD RECORD CURRENTLY ON FILE           
         LR    R3,R1                     SAVE OFF R1                            
         LA    R1,IO3(,R1)               USE IO AREA 3 (OLD RECORD)             
         OI    RESRSTA,X'80'             MARK    DELETED                        
         CLI   TEXTREC,YES               DO WE HAVE TO ADD ONE ?                
         BNE   UPLRC96A                  YES                                    
         L     R2,AIOAREA2               NEW RECORD TO ADD                      
         LA    R3,IO2(,R3)               USE IO AREA 2 (NEW RECORD)             
         NI    RESRSTA,TURNOFF-X'80'     MARK    UNDELETE                       
         LR    R1,R3                                                            
*                                                                               
UPLRC96A GOTO1 AIO                                                              
*                                                                               
UPLREC97 TWAXC STORECH,STOLSTH                                                  
*                                                                               
UPLREC98 B     EXIT                                                             
*                                                                               
UPLRECER MVI   STOTAB,C' '         CLEAR ANY  MORE DATA                         
*                                  TURN  OFF  RESTORE   ELEMENT                 
         NI    GENIND,TURNOFF-GENRSTEL                                          
         XC    SAVEKEY,SAVEKEY     FORCE TO   BE   ABLE TO   UPLOAD             
         TWAXC STORECH,STOLSTH                                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
         USING ELMSUBEL,R6                                                      
UPLSPC   NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,ELMFLDLN         FIXED LENGTH OF FIELD                        
         SR    RF,RF                                                            
         IC    RF,ELMDISP          GET DISPLACMENT                              
         AR    RF,R1               RF=LENGTH+DISPLACMENT                        
         STC   RF,ELEMENT+1        NEW ELEMENT LENGTH                           
         B     UPLCHR10                                                         
*                                                                               
UPLCHR   NTR1                                                                   
*                                                                               
UPLCHR01 SR    R1,R1                                                            
         ICM   R1,1,ELMFLDLN       IS IT VARIABLE LENGTH DATA                   
         BNZ   UPLCHR10                                                         
         IC    R1,UPLLEN                                                        
         SR    RF,RF                                                            
         IC    RF,ELEMENT+1        GET LENGTH                                   
         AR    RF,R1                                                            
         STC   RF,ELEMENT+1        RE-ADJUST ELEMENT LENGTH                     
         B     UPLCHR20                                                         
*                                                                               
UPLCHR10 CLC   UPLLEN,ELMFLDLN                                                  
         BNH   UPLCHR20                                                         
*                                  UPLOAD FIELD TOO BIG                         
         MVC   FVMSGNO,=AL2(ACEUPBIG)                                           
*                                  FOR  DEBUGGING, PROVIDE:                     
         MVC   FVXTRA(1),UPLCODE        UPLOAD CODE                             
         MVI   FVXTRA+1,C'='            EQUAL  SIGN                             
         LA    R3,FVXTRA+2              UPLOAD DATA                             
         IC    R1,UPLLEN           GET  UPLOAD DATA LENGTH                      
*                                                                               
UPLCHR20 SH    R1,=H'01'                                                        
         EX    R1,*+4                                                           
         MVC   0(0,R3),UPLDATA     MOVE IN DATA                                 
*                                                                               
UPLCHR90 B     XIT                                                              
         EJECT ,                                                                
         USING RCLELD,R3                                                        
UPLRCL   NTR1                                                                   
         LA    R3,ELEMENT                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         IC    R1,UPLLEN           LENGTH OF DATA                               
         LA    RE,RCLNDATA                                                      
         CLI   ELMSHRT,C'L'        RCLNDATA ?                                   
         BNE   UPLRCL20            NO                                           
         STC   R1,RCLDATLN         SAVE OFF LENGTH                              
         IC    RF,RCLHD1LN         IS THERE A HEAD 1 ALREADY ?                  
         IC    R0,RCLHD2LN         IS THERE A HEAD 2 ALREADY ?                  
         AR    RF,R0               TOTAL HEADING TO MOVE                        
         BZ    UPLRCL10                                                         
         AR    RE,R1               POINT TO WHERE HEADINGS GO                   
         BCTR  RF,0                LENGTH OF HEADINGS                           
         EX    RF,*+4                                                           
         MVC   APWORK(0),RCLNDATA  TEMPORARY WORK AREA                          
         EX    RF,*+4                                                           
         MVC   0(0,RE),APWORK                                                   
         LA    RF,1(,RF)           ADD BACK FROM EX INSTR.                      
*                                                                               
UPLRCL10 BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   RCLNDATA(0),UPLDATA                                              
         LA    RF,RCLNLNQ+1(R1,RF) RF = L'RCLNDATA+L'RCLNHD1+L'RCLNHD2          
         B     UPLRCLOK                                                         
*                                                                               
UPLRCL20 CLI   ELMSHRT,C'1'        HEADING 1 ?                                  
         BNE   UPLRCL40            NO                                           
         IC    RF,RCLDATLN         LENGTH OF RCLNDATA                           
         AR    RE,RF               POINT TO WHERE HEAD 1 WILL GO                
         ICM   RF,1,RCLHD2LN       ANY HEADING 2 ?                              
         BZ    UPLRCL30                                                         
         LA    R6,0(R1,RE)         POINT TO WHERE TO MOVE HEADING 2             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK(0),0(RE)     MOVE HEADING 2 TO TEMPORARY AREA             
         EX    RF,*+4                                                           
         MVC   0(0,R6),APWORK      MOVE HEADING 2                               
*                                                                               
UPLRCL30 IC    RF,RCLDATLN         RESET RF = L'RCLNDATA                        
         STC   R1,RCLHD1LN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),UPLDATA     MOVE IN HEADING 1                            
         LA    RF,RCLNLNQ+1(R1,RF) RF = L'RCLNHD1 + L'RCLNDATA                  
         IC    R1,RCLHD2LN                                                      
         AR    RF,R1               RF = RF + L'RCLNHD2                          
         B     UPLRCLOK                                                         
*                                                                               
UPLRCL40 CLI   ELMSHRT,C'2'        HEADING 2                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         IC    RF,RCLDATLN                                                      
         AR    RE,RF               POINT TO HEADING 1                           
         IC    R0,RCLHD1LN                                                      
         AR    RE,R0               POINT TO HEADING 2                           
         STC   R1,RCLHD2LN         SAVE LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),UPLDATA                                                  
         LA    RF,RCLNLNQ+1(R1,RF) RF = L'RCLNDATA + L'RCLNHD2                  
         AR    RF,R0               RF = RF + L'RCLNHD1                          
*                                                                               
UPLRCLOK STC   RF,RCLLN            SAVE OFF NEW LENGTH OF ELEMENT               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
         USING RRWELD,R3                                                        
UPLRRW   NTR1                                                                   
         LA    R3,ELEMENT                                                       
         LA    RE,RRWNDATA                                                      
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,UPLLEN           LENGTH OF DATA                               
         CLI   ELMSHRT,C'G'        RRWNDATA ?                                   
         BNE   UPLRRW20            NO                                           
         ICM   RF,1,RRWPFXLN       LENGTH OF PREFIX                             
         BZ    UPLRRW10            MUST NOT HAVE A PREFIX                       
         AR    RE,R1               POINT WHERE PREFIX IS TO MOVE TO             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK(0),RRWNDATA    MOVE PREFIX                                
         EX    RF,*+4                                                           
         MVC   0(0,RE),APWORK      MOVE PREFIX                                  
         LA    RF,1(,RF)           ADD BACK FROM EX INSTR.                      
*                                                                               
UPLRRW10 STC   R1,RRWDATLN         SAVE OF DATA LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   RRWNDATA(0),UPLDATA MOVE IN RWWNDATA                             
         LA    RF,RRWNLNQ+1(R1,RF)                                              
         B     UPLRRWOK                                                         
*                                                                               
UPLRRW20 CLI   ELMSHRT,C'H'        RRWNDATA ?                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         IC    RF,RRWDATLN         LENGTH OF RRWNDATA                           
         AR    RE,RF               POINT TO END OF RRWNDATA                     
         STC   R1,RRWPFXLN         SAVE LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),UPLDATA                                                  
         LA    RF,RRWNLNQ+1(R1,RF)                                              
*                                                                               
UPLRRWOK STC   RF,RRWLN            SAVE NEW LENGTH                              
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
UPLRNK   NTR1                                                                   
         CLI   UPLDATA,C'C'                                                     
         BE    *+8                                                              
         CLI   UPLDATA,C'R'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   0(1,R3),UPLDATA                                                  
         SR    R1,R1                                                            
         IC    R1,UPLLEN                                                        
         SH    R1,=H'02'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,UPLDATA+1(0)                                                 
         ZAP   DUB,DUB             CHANGE X'0F' SIGN TO X'0C' SIGN              
         CVB   R1,DUB                                                           
         STC   R1,1(R3)                                                         
*                                                                               
UPLRNK90 B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  UPLOAD KEYWORD AND ITS PARAMETERS                                  *         
*                                                                     *         
*  INPUT:                                                             *         
*    R3 - ADDRESS OF WHERE TO INSERT THE KEYWORD AND ITS PARAMETERS   *         
*    R6 - ELMSUBEL                                                    *         
*    UPLLEN                                                           *         
*    UPLDATA                                                          *         
*    ELEMENT                                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
         USING DEFTABD,R8                                                       
UPLKYW   NTR1                                                                   
         LA    R2,ELEMENT                                                       
         MVC   TEMPFLD,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,UPLLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TEMPFLD(0),UPLDATA                                               
         MVC   TEMPFLDH+5(1),UPLLEN                                             
         TM    RCLOPT,RCLEQU       EQUATIONS?                                   
         BO    UPLKYW60                                                         
         MVI   REPMODE,REPCOL                                                   
         GOTO1 VALDEF,TEMPFLDH                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LR    R8,R1                                                            
         OC    SVSECLVL,DEFSEC#    SAVE OFF SECURITY LEVEL                      
         ST    R8,DEFENTRY         SAVE KEYWORD ENTRY ADDRESS                   
*                                                                               
         MVC   APPARM+8(2),=C',='                                               
         MVC   APPARM+10(1),SCCOMMA        GENERAL C','                         
         MVC   APPARM+11(1),APOPNPRN       GENERAL C'('                         
         GOTO1 VSCANNER,APPARM,TEMPFLDH,(6,BLOCK)                               
         SR    R0,R0                                                            
         IC    R0,APPARM+4                                                      
         BCTR  R0,0                                                             
         STC   R0,CPARMS                                                        
*                                                                               
         CLC   CPARMS,DEFCLMIN                                                  
         BL    UPLKWLOW            **ERROR** TOO FEW PARAMETERS                 
         CLC   CPARMS,DEFCLMAX                                                  
         BH    UPLKWHIH            **ERROR** TOO MANY PARAMETERS                
         CLI   CPARMS,0            ANY  PARAMETERS ?                            
         BE    UPLKYW60            NO,  SKIP PARAMETERS CHECK                   
*                                                                               
*                                  USER FIELD (SPECIAL) ?                       
         CLC   DEFCODE+1(2),=AL2(AC#RSUSF)                                      
         BNE   UPLKYW30            NO,  PROCESS PARAMETERS                      
*                                                                               
         BAS   RE,VALRUF           YES, VALIDATE USER FIELDS                    
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS?                                 
         BE    UPLKYW60            NO,  CONTINUE                                
         B     UPLKYWER            YES, EXIT                                    
*                                                                               
UPLKYW30 DS    0H                  VALIDATE THE PARAMETERS                      
         GOTO1 VALPARM,APPARM,ELEMENT,(CPARMS,BLOCK),DEFENTRY,0                 
         BNE   UPLKYWER                                                         
*                                                                               
UPLKYW60 BAS   RE,UPLCHR01         INSERT THE  KEYWORD AND PARAMETERS           
         B     UPLKYW90            RETURN                                       
*                                                                               
UPLKWLOW MVC   FVMSGNO,=AL2(ACE2FEW)      TOO  FEW  PARAMETERS                  
         B     UPLKYWER                                                         
*                                                                               
UPLKWHIH MVC   FVMSGNO,=AL2(ACE2MANY)     TOO  MANY PARAMETERS                  
*        B     UPLKYWER                                                         
*                                                                               
UPLKYWER LA    RF,FVXTRA           FIND   END  OF   FVXTRA                      
*                                                                               
UPLKYWE1 CLI   0(RF),C' '          CHARACTER   BLANK ?                          
         BE    UPLKYWE2            YES,   CONTINUE                              
         LA    RF,1(,RF)           NEXT   CHARACTER                             
         B     UPLKYWE1            LOOP                                         
*                                                                               
UPLKYWE2 MVI   0(RF),C':'          FOLLOW THE  PARAMETER WITH ':'               
         MVI   1(RF),C' '                 AND  BLANK                            
         MVC   2(12,RF),TEMPFLD           AND  THE  USER DATA                   
*                                                                               
UPLKYW90 B     XIT                 RETURN                                       
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE USER FIELD INPUT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALRUF   NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,CPARMS                                                        
         LA    RE,BLOCK+32                                                      
*                                                                               
VALRUF10 CLI   0(RE),2             LENGTH OF PARAMETER                          
         BNH   VALRUF20                                                         
         MVC   FVMSGNO,=AL2(ACEIVUF)                                            
         MVC   FVXTRA(3),12(RE)    PASS THE  1ST  3 BYTES OF BAD DATA           
         B     VALRUF90                                                         
*                                                                               
VALRUF20 LA    RE,32(,RE)          BUMP TO NEXT BLOCK PARAMETER                 
         BCT   RF,VALRUF10                                                      
*                                                                               
VALRUF90 B     XIT                                                              
         EJECT ,                                                                
UPLCDT   OI    0(R3),X'80'                                                      
         CLI   UPLDATA,C'A'        IS IT AFTER PARAMETER?                       
         BER   RE                                                               
         CLI   UPLDATA,C'P'        IS IT PRIOR PARAMETER?                       
         BER   RE                                                               
         B     UPLN2X                                                           
         EJECT ,                                                                
UPLN2X   NTR1                                                                   
         BAS   RE,EDPACK                                                        
         CVB   R1,DUB              CONVERT TO BINARY NUMBER                     
         CLI   ELMFLDLN,4          LENGTH OF 4 IS MAX                           
         BNH   *+6                                                              
         DC    H'00'                                                            
         LA    R2,1                                                             
         SR    RF,RF                                                            
         IC    RF,ELMFLDLN         FIELD LENGTH IN ELEMENT                      
         SLL   R2,0(RF)            POWERS OF 2                                  
         BCTR  R2,0                (2 TO THE POWER OF ELMFLDLN) -1              
         EX    R2,*+8                                                           
         B     *+8                                                              
         STCM  R1,0,0(R3)          STORE NUMBER                                 
         B     XIT                                                              
         EJECT ,                                                                
UPLN2P   NTR1                                                                   
         BAS   RE,EDPACK                                                        
         SR    RF,RF                                                            
         IC    RF,ELMFLDLN                                                      
         SH    RF,=H'01'                                                        
         BNM   *+6                                                              
         DC    H'00'                                                            
         SLL   RF,4                SHIFT TO HOB                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R3),DUB                                                      
         B     XIT                 THIS CONVERTS TO PACKED                      
         EJECT ,                                                                
UPLDTP   NTR1                      CREATE PACKED DATE                           
         CLI   UPLLEN,9            MUST BE DD MMM YY                            
         BNE   UPLNOTX                                                          
         LA    RF,12                                                            
         LA    RE,MONLIST                                                       
*                                                                               
UPLDTP20 CLC   UPLDATA+3(3),0(RE)  FIND MONTH                                   
         BE    UPLDTP25                                                         
         LA    RE,L'MONLIST(,RE)   TRY NEXT MONTH                               
         BCT   RF,UPLDTP20                                                      
         DC    H'00'               NO MONTH MATCHED                             
*                                                                               
UPLDTP25 SH    RF,=H'13'                                                        
         LPR   RF,RF               FIGURE OUT WHICH MONTH                       
         CVD   RF,DUB                                                           
         UNPK  WORK+2(2),DUB       MM OF YYMMDD                                 
         MVC   WORK(2),UPLDATA+7   YY OF YYMMDD FROM DD MMM YY                  
         MVC   WORK+4(2),UPLDATA   DD OF YYMMDD FROM DD MMM YY                  
         GOTO1 VDATCON,APPARM,(0,WORK),(1,(R3))                                 
*                                                                               
UPLDTP90 B     XIT                                                              
         EJECT ,                                                                
         USING RFLELD,R2                                                        
UPLRFL   NTR1                                                                   
         LA    R2,ELEMENT                                                       
         CLI   RFLTYPE,RFLTTYPE    TRANSACTION TYPE                             
         BE    UPLRFL10                                                         
         MVI   TMPLEN,2                                                         
         CLI   RFLTYPE,RFLBUDGT                                                 
         BE    UPLRFL40                                                         
         B     UPLCHR01            UPLOAD AS CHAR                               
*                                                                               
UPLRFL10 MVC   TEMPFLD,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,UPLLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TEMPFLD(0),UPLDATA                                               
         MVC   TEMPFLDH+5(1),UPLLEN                                             
         GOTO1 VSCANNER,APPARM,TEMPFLDH,(15,BLOCK),SCNP3NEQ                     
         SR    R4,R4                                                            
         IC    R4,APPARM+4         NUMBER OF PARAMETERS                         
         GOTO1 CNVTTYPE,APPARM,(C'N',BLOCK),((R4),(R3))                         
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1        LENGTH OF ELEMENT SO FAR                     
         AR    R1,R4               NUMBER OF ENTRIES IN SCAN BLOCK              
         STC   R1,ELEMENT+1        SAVE NEW LENGTH                              
         B     XIT                                                              
*                                                                               
UPLRFL20 BAS   RE,EDPACK           EDIT OUT FROM UPLDATA TO DUB                 
         CVB   RF,DUB                                                           
         STCM  RF,15,0(R3)                                                      
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1        ELEMENT LENGTH                               
         SR    RF,RF                                                            
         IC    RF,TMPLEN                                                        
         AR    R1,RF                                                            
         STC   R1,ELEMENT+1        RE-ADJUST ELEMENT SIZE                       
         B     XIT                 UPLOAD 4 BYTES OF INDICATORS                 
*                                                                               
         USING BUDRECD,R4                                                       
UPLRFL40 LA    R4,IOKEY                                                         
         OC    UPLDATA(L'BUDKEY),SPACES                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN                                                   
         MVC   BUDKCOD,UPLDATA                                                  
         GOTO1 AIO,IO3+IOACCDIR+IOHI                                            
         CLC   BUDKCOD,UPLDATA                                                  
         BNE   UPLRFL90                                                         
         MVC   0(2,R3),BUDKNO2                                                  
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1                                                     
         AH    R1,=H'02'                                                        
         STC   R1,ELEMENT+1                                                     
*                                                                               
UPLRFL90 B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT ,                                                                
UPLFLT   NTR1                                                                   
         MVC   0(1,R3),UPLDATA                                                  
         CLI   UPLLEN,1                                                         
         BNH   UPLFLT90                                                         
         CLI   UPLDATA,C'*'        EXCLUDE FILTER?                              
         BNE   UPLNOTX             SOMETHING WRONG                              
         MVC   0(1,R3),UPLDATA+1                                                
         NI    0(R3),TURNOFF-X'40' SET TO EXLCLUDE                              
*                                                                               
UPLFLT90 B     XIT                                                              
*                                                                               
UPLNOTX  XC    APELEM,APELEM                                                    
         B     XIT                                                              
         EJECT ,                                                                
DWLREC   CLI   APPFKEY,PFKDOWN                                                  
         BE    DWLREC02                                                         
         MVC   DISPADDR,PREVDISP                                                
         MVC   DISPAKYW,PREVAKYW                                                
*                                                                               
DWLREC02 XC    DISPIN2,DISPIN2     INITIALIZE DISP INTO SCREEN FIELD            
         MVI   CURLINE,1           START ON LINE ONE ON SCREEN                  
         SR    RF,RF                                                            
         ICM   RF,3,DISPADDR       DISPLACEMENT INTO AIO1                       
         AR    R2,RF               POINT INTO IOAREA                            
         TWAXC STORECH,STOLSTH                                                  
         MVI   STOTAB,C'X'         SET TO SAY MORE DATA                         
         OI    STOTABH+6,FVOXMT                                                 
         TM    INDKEY,KEYRDHI      WAS IT A POSSIBLE READ-HI ?                  
         BZ    DWLREC10            NO                                           
         L     R6,AIOAREA1         YES, SO VERIFY SAME RECORD                   
         CLC   KEYSAVE,0(R6)                                                    
         BNE   DWLREC12            NOT SAME SO FINISHED                         
*                                                                               
DWLREC10 CLI   0(R2),0             END OF   RECORD                              
         BNE   DWLREC14            FINISHED                                     
         CLI   IDKEY#,1            "INI" TYPE RECORD                            
         BNE   DWLREC12                                                         
         BAS   RE,KYWBLD                                                        
         BNE   DWLREC80            NOT FINSIHED YET                             
*                                                                               
DWLREC12 MVI   STOTAB,C' '         SET TO SAY NO MORE DATA                      
         B     DWLREC80            FINISHED                                     
*                                                                               
         USING ELMD,R6                                                          
DWLREC14 L     R6,AELMTAB          PROCESS AN ELEMENT                           
*                                                                               
DWLREC20 CLI   0(R6),EOT                                                        
         BE    DWLREC60            GET NEXT ELEMENT                             
         CLI   ELMKEYID,0          MATCH KEY AND IT'S ELEMENTS                  
         BE    *+10                                                             
         CLC   ELMKEYID,IDKEY#                                                  
         BNE   *+10                                                             
         CLC   ELMHEX,0(R2)        MATCH ON ELEMENT                             
         BE    DWLREC25                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ELMNTRY        NUMBER OF ENTRIES                            
         MH    RF,=Y(ELMLNQ2)                                                   
         LA    R6,ELMLNQ1(RF,R6)                                                
         B     DWLREC20                                                         
*                                                                               
DWLREC25 XC    SIZEOFEL,SIZEOFEL   INITIALIZE TO ZERO                           
         MVC   DWLCODE,=CL5'EL'                                                 
         MVI   DWLLEN,3                                                         
         MVI   DWLDATA,C' '                                                     
         MVC   DWLDATA+1(L'DWLDATA-1),DWLDATA                                   
         MVC   DWLDATA(3),ELMCODE                                               
         BAS   RE,PUTDATA          PUT DATA TO BLOCK                            
         XC    APELEM,APELEM                                                    
*                                                                               
DWLREC28 SR    RF,RF                                                            
         IC    RF,1(,R2)           ELEMENT LENGTH                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APELEM(0),0(R2)     MOVE INTO ELEMENT AREA                       
         SR    R1,R1                                                            
         ICM   R1,3,ELMNTRY        NUMBER OF ENTRIES                            
         LR    R0,R1                                                            
         LA    R6,ELMLNQ1(,R6)     BEGINNING OF ELEMENT COMPONENTS              
*                                                                               
         USING ELMSUBEL,R6                                                      
DWLREC30 MVC   DWLCODE,SPACES      SET CODE                                     
         TM    INOPT2,INOEXPD      IF NOT EXPANDING THEN USE SHORT CODE         
         BO    DWLREC32                                                         
         MVC   DWLCODE(L'ELMSHRT),ELMSHRT                                       
         B     DWLREC34                                                         
*                                                                               
DWLREC32 MVC   DWLCODE,ELMSCDE     SET CODE                                     
*                                                                               
DWLREC34 MVI   DWLLEN,0            INITIALIZE                                   
         MVI   DWLDATA,C' '                                                     
         MVC   DWLDATA+1(L'DWLDATA-1),DWLDATA                                   
         SR    R3,R3                                                            
         ICM   R3,1,ELMDISP                                                     
         LA    R3,APELEM(R3)       POINT TO ELEMENT DATA                        
         SR    RF,RF                                                            
         IC    RF,ELMDWL           ROUTINE NUMBER                               
         SLL   RF,2                MULTIPLY BY 4                                
         LA    RE,DWLREC40         RETURN ADDRESS                               
         B     *(RF)                                                            
*                                                                               
         B     DWNCHR           01 MOVE CHARACTER AND SCAN FOR "º"              
         B     DWNX2N           02 HEX TO NUMBER                                
         B     DWNP2N           03 PACK TO NUMBER                               
         B     DWNPDT           04 PACK DATE TO DD MMM YY                       
         B     DWNCDT           05 SPECIAL FOR COLUMN DATE FIELDS               
         B     DWNRFL           06 SPECIAL FOR RFLELD                           
         B     DWNFLT           07 SPECIAL FILTERS                              
         B     DWNRNK           08 RANK FOR PROFILE                             
         B     DWNCH2           09 ELEMENT LEN - LEN OF OBJECT                  
         B     DWNRCL           10 SPECIAL FOR COL ELEMENT                      
         B     DWNRRW           11 SPECIAL FOR ROW ELEMENT                      
*                                                                               
DWLREC40 BNE   DWLREC45                                                         
         BAS   RE,PUTDATA          PUT DATA TO BLOCK                            
*                                                                               
DWLREC45 LA    R6,ELMLNQ2(,R6)     NEXT ENTRY                                   
         BCT   R0,DWLREC30                                                      
         BAS   RE,PUTSCR           PUT ELEMENT DATA TO SCREEN                   
         BNE   DWLREC80                                                         
*                                                                               
DWLREC60 SR    RF,RF                                                            
         IC    RF,1(,R2)           GET TO NEXT ELEMENT                          
         AR    R2,RF                                                            
         B     DWLREC10                                                         
*                                                                               
DWLREC80 MVC   PREVDISP,DISPADDR                                                
         S     R2,AIOAREA1         SUBTRACT BASE OF IOAREA                      
         STH   R2,DISPADDR         SAVE DISPLACEMENT INTO AIOAREA               
         B     EXIT                                                             
         EJECT ,                                                                
         USING RFLELD,R2                                                        
DWNRFL   NTR1                                                                   
         LA    R2,APELEM                                                        
         CLI   RFLTYPE,RFLTTYPE    TRANSACTION TYPE                             
         BE    DWNRFL10                                                         
         MVI   TMPLEN,15           LENGTH FOR EXECUTE                           
         CLI   RFLTYPE,RFLBUDGT    BUDGET                                       
         BE    DWNRFL40            DOWNLOAD BUDGET NAME                         
         B     DWNCHR01            DOWNLOAD AS CHAR                             
*                                                                               
DWNRFL10 MVC   WORK,SPACES                                                      
         GOTO1 CNVTTYPE,APPARM,(C'S',APELEM),(0,WORK)                           
         LA    RE,WORK+L'WORK-1                                                 
         LA    RF,L'WORK                                                        
*                                                                               
DWNRFL12 CLI   0(RE),C' '                                                       
         BH    DWNRFL14                                                         
         BCTR  RE,0                                                             
         BCT   RF,DWNRFL12                                                      
*                                                                               
DWNRFL14 LA    RE,WORK             BEGINNING GO STRING                          
         TM    RFLIND,RFLXCLD      EXCLUDE BIT?                                 
         BZ    DWNRFL16                                                         
         BCTR  RF,0                                                             
         LA    RE,WORK+1           POINT PAST C'*' FOR EXCLUDE                  
*                                                                               
DWNRFL16 STC   RF,DWLLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   DWLDATA(0),0(RE)    MOVE IN DATA                                 
         CLI   DWLLEN,0                                                         
         BE    *+6                                                              
         SR    R7,R7                                                            
         LTR   R7,R7                                                            
         B     XIT                                                              
*                                                                               
DWNRFL20 SR    R1,R1                                                            
         IC    R1,TMPLEN                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         ICM   RF,0,RFLDATA                                                     
         CVD   RF,DUB                                                           
         B     DWNX2N10            DOWNLOAD AS NUMBER (4 BYTES)                 
*                                                                               
         USING BUDRECD,R4                                                       
DWNRFL40 LA    R4,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN                                                   
         MVC   BUDKNO1,RFLDATA                                                  
         MVC   APHALF,RFLDATA                                                   
         GOTO1 AIO,IO3+IOACCDIR+IOHI                                            
         CLC   BUDKNO1,APHALF                                                   
         BNE   DWNRFL90                                                         
         LA    RF,L'BUDKCOD        FIND OUT LEN OF BUD CODE                     
         LA    RE,BUDKCOD-1(RF)                                                 
*                                                                               
DWNRFL45 CLI   0(RE),C' '                                                       
         BNE   DWNRFL50                                                         
         BCTR  RE,0                                                             
         BCT   RF,DWNRFL45                                                      
         DC    H'00'                                                            
*                                                                               
DWNRFL50 STC   RF,DWLLEN                                                        
         MVC   DWLDATA(L'BUDKCOD),BUDKCOD                                       
         SR    RE,RE                                                            
*                                                                               
DWNRFL90 B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT ,                                                                
DWNCDT   NTR1                                                                   
         MVC   APHALF,0(R3)                                                     
         LH    RF,APHALF                                                        
         CVD   RF,DUB                                                           
         TM    APHALF,X'80'        IS BIT ON                                    
         BZ    DWNX2N10                                                         
         NI    APHALF,TURNOFF-X'80'                                             
         OC    APHALF,APHALF                                                    
         BNZ   DWNX2N10                                                         
         MVI   DWLLEN,1                                                         
         MVI   DWLDATA,C'P'        FOR PRIOR                                    
         CLI   ELMDISP,RCLSTDT-RCLELD                                           
         BE    XIT                                                              
         MVI   DWLDATA,C'A'        FOR AFTER                                    
         CLI   ELMDISP,RCLENDT-RCLELD                                           
         B     XIT                                                              
         EJECT ,                                                                
DWNCH2   NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,ELMFLDLN         LENGTH OF FIELD                              
         SR    R1,R1                                                            
         IC    R1,APELEM+1         LENGTH OF ELEMENT                            
         SR    R1,RF                                                            
         CLM   R1,1,APELEM+1                                                    
         BNL   DWNCHR90            ELEMENT AIN'T THAT BIG                       
         AR    R3,R1               POINT TO ELEMENT DATA                        
         B     DWNCHR12                                                         
         EJECT ,                                                                
DWNCHR   NTR1                                                                   
DWNCHR01 SR    RF,RF                                                            
         ICM   RF,1,ELMFLDLN       LENGTH OF DATA                               
         BNZ   DWNCHR10                                                         
         IC    RF,APELEM+1         LENGTH OF ELEMENT                            
         SR    R1,R1                                                            
         ICM   R1,1,ELMDISP        DISPLACMENT                                  
         SR    RF,R1               VARIABLE LENGTH ELEMENT                      
*                                                                               
DWNCHR10 CLC   ELMDISP,APELEM+1                                                 
         BNL   DWNCHR90            ELEMENT AIN'T THAT BIG                       
*                                                                               
DWNCHR12 SH    RF,=H'01'                                                        
         BM    DWNCHR90            NOT TO GOOD                                  
         EX    RF,*+4                                                           
         MVC   DWLDATA(0),0(R3)                                                 
*                                                                               
         LA    RE,DWLDATA(RF)      POINT TO END OF DATA                         
         LA    RF,1(,RF)                                                        
DWNCHR14 DS    0H                                                               
         CLI   0(RE),C' '          FIND LENGTH OF DATA                          
         BH    DWNCHR20                                                         
         BCTR  RE,0                                                             
         BCT   RF,DWNCHR14                                                      
         B     DWNCHR90            NO DATA                                      
*                                                                               
DWNCHR20 STC   RF,DWLLEN           SAVE LENGTH                                  
         LA    RE,DWLDATA                                                       
*                                                                               
DWNCHR30 CLI   0(RE),MYSEP                                                      
         BNE   *+8                                                              
         MVI   0(RE),C'\'          REPLACE THE SEPARATOR "º"                    
         LA    RE,1(,RE)                                                        
         BCT   RF,DWNCHR30                                                      
*                                                                               
         SR    R7,R7               SET CC TO OK                                 
DWNCHR90 LTR   R7,R7                                                            
         B     XIT                 RETURN                                       
         EJECT ,                                                                
         USING RCLELD,R3                                                        
DWNRCL   NTR1                                                                   
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)          LENGTH OF DATA                               
         BZ    DWNRCLNO                                                         
         STC   RF,DWLLEN           LENGTH OF DATA                               
         LA    R3,APELEM                                                        
         LA    RE,RCLNDATA                                                      
         CLI   ELMSHRT,C'L'        DATA PART ?                                  
         BNE   DWNRCL10                                                         
         BCTR  RF,0                                                             
         EX    RF,EXDWNDAT         MOVE HEADING INTO DWLDATA                    
         B     DWNRCLOK                                                         
*                                                                               
DWNRCL10 SR    R1,R1                                                            
         IC    R1,RCLDATLN                                                      
         AR    RE,R1               POINT TO HEADING 1                           
         CLI   ELMSHRT,C'1'        HEADING 1                                    
         BNE   DWNRCL20                                                         
         BCTR  RF,0                                                             
         EX    RF,EXDWNDAT         MOVE HEADING INTO DWLDATA                    
         B     DWNRCLOK                                                         
*                                                                               
DWNRCL20 IC    R1,RCLHD1LN                                                      
         AR    RE,R1               POINT TO HEADING 2                           
         CLI   ELMSHRT,C'2'        HEADING 2                                    
         BNE   DWNRCLNO                                                         
         BCTR  RF,0                                                             
         EX    RF,EXDWNDAT         MOVE HEADING INTO DWLDATA                    
*                                                                               
DWNRCLOK SR    RE,RE               SET CC = OK                                  
DWNRCLNO LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
         USING RRWELD,R3                                                        
DWNRRW   NTR1                                                                   
         LA    R3,APELEM                                                        
         SR    RF,RF                                                            
         IC    RF,RRWDATLN                                                      
         MVC   DWLLEN,RRWDATLN     LENGTH OF DATA                               
         LA    RE,RRWNDATA                                                      
         CLI   ELMSHRT,C'G'        RRWNDATA ?                                   
         BNE   DWNRRW10            NO                                           
         BCTR  RF,0                                                             
         EX    RF,EXDWNDAT         MOVE IN RRWNDATA                             
         B     DWNRRWOK                                                         
*                                                                               
DWNRRW10 AR    RE,RF               POINT TO PREFIX                              
         CLI   ELMSHRT,C'H'        PREFIX ?                                     
         BNE   DWNRRWNO            NO                                           
         SR    R1,R1                                                            
         ICM   R1,1,RRWPFXLN       LENGTH OF PREFIX                             
         BZ    DWNRRWNO            NO PREFIX ATTACHED                           
         STC   R1,DWLLEN           LENGTH OF DATA                               
         BCTR  R1,0                                                             
         EX    R1,EXDWNDAT         MOVE IN PREFIX                               
*                                                                               
DWNRRWOK SR    RE,RE                                                            
DWNRRWNO LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
EXDWNDAT MVC   DWLDATA(0),0(RE)                                                 
         EJECT ,                                                                
DWNRNK   NTR1                                                                   
         CLI   0(R3),C'R'                                                       
         BE    *+8                                                              
         CLI   0(R3),C'C'                                                       
         BNE   DWNRNK99                                                         
         MVC   DWLDATA(1),0(R3)    MOVE IN "R" OR "C"                           
         SR    R1,R1                                                            
         IC    R1,1(,R3)           GET NUMBER                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVC   DWLDATA+1(1),WORK+1                                              
         MVI   DWLLEN,2                                                         
         CLI   WORK,C'0'                                                        
         BE    DWNRNK90                                                         
         MVC   DWLDATA+1(2),WORK                                                
         MVI   DWLLEN,3                                                         
*                                                                               
DWNRNK90 SR    R7,R7               SET CC TO OK                                 
*                                                                               
DWNRNK99 LTR   R7,R7                                                            
         B     XIT                 RETURN                                       
         EJECT ,                                                                
DWNP2N   NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,ELMFLDLN         GET LENGTH OF DATA                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,0(0,R3)         PUT IT IN DUB                                
         B     DWNX2N10                                                         
         SPACE 2                                                                
*                                                                               
DWNX2N   NTR1                                                                   
         SR    R1,R1                                                            
         CLI   ELMFLDLN,4                                                       
         BL    DWNX2N05                                                         
         BE    *+6                                                              
         DC    H'00'               BAD TABEL ENTRY                              
         MVC   APFULL,0(R3)        BOUNDRY ALIGNE                               
         L     RF,APFULL                                                        
         CVD   RF,DUB                                                           
         B     DWNX2N10                                                         
*                                                                               
DWNX2N05 CLI   ELMFLDLN,2          HALF WORD?                                   
         BNE   DWNX2N06                                                         
         MVC   APHALF,0(R3)        BOUNDRY ALIGNED                              
         LH    RF,APHALF                                                        
         CVD   RF,DUB                                                           
         B     DWNX2N10                                                         
*                                                                               
DWNX2N06 IC    R1,ELMFLDLN         GET LENGTH OF DATA                           
         SR    RF,RF                                                            
         EX    R1,*+8                                                           
         B     *+8                                                              
         ICM   RF,0,0(R3)          GET VALUE                                    
         CVD   RF,DUB                                                           
*                                                                               
DWNX2N10 MVI   SIGN,0              RESET SIGN VALUE                             
         CP    DUB,=P'0'                                                        
         BE    DWNX2N90                                                         
         BP    *+8                                                              
         OI    SIGN,NEGATIVE                                                    
         UNPK  APWORK(10),DUB                                                   
         OI    APWORK+9,X'F0'      REMOVE THE SIGN HOB                          
         LA    RF,10                                                            
         LA    RE,APWORK                                                        
         CLI   0(RE),C'0'                                                       
         BNE   DWNX2N20                                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,*-12                                                          
         LA    RF,1(,RF)           ADD ONE TO LENGTH                            
         BCTR  RE,0                BUMP BACK ONE SPACE TO ZERO                  
         B     DWNX2N30                                                         
*                                                                               
DWNX2N20 TM    SIGN,NEGATIVE       MINUS AMOUNT?                                
         BZ    DWNX2N30                                                         
         LA    RF,1(,RF)           ADD ONE TO LENGTH                            
         BCTR  RE,0                BUMP BACK ONE SPACE TO PUT MINUS             
         MVI   0(RE),C'-'          SET MINUS SIGN                               
*                                                                               
DWNX2N30 STC   RF,DWLLEN           SAVE LENGTH                                  
         EX    RF,*+4                                                           
         MVC   DWLDATA(0),0(RE)                                                 
         SR    R7,R7               SET CC TO OK                                 
*                                                                               
DWNX2N90 LTR   R7,R7                                                            
         B     XIT                                                              
         EJECT ,                                                                
DWNPDT   NTR1                                                                   
         OC    0(3,R3),0(R3)       ANY DATA                                     
         BZ    DWNPDTX                                                          
         GOTO1 VDATCON,APPARM,(1,(R3)),(11,APWORK)                              
         MVI   DWLLEN,9                                                         
         MVC   DWLDATA(2),APWORK+3                                              
         MVC   DWLDATA+3(3),APWORK                                              
         MVC   DWLDATA+7(2),APWORK+6                                            
         SR    R7,R7                                                            
*                                                                               
DWNPDTX  LTR   R7,R7                                                            
         B     XIT                                                              
         EJECT ,                                                                
DWNFLT   NTR1                                                                   
         CLI   0(R3),0             ANY DATA?                                    
         BE    DWNFLTX                                                          
         MVI   DWLLEN,1                                                         
         MVC   DWLDATA(1),0(R3)    MOVE IN DATA                                 
         TM    DWLDATA,X'40'       IS IT EXCLUDE                                
         BO    DWNFLT10            NO                                           
         MVI   DWLLEN,2                                                         
         MVI   DWLDATA,C'*'                                                     
         MVC   DWLDATA+1(1),0(R3)  MOVE IN DATA                                 
         OI    DWLDATA+1,X'40'                                                  
*                                                                               
DWNFLT10 SR    R7,R7                                                            
*                                                                               
DWNFLTX  LTR   R7,R7                                                            
         B     XIT                                                              
         EJECT ,                                                                
         USING KEYD,R6                                                          
         USING NAMELD,R1                                                        
VALACT   L     R6,AKEYNTRY                                                      
         TWAXC STORECH,STOLSTH                                                  
         TM    KEYIND,KEYNME       SHOW NAME                                    
         BZ    VALACT10                                                         
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,NAMELQ     X'20'                                        
         GOTO1 GETEL                                                            
         BNE   VALACT10                                                         
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    VALACT10                                                         
         EX    RF,*+4                                                           
         MVC   STOREC(0),NAMEREC                                                
         DROP  R1                                                               
*                                                                               
         USING BUDRECD,R2                                                       
SAV      USING BUDRECD,KEYSAVE                                                  
*                                                                               
VALACT10 L     R2,AIOAREA1                                                      
         CLI   KEYID,5             BUDGET TYPE                                  
         BNE   VALACT15                                                         
         CLC   BUDKCOD,SAV.BUDKCOD                                              
         BNE   IVALSET                                                          
         LR    R1,R2                                                            
         MVI   APELCODE,BIVELQ     X'1C'                                        
         LA    R4,BLOCK+32         SECOND PARAMETER                             
         SR    R0,R0                                                            
         IC    R0,NPARMS                                                        
         BCTR  R0,0                                                             
         GOTO1 GETEL                                                            
*                                                                               
         USING BIVELD,R1                                                        
VALACT12 BNE   IVALBUDA                                                         
*                                                                               
VALACT13 CLC   BIVAUNT(2),12(R4)   MATCH  UNIT/LEDGER                           
         BE    VALACT14                                                         
         LA    R4,32(,R4)          NEXT   PARAMETER                             
         BCT   R0,VALACT13                                                      
*                                                                               
         IC    R0,NPARMS           RESET                                        
         BCTR  R0,0                                                             
         LA    R4,BLOCK+32         SECOND PARAMETER                             
         GOTO1 NEXTEL                                                           
         B     VALACT12                                                         
*                                                                               
VALACT14 MVC   FVMSGNO,=AL2(FVFOK)                                              
         DROP  R1,R2                                                            
         DROP  SAV                                                              
*                                                                               
         USING LITELD,R1                                                        
VALACT15 CLI   KEYID,10            LIST RECORDS                                 
         BNE   VALACT20                                                         
         LR    R1,R2                                                            
         LA    R4,BLOCK+32         SECOND PARAMETER                             
         MVI   APELCODE,LITELQ     X'1E' LIST DATA                              
         MVI   BYTE,0                                                           
         MVC   ULSAVE,SPACES                                                    
         CLI   12(R4),C'B'         BILLING SOURCE                               
         BNE   *+14                                                             
         MVC   ULSAVE,=C'ME'                                                    
         MVI   BYTE,LITTMED                                                     
*                                                                               
         CLI   12(R4),C'A'         ACCOUNT LIST                                 
         BNE   *+8                                                              
         MVI   BYTE,LITTACT                                                     
*                                                                               
         CLI   12(R4),C'C'         CLIENT LIST                                  
         BNE   *+14                                                             
         MVC   ULSAVE,=C'SJ'                                                    
         MVI   BYTE,LITTACT                                                     
*                                                                               
         CLI   12(R4),C'W'         WORKCODE                                     
         BNE   *+8                                                              
         MVI   BYTE,LITTWRK                                                     
         GOTO1 GETEL                                                            
*                                                                               
VALACT16 BNE   IVALSET                                                          
         CLC   BYTE,LITTYPE        MATCH TYPE                                   
         BE    VALACT18                                                         
         GOTO1 NEXTEL                                                           
         B     VALACT16                                                         
*                                                                               
         USING LIDELD,R1                                                        
VALACT18 CLC   ULSAVE,SPACES       VERIFY UNIT/LEDGER FOR LIST                  
         BE    VALACT19                                                         
         LR    R1,R2                                                            
         MVI   APELCODE,LIDELQ     X'1F' LIST DATA                              
         GOTO1 GETEL                                                            
         BNE   IVALSET                                                          
         CLC   LIDDLEDG,ULSAVE                                                  
         BNE   IVALSET                                                          
*                                                                               
VALACT19 MVC   FVMSGNO,=AL2(FVFOK)                                              
         DROP  R1                                                               
*                                                                               
VALACT20 LA    R4,BLOCK                                                         
         CLI   KEYID,15            ONE  BYTE OFFICE OR                          
         BL    VALACTX                                                          
         CLI   KEYID,16            TWO  BYTE OFFICE ?                           
         BH    VALACTX             NO,  RETURN                                  
         ZIC   R2,0(,R4)           GET  LENGTH                                  
*                                  VALIDATE THE OFFICE/OFFICE LIST              
         GOTO1 VOFFICE,APPARM,('VOFTYPE1',12(R4)),('NO',(R2))                   
         BNE   EXIT                BAD  INPUT, EXIT                             
*                                                                               
         CLI   APPARM,X'00'        IS   THIS AN OFFICE LIST ?                   
         BE    VALACTX             NO,  RETURN                                  
         LA    R1,STORECH                                                       
         SR    RF,RF                                                            
         IC    RF,0(,R1)           BUMP TO NEXT FIELD                           
         AR    R1,RF                                                            
         MVC   8(4,R1),=C'LIST'                                                 
*                                                                               
VALACTX  B     XIT                                                              
         EJECT ,                                                                
EDPACK   NTR1                                                                   
         MVI   SIGN,0                                                           
         SR    RF,RF                                                            
         IC    RF,UPLLEN           LENGTH OF DATA IN UPLDATA                    
         LA    RE,UPLDATA          LENGTH                                       
         LR    R1,RF                                                            
         CLI   0(RE),C'-'          FIND MINUS                                   
         BE    EDPACK10                                                         
         LA    RE,1(,RE)                                                        
         BCT   R1,*-12                                                          
         LR    R1,RF               NO MINUS - RESTORE LENGTH                    
         LA    RE,UPLDATA          RESET ADDRESS                                
         B     EDPACK30                                                         
*                                                                               
EDPACK10 LA    RE,1(,RE)           BUMP PAST MINUS                              
         OI    SIGN,NEGATIVE                                                    
         SH    R1,=H'01'           LENGTH IS ONE LESS                           
         BNZ   EDPACK30                                                         
         LR    R1,RF               RESTORE LENGTH                               
         BCTR  R1,0                ONE FOR MINUS                                
         LA    RE,UPLDATA                                                       
*                                                                               
EDPACK30 BCTR  R1,0                ONE FOR EXECUTE                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         ZAP   DUB,DUB             CHANGE X'0F' SIGN TO X'0C' SIGN              
         TM    SIGN,NEGATIVE                                                    
         BZ    *+10                                                             
         MP    DUB,=P'-1'          MAKE NEGATIVE                                
         B     XIT                                                              
         EJECT ,                                                                
PUTDATA  NTR1                                                                   
         L     RE,AELMBLK          BASE OF BLOCK FOR AN ELEMENT                 
         AH    RE,SIZEOFEL         END OF RECORD OR SIZE SO FAR                 
         MVC   0(5,RE),DWLCODE                                                  
         LA    RF,5                MAX OF FIVE                                  
*                                                                               
PUTDATA2 DS    0H                                                               
         CLI   0(RE),C' '                                                       
         BE    PUTDATA4                                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,PUTDATA2                                                      
*                                                                               
PUTDATA4 DS    0H                                                               
         MVI   0(RE),C'='                                                       
         SR    RF,RF                                                            
         IC    RF,DWLLEN           LENGTH OF DATA TO DOWNLOAD                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   1(0,RE),DWLDATA                                                  
         LA    RE,2(RF,RE)         POINT TO END OF DATA                         
         MVI   0(RE),MYSEP         MARK END WITH SEPARATOR "º"                  
         LA    RE,1(,RE)           ADD ONE                                      
         S     RE,AELMBLK          GET LENGTH OF ELEMENT DATA SO FAR            
         STH   RE,SIZEOFEL                                                      
         B     XIT                                                              
         EJECT ,                                                                
PUTSCR   NTR1                                                                   
         L     R3,AELMBLK          BASE OF BLOCK FOR AN ELEMENT                 
         LA    R2,STORECH                                                       
         ST    R2,SVR2             INITIALIZE TO FIRST LINE                     
         TM    INOPT2,INOEXPD                                                   
         BZ    *+10                                                             
         XC    DISPIN2,DISPIN2     FORCE ELEMENT ON NEW LINE                    
         SR    RF,RF                                                            
         IC    RF,CURLINE          CURRENT LINE TO PUT DATA                     
         SH    RF,=H'01'                                                        
         BZ    PUTSCR20            ON FIRST LINE SO ALREADY THERE               
         SR    R1,R1                                                            
*                                                                               
PUTSCR10 IC    R1,0(,R2)           LENGTH OF FIELD                              
         AR    R2,R1               BUMP TO NEXT                                 
         C     R2,AEOFSRN          AT END OF SCREEN?                            
         BNL   PUTSCRX             NO ROOM LEFT, PAST END OF SCREEN             
         BCT   RF,PUTSCR10                                                      
         ST    R2,SVR2             SAVE STARTING LINE                           
*                                                                               
PUTSCR20 IC    R1,0(,R2)           LENGTH OF FIELD                              
         AR    RF,R1               ADD UP SPACE AVAILABLE                       
         SH    RF,=H'08'                                                        
         TM    1(R2),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    RF,=H'08'                                                        
         AR    R2,R1               BUMP TO NEXT FIELD                           
         C     R2,AEOFSRN          COMPARE TO END OF SCREEN                     
         BL    PUTSCR20                                                         
         SH    RF,DISPIN2          DISPLACMENT INTO LAST LINE                   
*                                                                               
         CH    RF,SIZEOFEL         RF=AVAILABLE SPACE                           
         BL    PUTSCRX             NOT ENOUGH ROOM SET CC                       
*                                                                               
         L     R2,SVR2             RESTORE CURRENT LINE ADDRESS                 
         LH    RF,SIZEOFEL         LENGTH OF DATA TO PUT TO SCREEN              
*                                                                               
PUTSCR50 SR    R1,R1                                                            
         IC    R1,0(,R2)           LENGTH OF FIELD                              
         ST    R1,SVR1             SAVE LENGTH OF HEADER + FIELD                
         SH    R1,=H'08'                                                        
         TM    1(R2),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    R1,=H'08'                                                        
         SH    R1,DISPIN2                                                       
         LA    R4,8(,R2)           POINT TO SCREEN FIELD                        
         AH    R4,DISPIN2          ADD DISPLACMENT INTO LINE                    
         XC    DISPIN2,DISPIN2     CLEAR AFTER FIRST TIME                       
         CR    R1,RF               WILL THE REST FIT ON THIS LINE?              
         BNH   PUTSCR55            NO                                           
         LR    R1,RF               USE LENGTH OF STRING                         
         TM    INOPT2,INOEXPD                                                   
         BO    PUTSCR55                                                         
         LA    R6,8(,R2)           START OF FIELD                               
         LA    RE,0(R4,R1)         ADD STRING LENGTH +1 INTO FIELD              
         SR    RE,R6                                                            
         STH   RE,DISPIN2          SAVE NEW DISP INTO THIS LINE                 
*                                                                               
PUTSCR55 BCTR  R1,0                ONE FOR EXECUTE                              
         EX    R1,*+4                                                           
         MVC   0(0,R4),0(R3)       MOVE DATA TO SCREEN                          
         LA    R3,1(R1,R3)         BUMP UP IN DATA TO MOVE                      
         LA    R1,1(,R1)           ADJUST BACK BECAUSE OF EXECUTE               
         SR    RF,R1               REDUCE NUMBER OF CHARS TO SCREEN             
         BZ    PUTSCR60                                                         
         BP    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         A     R2,SVR1             BUMP TO NEXT FIELD SCREEN HEADER             
         IC    R0,CURLINE                                                       
         AH    R0,=H'01'                                                        
         STC   R0,CURLINE                                                       
         B     PUTSCR50                                                         
*                                                                               
PUTSCR60 OC    DISPIN2,DISPIN2     IF ZERO GO TO NEXTLINE                       
         BNZ   PUTSCR65                                                         
         IC    R0,CURLINE          NOTHING LEFT BUMP UP A LINE                  
         AH    R0,=H'01'                                                        
         STC   R0,CURLINE                                                       
*                                                                               
PUTSCR65 SR    R7,R7                                                            
*                                                                               
PUTSCRX  LTR   R7,R7                                                            
         B     XIT                 SET CC TO NOT OK                             
         EJECT ,                                                                
***********************************************************************         
*  MOVE SCREEN IN TO CONTIGUOUS AREA                                  *         
***********************************************************************         
         SPACE 1                                                                
BLDBLOCK NTR1                                                                   
         L     R3,AELMBLK                                                       
         L     RF,LASTELN          LENGTH OF DATA                               
         AR    R3,RF               POINT TO NEW LOCATION                        
         XC    LASTELN,LASTELN     RESET                                        
*                                                                               
BLDBLK10 LA    R2,STORECH                                                       
         SR    RF,RF                                                            
*                                                                               
BLDBLK20 IC    RF,0(,R2)           LENGTH OF FIELD                              
         SH    RF,=H'08'                                                        
         TM    1(R2),X'02'         EXTENDED FIELD HEADER                        
         BZ    *+8                 NO                                           
         SH    RF,=H'08'                                                        
         LA    R4,8(,R2)           POINT TO DATA                                
         SH    RF,=H'01'                                                        
         BM    BLDBLK90            FIELD HAS NO LENGTH (BAD NEWS)               
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R4)       MOVE IN DATA                                 
         AR    R3,RF               POINT TO LAST CHARACTER                      
         LR    R6,R3               SAVE OFF IN CASE NEED TO RESET               
         LA    R0,1(,RF)           LOOP FOR LENGTH OF DATA ON NON-SPACE         
         CLI   0(R3),C' '          CHECK FOR SPACES OR NULLS                    
         BH    BLDBLK30            IT IS OK                                     
*                                                                               
BLDBLK25 CLI   0(R3),C' '          CHECK FOR SPACES OR NULLS                    
         BH    BLDBLK26                                                         
         BCTR  R3,0                DECREMENT TO REMOVE SPACE OR NULL            
         BCT   R0,BLDBLK25         IF DROPS THROUGH HERE, MEANS                 
*                                  WHOLE LINE WAS BLANK, SEE IF VALID           
BLDBLK26 CLI   0(R3),MYSEP         IS IT A SEPARATOR "º"                        
         BNE   BLDBLK28                                                         
         LA    R3,1(,R3)           BUMP PAST LAST CHARACTER                     
         B     BLDBLK90            IF = THEN MUST BE FINISHED                   
*                                                                               
BLDBLK28 LR    R3,R6               ELSE RESET R3 TO FULL LENGTH                 
*                                                                               
BLDBLK30 LA    R3,1(,R3)           BUMP PAST LAST CHARACTER                     
         IC    RF,0(,R2)           GET LENGTH AGAIN                             
         AR    R2,RF               POINT TO NEXT SCREEN HEADER                  
         C     R2,AEOFSRN          AT END OF SCREEN?                            
         BL    BLDBLK20                                                         
*                                                                               
BLDBLK90 MVI   0(R3),EOFDATA       MARK AS END OF DATA                          
         ST    R3,AEOFBLK                                                       
         B     XIT                                                              
         EJECT ,                                                                
         USING KDWD,R6                                                          
         USING DEFTABD,R8                                                       
KYWBLD   NTR1                                                                   
         L     R6,AKDWTAB          KEYWORD DOWNLOAD ENTRY                       
         SR    RF,RF                                                            
         ICM   RF,3,DISPAKYW                                                    
         AR    R6,RF                                                            
*                                                                               
KYWBLD05 CLI   0(R6),EOT           END OF TABLE ?                               
         BE    KYWBLD85            YES SO FINISHED                              
         CLC   OPTVER#,KDWVER#                                                  
         BH    KYWBLD80                                                         
         CLC   OPTMOD#,KDWMOD#                                                  
         BNL   KYWBLD80                                                         
         CLC   HIGHVER#(3),KDWVER#                                              
         BH    KYWBLD05                                                         
         MVC   HIGHVER#(3),KDWVER#                                              
*                                                                               
KYWBLD08 XC    SIZEOFEL,SIZEOFEL                                                
         MVC   DWLCODE,=CL5'EL'                                                 
         MVI   DWLLEN,3                                                         
         MVI   DWLDATA,C' '                                                     
         MVC   DWLDATA+1(L'DWLDATA-1),DWLDATA                                   
         MVC   DWLDATA(3),=C'KYW'                                               
         BAS   RE,PUTDATA          BUFFER DATA                                  
*                                                                               
         MVC   DWLCODE,SPACES                                                   
         MVI   DWLCODE,C'A'                                                     
         MVI   DWLLEN,0                                                         
         MVI   DWLDATA,C' '                                                     
         MVC   DWLDATA+1(L'DWLDATA-1),DWLDATA                                   
*                                                                               
         MVC   TEMPFLD,SPACES                                                   
         MVI   TEMPFLDH+5,L'KDWCODE                                             
         MVC   TEMPFLD(L'KDWCODE),KDWCODE                                       
         CLI   KDWCODE,ESCHIGHQ                                                 
         BNL   KYWBLD10                                                         
         GOTO1 VDICTAT,APPARM,C'SU  ',TEMPFLD,0                                 
*                                                                               
KYWBLD10 MVI   REPMODE,REPEVRY                                                  
         GOTO1 VALDEF,TEMPFLDH                                                  
         BE    *+6                                                              
         DC    H'00'               KEYWORD NOT FOUND ?                          
*                                                                               
         LR    R8,R1                                                            
         LA    R2,DWLDATA                                                       
*                                                                               
         MVC   0(2,R2),=C'n='       KEYWORD NARRATIVE                           
         LA    R2,2(,R2)                                                        
         TM    KDWIND1,KDWHPNAR                                                 
         BZ    KYWBLD12                                                         
         TM    DEFIND,DEFGTXT      GET TEXT VALUE ?                             
         BZ    KYWBLD12            NO                                           
         SR    RF,RF                                                            
         ICM   RF,3,DEFHELP#                                                    
         BZ    KYWBLD12                                                         
         GOTO1 TEXTGET,APPARM,(C'S',(RF)),(40,(R2)),0                           
         LA    RF,40                                                            
         B     KYWBLD18                                                         
*                                                                               
KYWBLD12 SR    RF,RF                                                            
         CLI   KDWLN,KDWNARR-KDWD                                               
         BH    KYWBLD14                                                         
         MVC   0(L'APKEYHD,R2),APKEYHD                                          
         LA    RF,L'APKEYHD                                                     
         B     KYWBLD18                                                         
*                                                                               
KYWBLD14 IC    RF,KDWLN                                                         
         SH    RF,=Y(KDWNARR-KDWD+1)                                            
         EX    RF,*+4                                                           
         MVC   0(L'KDWNARR,R2),KDWNARR                                          
*                                                                               
KYWBLD18 LA    R2,1(R2,RF)                                                      
         BAS   RE,BACKUP                                                        
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
         MVC   0(2,R2),=C'c='                                                   
         LA    R2,2(,R2)                                                        
         MVC   0(L'KDWCODE,R2),APKEYWRD                                         
         LA    R2,L'KDWCODE(,R2)                                                
         LA    RF,L'KDWCODE                                                     
         BAS   RE,BACKUP                                                        
         TM    KDWIND1,KDWCN       SPECIAL CODE/NAME FORM ?                     
         BZ    *+6                 NO                                           
         BCTR  R2,0                BACK ONE TO WIPE OUT C"C" OR C"N"            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
         MVC   0(2,R2),=C't='                                                   
         LA    R2,2(,R2)                                                        
         TM    DEFIND,DEFROW       ROW     KEYWORD ?                            
         BZ    KYWBLD22                                                         
         MVI   0(R2),C'r'                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
KYWBLD22 TM    DEFIND,DEFCOL       COLUMN  KEYWORD ?                            
         BZ    KYWBLD24            NO                                           
         MVI   0(R2),C'c'                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
KYWBLD24 TM    DEFIND,DEFHEAD      HEADING KEYWORD ?                            
         BZ    KYWBLD25            NO                                           
         MVI   0(R2),C'h'                                                       
         LA    R2,1(,R2)                                                        
         TM    DEFIND,DEFNOFUT     not valid in footer ?                        
         BO    KYWBLD25                                                         
         MVI   0(R2),C'f'                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
KYWBLD25 MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         TM    KDWIND1,KDWLV4      SET LEVELS ON                                
         BZ    KYWBLD26                                                         
         MVC   0(4,R2),=C'l=4,'                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
KYWBLD26 TM    DEFCLIND,DEFCLCDE+DEFCLNME                                       
         BO    KYWBLD28                                                         
         TM    DEFRWIND,DEFRWCDE+DEFRWNME                                       
         BZ    KYWBLD30                                                         
*                                                                               
KYWBLD28 MVC   0(4,R2),=C'p=cn'                                                 
         LA    R2,4(,R2)                                                        
         TM    DEFRWIND,DEFRWADR                                                
         BZ    KYWBLD30                                                         
         MVI   0(R2),C'a'          SET TO SAY ADDRESS PARAMETER                 
         LA    R2,1(,R2)                                                        
*                                                                               
KYWBLD30 MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         TM    DEFCLOTH,DEFCLAMT   AMOUNT TYPE COLUMN ?                         
         BO    KYWBLD32                                                         
         SR    R1,R1                                                            
         ICM   R1,1,DEFWDTH        GET WIDTH                                    
         BZ    KYWBLD32                                                         
         MVC   0(2,R2),=C'w='                                                   
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  2(2,R2),APDUB                                                    
         LA    R2,4(,R2)                                                        
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
KYWBLD32 TM    DEFTYPE,DEFCHAR                                                  
         BO    KYWBLD38                                                         
         TM    DEFTYPE,DEFPACK                                                  
         BZ    KYWBLD34                                                         
         LA    RF,2                                                             
         MVC   0(2,R2),=C'$,'                                                   
         TM    DEFTYPE,DEFRATE                                                  
         BZ    *+14                                                             
         LA    RF,5                                                             
         MVC   0(5,R2),=C'rate'                                                 
*                                                                               
         TM    DEFCLIND,DEFCLJBR   JOBBER TYPE ?                                
         BZ    *+14                                                             
         LA    RF,9                                                             
         MVC   0(9,R2),=C'$NOBASIS,'                                            
         LA    R2,2(RF,R2)                                                      
*                                                                               
KYWBLD34 TM    DEFTYPE,DEFDTE1                                                  
         BZ    KYWBLD38                                                         
         MVC   0(4,R2),=C'dte,'                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
         USING RCLELD,R3                                                        
KYWBLD38 LA    R3,APWORK                                                        
         XC    APWORK,APWORK                                                    
         MVI   RCLEL,RCLELQ                                                     
         MVI   RCLLN,RCLNLNQ                                                    
         GOTO1 MKHEAD,APPARM,APWORK                                             
         SR    RF,RF                                                            
         IC    RF,RCLDATLN                                                      
         LA    RE,RCLNDATA(RF)     POINT TO FIRST HEADING                       
*                                                                               
         ICM   RF,1,RCLHD1LN                                                    
         BZ    KYWBLD39            NO HEADING 1                                 
         MVC   0(2,R2),=C'1='                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   2(0,R2),0(RE)                                                    
         LA    R2,3(RF,R2)         POINT TO NEXT AREA                           
         LA    RF,1(,RF)           LENGTH OF HEADING                            
         AR    RE,RF               POINT TO END OF HEADING 1                    
         ST    RE,SVRE                                                          
         BAS   RE,BACKUP           REMOVE BLANK SPACES                          
         L     RE,SVRE                                                          
*                                                                               
KYWBLD39 ICM   RF,1,RCLHD2LN       HEADING 2 ?                                  
         BZ    KYWBLD50            NO                                           
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(2,R2),=C'2='                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   2(0,R2),0(RE)                                                    
         LA    R2,3(RF,R2)                                                      
         LA    RF,1(,RF)           LENGTH OF HEADING                            
         BAS   RE,BACKUP           REMOVE EXTRANEOUS BLANKS                     
         DROP  R3                                                               
*                                                                               
KYWBLD50 MVC   APWORK(4),DEFREPS                                                
         NC    APWORK(4),=AL4(REPRCV)                                           
         BZ    KYWBLD52                                                         
         MVC   0(4,R2),=C',RCV'                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
KYWBLD52 MVC   APWORK(4),DEFREPS                                                
         NC    APWORK(4),=AL4(REPINC)                                           
         BZ    KYWBLD54                                                         
         MVC   0(4,R2),=C',INC'                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
KYWBLD54 MVC   APWORK(4),DEFREPS                                                
         NC    APWORK(4),=AL4(REPPAY)                                           
         BZ    KYWBLD56                                                         
         MVC   0(4,R2),=C',PAY'                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
KYWBLD56 MVC   APWORK(4),DEFREPS                                                
         NC    APWORK(4),=AL4(REPEXP)                                           
         BZ    KYWBLD58                                                         
         MVC   0(4,R2),=C',EXP'                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
KYWBLD58 MVC   APWORK(4),DEFREPS                                                
         NC    APWORK(4),=AL4(REPPROD)                                          
         BZ    KYWBLD60                                                         
         MVC   0(5,R2),=C',PROD'                                                
         LA    R2,5(,R2)                                                        
*                                                                               
KYWBLD60 MVC   APWORK(4),DEFREPS                                                
         NC    APWORK(4),=AL4(REPCOST)                                          
         BZ    KYWBLD62                                                         
         MVC   0(5,R2),=C',COST'                                                
         LA    R2,5(,R2)                                                        
*                                                                               
KYWBLD62 LA    RE,DWLDATA          BUFFER DATA                                  
         SR    R2,RE                                                            
         STC   R2,DWLLEN                                                        
         BAS   RE,PUTDATA          BUFFER DATA                                  
         BAS   RE,PUTSCR           FLUSH  BUFFER TO SCREEN                      
         BNE   KYWBLD90                                                         
*                                                                               
KYWBLD80 SR    RF,RF                                                            
         IC    RF,KDWLN                                                         
         AR    R6,RF                                                            
         B     KYWBLD05            PUT ANOTHER                                  
*                                                                               
KYWBLD85 XC    SIZEOFEL,SIZEOFEL                                                
         MVC   DWLCODE,=CL5'EL'                                                 
         MVI   DWLLEN,3                                                         
         MVI   DWLDATA,C' '                                                     
         MVC   DWLDATA+1(L'DWLDATA-1),DWLDATA                                   
         MVC   DWLDATA(3),=C'VER'                                               
         BAS   RE,PUTDATA          BUFFER DATA                                  
*                                                                               
         MVC   DWLCODE,SPACES                                                   
         MVI   DWLCODE,C'A'                                                     
         MVI   DWLLEN,9                                                         
         MVI   DWLDATA,C' '                                                     
         MVC   DWLDATA+1(L'DWLDATA-1),DWLDATA                                   
         SR    RF,RF               CONVERT VERSION NUMBER TO CHAR               
         ICM   RF,3,HIGHVER#                                                    
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  DWLDATA(5),APDUB                                                 
*                                                                               
         MVI   DWLDATA+5,C'.'                                                   
         SR    RF,RF               CONVERT MODIFICATION NUMBER TO CHAR          
         ICM   RF,1,HIGHMOD#                                                    
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  DWLDATA+6(3),APDUB                                               
         BAS   RE,PUTDATA          BUFFER DATA                                  
         BAS   RE,PUTSCR                                                        
         BE    KYWBLDOK                                                         
*                                                                               
KYWBLD90 MVC   PREVAKYW,DISPAKYW                                                
         S     R6,AKDWTAB                                                       
         STCM  R6,3,DISPAKYW                                                    
         B     KYWBLDNO                                                         
*                                                                               
KYWBLDOK SR    RE,RE                                                            
*                                                                               
KYWBLDNO LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R6,R8                                                            
         EJECT ,                                                                
***********************************************************************         
*  BACKUP UNTIL FIND HIGHER THEN SPACE X'40'                          *         
*     RF = MAX NUMBER OF CHARACTERS TO LOOK AT                        *         
*     R2 = REGISTER TO BACK TRACK ON                                  *         
***********************************************************************         
         SPACE 1                                                                
BACKUP   CLI   0(R2),C' '          LOOK FOR BLANKS                              
         BH    BACKUP8                                                          
         BCTR  R2,0                                                             
         BCT   RF,BACKUP                                                        
         DC    H'00'                                                            
*                                                                               
BACKUP8  LA    R2,1(,R2)           WENT ONE TOO FAR SO FIX IT                   
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  INVALID ERRORS TO DISPLAY ON TOP OF SCREEN                         *         
***********************************************************************         
         USING KEYD,R6                                                          
IVALSET  L     R6,AKEYNTRY                                                      
         MVC   FVMSGNO,KEYERR#                                                  
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALBUDA MVC   FVMSGNO,=AL2(0101)                                               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALUPL  MVC   FVMSGNO,=AL2(2090)                                               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT ,                                                                
DMWRITE  DC    CL8'DMWRT'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
         EJECT ,                                                                
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT ,                                                                
GETCODE  NMOD1 0,*GCOD*                                                         
         L     RC,APALOCAL                                                      
         L     R2,0(,R1)           BEGINNING OF SCAN AREA                       
         CLI   0(R2),EOFDATA       END OF SCAN BLOCK?                           
         BE    GETCDE98                                                         
         LA    R3,UPLCODE          AREA TO PUT DATA                             
         SR    R6,R6                                                            
         SR    RF,RF                                                            
         LR    RF,R2                                                            
         LA    R0,6                MUST MATCH WITHIN FOUR                       
*                                                                               
GETCDE08 CLI   0(RF),C'='                                                       
         BE    GETCDE10                                                         
         CLI   0(RF),EOFDATA       END OF SCAN BLOCK                            
         BE    GETCDE98                                                         
         LA    RF,1(,RF)           BUMP UP                                      
         BCT   R0,GETCDE08                                                      
         DC    H'00'                                                            
*                                                                               
GETCDE10 MVI   UPLLEN,0            SET TO ZERO                                  
         MVI   UPLFLG,0            SET TO NONE PROCESSED                        
         MVC   UPLCODE,SPACES      INITIALIZE                                   
         MVI   UPLDATA,C' '                                                     
         MVC   UPLDATA+1(L'UPLDATA-1),UPLDATA                                   
         SR    RF,R2                                                            
         BP    *+6                                                              
         DC    H'00'               LENGTH OF ZERO NO GOOD                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   UPLCODE(0),0(R2)                                                 
         LA    RE,2(RF,R2)         BUMP PAST "=" SIGN                           
*                                                                               
         LA    R0,L'UPLDATA        MAX FOR ANY SINGLE TYPE                      
         LR    RF,RE               SAVE LOCATION                                
*                                                                               
GETCDE30 CLI   0(RF),MYSEP         THIS IS A SEPARATOR "º"                      
         BE    GETCDE50                                                         
         CLI   0(RF),EOFDATA       END OF BLOCK                                 
         BE    GETCDE98                                                         
         LA    RF,1(,RF)           BUMP UP                                      
         BCT   R0,GETCDE30                                                      
         DC    H'00'                                                            
*                                                                               
GETCDE50 ST    RF,SVRF             END OF DATA                                  
         SR    RF,RE               LENGTH OF DATA                               
         BNP   GETCDE90            NO DATA                                      
         STC   RF,UPLLEN           LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   UPLDATA(0),0(RE)                                                 
*                                                                               
         USING ELMD,R6                                                          
         L     R6,AELMTAB                                                       
         CLC   UPLCODE,=CL5'EL'    PROCESS A NEW ELEMENT                        
         BNE   GETCDE60                                                         
         MVC   APELEM,ELEMENT      SAVE LAST ONE BUILT                          
         XC    ELEMENT,ELEMENT     NEW ELEMENT                                  
         OI    UPLFLG,UPLELM       UPLOAD ELEMENT                               
*                                                                               
GETCDE55 CLI   0(R6),EOT                                                        
         BNE   *+6                                                              
         DC    H'00'               ELEMENT NOT RECOGNIZED                       
         CLC   ELMCODE,UPLDATA     MATCH WHICH ELEMENT                          
         BNE   GETCDE58                                                         
         CLI   ELMKEYID,0                                                       
         BE    *+10                                                             
         CLC   ELMKEYID,IDKEY#                                                  
         BNE   GETCDE58                                                         
         MVC   ELEMENT(1),ELMHEX   ELEMENT NUMBER IN HEX                        
         MVC   ELEMENT+1(1),ELMLEN MOVE IN DEFAULT LENGTH                       
         B     GETCDE90                                                         
*                                                                               
GETCDE58 SR    RF,RF                                                            
         ICM   RF,3,ELMNTRY        NUMBER OF ENTRIES                            
         MH    RF,=Y(ELMLNQ2)                                                   
         LA    R6,ELMLNQ1(RF,R6)   POINT TO NEXT ENTRY                          
         B     GETCDE55                                                         
*                                                                               
GETCDE60 CLI   0(R6),EOT                                                        
         BNE   *+6                                                              
         DC    H'00'               ELEMENT NOT RECOGNIZED                       
         CLC   ELMHEX,ELEMENT      MATCH ON ELEMENT NUMBER                      
         BE    GETCDE70                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ELMNTRY        NUMBER OF ENTRIES                            
         MH    RF,=Y(ELMLNQ2)                                                   
         LA    R6,ELMLNQ1(RF,R6)   POINT TO NEXT ENTRY                          
         B     GETCDE60                                                         
*                                                                               
GETCDE70 SR    RF,RF                                                            
         ICM   RF,3,ELMNTRY        NUMBER OF ENTRIES                            
         LA    R6,ELMLNQ1(,R6)     BEGINNING OF ELEMENT COMPONENTS              
*                                                                               
         USING ELMSUBEL,R6                                                      
         CLC   ELMSHRT,UPLCODE     UPLOAD ALWAYS USES SHORT CODE                
         BE    GETCDE90                                                         
         LA    R6,ELMLNQ2(,R6)     NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         DC    H'00'               UNKNOWN TYPE                                 
         OI    UPLFLG,UPLELCMP     UPLOAD ELEMENT COMPONENT                     
*                                                                               
GETCDE90 L     RF,SVRF                                                          
         LA    RF,1(,RF)           BUMP PAST SEPARATOR "º"                      
         ST    RF,0(,R1)           P1 = NEXT POSITION IN BLOCK                  
         ST    R6,4(,R1)           P2 = SAVE ELEMENT ENTRY                      
         B     GETCDEEX            RETURN                                       
*                                                                               
GETCDE98 XC    4(4,R1),4(R1)                                                    
         MVI   0(R1),X'FF'         MARK AS EOF DATA                             
*                                                                               
GETCDEEX DS    0H                  RETURN TO CALLER                             
         XMOD1                                                                  
         DROP  R6                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* TABLE OF MONTHS                                                     *         
***********************************************************************         
         SPACE 1                                                                
MONTAB   DS    0C                                                               
         DCDDL AC#JAN,4                                                         
         DCDDL AC#FEB,4                                                         
         DCDDL AC#MAR,4                                                         
         DCDDL AC#APR,4                                                         
         DCDDL AC#MAY,4                                                         
         DCDDL AC#JUN,4                                                         
         DCDDL AC#JUL,4                                                         
         DCDDL AC#AUG,4                                                         
         DCDDL AC#SEP,4                                                         
         DCDDL AC#OCT,4                                                         
         DCDDL AC#NOV,4                                                         
         DCDDL AC#DEC,4                                                         
MONTABX  DC    X'00'                                                            
         EJECT ,                                                                
LDGINI   DC    C'R',AL1(08),C'SASBSR'                                           
         DC    C'I',AL1(08),C'SISK1C'                                           
         DC    C'L',AL1(04),C'1R'                                               
         DC    C'V',AL1(04),C'SJ'                                               
*&&US*&& DC    C'P',AL1(22),C'SPSQSSSTSUSVSWSXSY2C'                             
*&&UK*&& DC    C'P',AL1(22),C'SFSTSVSX'                                         
*&&US*&& DC    C'X',AL1(12),C'SESFSL2DSB'                                       
*&&UK*&& DC    C'X',AL1(12),C'SESL2DSB'                                         
*&&US*&& DC    C' ',AL1(46)                                                     
*&&US*&& DC    C'SASBSESFSISJSKSLSPSQSRSSSTSUSVSWSXSY1C1R2C2D'                  
*&&UK*&& DC    C' ',AL1(26)                                                     
*&&UK*&& DC    C'SASBSESFSISJSLSRSVSX1C1R2D'                                    
         DC    AL1(0)                                                           
         EJECT ,                                                                
***********************************************************************         
* RECORD TYPE TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
KEYTAB   DS    0F                                                               
KYINI    DC    CL3'INI',X'80',AL1(01),AL2(0000,(KYINIX-*)/KEYLNQ2+1)            
KYINIX   DC    CL5'RTYPE',AL1(01,04,00,00)                                      
*                                                                               
KYLDG    DC    CL3'LDG',X'80',AL1(02),AL2(0000,(KYLDGX-*)/KEYLNQ2+1)            
         DC    CL5'KCPY ',AL1(01,02,00,LDGKCPY-LDGRECD)                         
KYLDGX   DC    CL5'KUNT ',AL1(02,03,01,LDGKUNT-LDGRECD)                         
*                                                                               
KYRES    DC    CL3'RES',X'81',AL1(03),AL2(0000,(KYRESX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,RESKTYPQ,RESKTYP-RESRECD)                   
         DC    CL5'KSUB ',AL1(01,01,RESKSUBQ,RESKSUB-RESRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,RESKCPY-RESRECD)                         
KYRESX   DC    CL5'KFORM',AL1(08,03,01,RESKFORM-RESRECD)                        
*                                                                               
KYTXT    DC    CL3'TXT',X'90',AL1(07),AL2(0000,(KYTXTX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,RESKTYPQ,RESKTYP-RESRECD)                   
         DC    CL5'KSUB ',AL1(01,01,RESKSUBQ,RESKSUB-RESRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,RESKCPY-RESRECD)                         
         DC    CL5'KFORM',AL1(08,03,01,RESKFORM-RESRECD)                        
KYTXTX   DC    CL5'KSEQ ',AL1(01,01,RESKSTXT,RESKSEQ-RESRECD)                   
*                                                                               
KYACT    DC    CL3'ACT',X'A0',AL1(04),AL2(0000,(KYACTX-*)/KEYLNQ2+1)            
         DC    CL5'KCPY ',AL1(01,02,00,ACTKCPY-ACTRECD)                         
KYACTX   DC    CL5'KUNT ',AL1(14,03,01,ACTKUNT-ACTRECD)                         
*                                                                               
KYBUD    DC    CL3'BUD',X'50',AL1(05),AL2(0053,(KYBUDX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,BUDKTYPQ,BUDKTYP-BUDRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,BUDKCPY-BUDRECD)                         
KYBUDX   DC    CL5'KCOD ',AL1(10,05,01,BUDKCOD-BUDRECD)                         
*                                                                               
KYCLI    DC    CL3'CLI',X'A0',AL1(06),AL2(0000,(KYCLIX-*)/KEYLNQ2+1)            
         DC    CL5'KCPY ',AL1(01,02,00,ACTKCPY-ACTRECD)                         
         DC    CL5'KUNT ',AL1(01,01,C'S',ACTKUNT-ACTRECD)                       
         DC    CL5'KLDG ',AL1(01,01,C'J',ACTKLDG-ACTRECD)                       
KYCLIX   DC    CL5'KACT ',AL1(12,03,01,ACTKACT-ACTRECD)                         
*                                                                               
KYLST    DC    CL3'LST',X'A0',AL1(10),AL2(1416,(KYLSTX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,LSTKTYPQ,LSTKTYP-LSTRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,LSTKCPY-LSTRECD)                         
KYLSTX   DC    CL5'KLST ',AL1(05,05,01,LSTKLST-LSTRECD)                         
*                                                                               
KYOF1    DC    CL3'OFF',X'A0',AL1(15),AL2(0000,(KYOF1X-*)/KEYLNQ2+1)            
         DC    CL5'KCPY ',AL1(01,02,00,ACTKCPY-ACTRECD)                         
         DC    CL5'KUNT ',AL1(01,01,C'2',ACTKUNT-ACTRECD)                       
         DC    CL5'KLDG ',AL1(01,01,C'D',ACTKLDG-ACTRECD)                       
KYOF1X   DC    CL5'KACT ',AL1(01,03,01,ACTKACT-ACTRECD)                         
*                                                                               
KYOF2    DC    CL3'OFF',X'A0',AL1(16),AL2(0000,(KYOF2X-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,OFFKTYPQ,OFFKTYP-OFFRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,OFFKCPY-OFFRECD)                         
KYOF2X   DC    CL5'KOFF ',AL1(02,03,01,OFFKOFF-OFFRECD)                         
*                                                                               
KYOGR    DC    CL3'OGR',X'60',AL1(19),AL2(0000,(KYOGRX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,OGRKTYPQ,OGRKTYP-OGRRECD)                   
         DC    CL5'KSUB ',AL1(01,01,OGRKGRPQ,OGRKSUB-OGRRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,OGRKCPY-OGRRECD)                         
         DC    CL5'KUNT ',AL1(01,01,C'S',OGRKUNT-OGRRECD)                       
         DC    CL5'KLDG ',AL1(01,01,C'J',OGRKLDG-OGRRECD)                       
KYOGRX   DC    CL5'KCODE',AL1(01,03,01,OGRKCODE-OGRRECD)                        
*                                                                               
KYMGR    DC    CL3'MGR',X'60',AL1(20),AL2(0000,(KYMGRX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,MGRKTYPQ,MGRKTYP-MGRRECD)                   
         DC    CL5'KSUB ',AL1(01,01,MGRKSUBQ,MGRKSUB-MGRRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,MGRKCPY-MGRRECD)                         
         DC    CL5'KUNT ',AL1(01,01,C'S',MGRKUNT-MGRRECD)                       
         DC    CL5'KLDG ',AL1(01,01,C'J',MGRKLDG-MGRRECD)                       
KYMGRX   DC    CL5'KCODE',AL1(01,03,01,MGRKCODE-MGRRECD)                        
*                                                                               
KYWGR    DC    CL3'WGR',X'60',AL1(21),AL2(0000,(KYWGRX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,WGRKTYPQ,WGRKTYP-WGRRECD)                   
         DC    CL5'KSUB ',AL1(01,01,WGRKSUBQ,WGRKSUB-WGRRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,WGRKCPY-WGRRECD)                         
         DC    CL5'KUNT ',AL1(01,01,C'S',WGRKUNT-WGRRECD)                       
         DC    CL5'KLDG ',AL1(01,01,C'J',WGRKLDG-WGRRECD)                       
KYWGRX   DC    CL5'KCODE',AL1(01,03,01,WGRKCODE-WGRRECD)                        
*                                                                               
KYSTU    DC    CL3'STU',X'60',AL1(22),AL2(0000,(KYSTUX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,STUKTYPQ,STUKTYP-STURECD)                   
         DC    CL5'KSUB ',AL1(01,01,STUKSUBQ,STUKSUB-STURECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,STUKCPY-STURECD)                         
KYSTUX   DC    CL5'KCODE',AL1(04,03,01,STUKCODE-STURECD)                        
*                                                                               
KYTSK    DC    CL3'TSK',X'A0',AL1(06),AL2(0000,(KYTSKX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,WCOKTYPQ,WCOKTYP-WCORECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,WCOKCPY-WCORECD)                         
         DC    CL5'KUNT ',AL1(01,01,C'S',WCOKUNT-WCORECD)                       
         DC    CL5'KLDG ',AL1(01,01,C'J',WCOKLDG-WCORECD)                       
KYTSKX   DC    CL5'KWRK ',AL1(02,03,01,WCOKWRK-WCORECD)                         
*                                                                               
KYPMD    DC    CL3'PMD',X'A0',AL1(17),AL2(0000,(KYPMDX-*)/KEYLNQ2+1)            
         DC    CL5'KTYP ',AL1(01,01,PMDKTYPQ,PMDKTYP-PMDRECD)                   
         DC    CL5'KCPY ',AL1(01,02,00,PMDKCPY-PMDRECD)                         
KYPMDX   DC    CL5'KMED ',AL1(01,03,01,PMDKMED-PMDRECD)                         
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  ELEMENT TYPE TABLE                                                 *         
***********************************************************************         
         SPACE 1                                                                
ELMTAB   DS    0F                                                               
ELACL    DC    X'16',AL1(ACLLN1Q,01),CL3'ACL',AL2((ELACLX-*)/ELMLNQ2+1)         
         DC    CL6'AVLEN ',AL1(01,02,02,ACLVLEN-ACLELD)                         
         DC    CL6'BVDESC',AL1(15,01,01,ACLVDESC-ACLELD)                        
         DC    CL6'CVLEN ',AL1(01,02,02,ACLVLEN-ACLELD+L'ACLVALS*1)             
         DC    CL6'DVDESC',AL1(15,01,01,ACLVDESC-ACLELD+L'ACLVALS*1)            
         DC    CL6'EVLEN ',AL1(01,02,02,ACLVLEN-ACLELD+L'ACLVALS*2)             
         DC    CL6'FVDESC',AL1(15,01,01,ACLVDESC-ACLELD+L'ACLVALS*2)            
         DC    CL6'GVLEN ',AL1(01,02,02,ACLVLEN-ACLELD+L'ACLVALS*3)             
         DC    CL6'HVDESC',AL1(15,01,01,ACLVDESC-ACLELD+L'ACLVALS*3)            
ELACLX   DC    CL6'ILDG  ',AL1(38,09,01,00)                                     
*                                                                               
ELAL2    DC    X'16',AL1(ACLLN1Q,02),CL3'ACL',AL2((ELAL2X-*)/ELMLNQ2+1)         
         DC    CL6'AVLEN ',AL1(01,02,02,ACLVLEN-ACLELD)                         
         DC    CL6'BVDESC',AL1(15,01,01,ACLVDESC-ACLELD)                        
         DC    CL6'CVLEN ',AL1(01,02,02,ACLVLEN-ACLELD+L'ACLVALS*1)             
         DC    CL6'DVDESC',AL1(15,01,01,ACLVDESC-ACLELD+L'ACLVALS*1)            
         DC    CL6'EVLEN ',AL1(01,02,02,ACLVLEN-ACLELD+L'ACLVALS*2)             
         DC    CL6'FVDESC',AL1(15,01,01,ACLVDESC-ACLELD+L'ACLVALS*2)            
         DC    CL6'GVLEN ',AL1(01,02,02,ACLVLEN-ACLELD+L'ACLVALS*3)             
ELAL2X   DC    CL6'HVDESC',AL1(15,01,01,ACLVDESC-ACLELD+L'ACLVALS*3)            
*                                                                               
ELNAM    DC    X'20',AL1(NAMLN1Q,00),CL3'NAM',AL2((ELNAMX-*)/ELMLNQ2+1)         
ELNAMX   DC    CL6'AEREC ',AL1(00,01,01,NAMEREC-NAMELD)                         
*                                                                               
ELADR    DC    X'22',AL1(ADRLN1Q,00),CL3'ADR',AL2((ELADRX-*)/ELMLNQ2+1)         
         DC    CL6'1ADD1 ',AL1(26,01,01,ADRADD1-ADRELD)                         
         DC    CL6'2ADD2 ',AL1(26,01,01,ADRADD2-ADRELD)                         
         DC    CL6'3ADD3 ',AL1(26,01,01,ADRADD3-ADRELD)                         
         DC    CL6'4ADD4 ',AL1(26,01,01,ADRADD4-ADRELD)                         
ELADRX   DC    CL6'5ADD5 ',AL1(26,01,01,ADRADD5-ADRELD)                         
*                                                                               
ELFFN    DC    X'25',AL1(FFNLN2Q,03),CL3'FFN',AL2((ELFFNX-*)/ELMLNQ2+1)         
         DC    CL6'ATYPE ',AL1(01,01,01,FFNUMBER-FFNELD)                        
ELFFNX   DC    CL6'BCODE ',AL1(06,01,01,FFNUMBER+1-FFNELD)                      
*                                                                               
ELSTY    DC    X'25',AL1(STYLNQ2,03),CL3'STY',AL2((ELSTYX-*)/ELMLNQ2+1)         
         DC    CL6'ACODE ',AL1(01,01,01,STYCODE-STYELD)                         
         DC    CL6'BNAME ',AL1(06,01,01,STYNAME-STYELD)                         
         DC    CL6'CSEC#1',AL1(01,02,02,STYSEC#1-STYELD)                        
         DC    CL6'CSEC#5',AL1(01,02,02,STYSEC#5-STYELD)                        
ELSTYX   DC    CL6'DSTAT ',AL1(01,02,02,STYSTAT-STYELD)                         
*                                                                               
ELSEC    DC    X'25',AL1(SECLNQ,01),CL3'SEC',AL2((ELSECX-*)/ELMLNQ2+1)          
         DC    CL6'ADSGN ',AL1(01,01,01,SECDSGN-SECELD)                         
ELSECX   DC    CL6'BRQST ',AL1(01,01,01,SECRQST-SECELD)                         
*                                                                               
ELCTD    DC    X'30',AL1(166,01),CL3'CTD',AL2((ELCTDX-*)/ELMLNQ2+1)             
         DC    CL6'ASTPOW',AL1(04,01,01,CTDSTPOW-CTDSTD)                        
         DC    CL6'BSTNAM',AL1(33,01,01,CTDSTNAM-CTDSTD)                        
         DC    CL6'1STADD',AL1(33,01,01,CTDSTADD-CTDSTD)                        
         DC    CL6'2STAD2',AL1(33,01,01,CTDSTAD2-CTDSTD)                        
ELCTDX   DC    CL6'3STAD3',AL1(33,01,01,CTDSTAD3-CTDSTD)                        
*                                                                               
ELCTV    DC    X'34',AL1(12,01),CL3'CTV',AL2((ELCTVX-*)/ELMLNQ2+1)              
ELCTVX   DC    CL6'AALDST',AL1(10,01,01,CTVALDST-CTVALD)                        
*                                                                               
ELMET    DC    X'82',AL1(6,01),CL3'MET',AL2((ELMETX-*)/ELMLNQ2+1)               
         DC    CL6'ANUM  ',AL1(01,01,01,METNUM-METELD)                          
         DC    CL6'BCODE ',AL1(03,01,01,METCODE-METELD)                         
ELMETX   DC    CL6'CNAME ',AL1(00,01,01,METCODE-METELD+3)                       
*                                                                               
ELPAC    DC    X'A1',AL1(PACLNQ,00),CL3'PAC',AL2((ELPACX-*)/ELMLNQ2+1)          
         DC    CL6'APERS ',AL1(08,01,01,PACPERS-PACELD)                         
ELPACX   DC    CL6'BDATE ',AL1(06,04,04,PACDATE-PACELD)                         
*                                                                               
ELRHD    DC    X'C1',AL1(RHDLNQ,00),CL3'RHD',AL2((ELRHDX-*)/ELMLNQ2+1)          
         DC    CL6'ATYPE ',AL1(01,02,02,RHDTYPE-RHDELD)                         
         DC    CL6'BSEQ  ',AL1(01,02,02,RHDSEQ-RHDELD)                          
         DC    CL6'CFRM  ',AL1(01,02,02,RHDFRM-RHDELD)                          
         DC    CL6'DPSEQ ',AL1(01,02,02,RHDPSEQ-RHDELD)                         
ELRHDX   DC    CL6'EDATA ',AL1(00,01,01,RHDDATA-RHDELD)                         
*                                                                               
ELRRW    DC    X'C2',AL1(RRWNLNQ,00),CL3'RRW',AL2((ELRRWX-*)/ELMLNQ2+1)         
         DC    CL6'ASEQ  ',AL1(01,02,02,RRWSEQ-RRWELD)                          
         DC    CL6'BOPT  ',AL1(01,02,02,RRWOPT-RRWELD)                          
         DC    CL6'CTYPE ',AL1(01,01,01,RRWTYPE-RRWELD)                         
         DC    CL6'DOPT2 ',AL1(01,02,02,RRWOPT2-RRWELD)                         
         DC    CL6'EDATES',AL1(01,02,02,RRWDATES-RRWELD)                        
         DC    CL6'FPSEQ ',AL1(01,02,02,RRWPSEQ-RRWELD)                         
         DC    CL6'GDATA ',AL1(MXRDATLN,11,12,RRWDATLN-RRWELD)                  
ELRRWX   DC    CL6'HPRFX ',AL1(15,11,12,RRWPFXLN-RRWELD)                        
*                                                                               
ELRCL    DC    X'C3',AL1(RCLNLNQ,00),CL3'RCL',AL2((ELRCLX-*)/ELMLNQ2+1)         
         DC    CL6'ASEQ  ',AL1(01,02,02,RCLSEQ-RCLELD)                          
         DC    CL6'BWDTH ',AL1(01,02,02,RCLWDTH-RCLELD)                         
         DC    CL6'COPT  ',AL1(01,02,02,RCLOPT-RCLELD)                          
         DC    CL6'DOPT2 ',AL1(01,02,02,RCLOPT2-RCLELD)                         
         DC    CL6'ESORTN',AL1(01,02,02,RCLSORTN-RCLELD)                        
         DC    CL6'FSPCL ',AL1(01,02,02,RCLSPCL-RCLELD)                         
         DC    CL6'GSTACK',AL1(01,02,02,RCLSTACK-RCLELD)                        
         DC    CL6'HDATES',AL1(01,02,02,RCLDATES-RCLELD)                        
         DC    CL6'IDTEFG',AL1(01,02,02,RCLDTEFG-RCLELD)                        
         DC    CL6'JSTDT ',AL1(02,05,07,RCLSTDT-RCLELD)                         
         DC    CL6'KENDT ',AL1(02,05,07,RCLENDT-RCLELD)                         
         DC    CL6'LDATA ',AL1(MXCDATLN,10,11,RCLDATLN-RCLELD)                  
         DC    CL6'1HDL1 ',AL1(12,10,11,RCLHD1LN-RCLELD)                        
ELRCLX   DC    CL6'2HDL2 ',AL1(12,10,11,RCLHD2LN-RCLELD)                        
*                                                                               
ELRPF    DC    X'C4',AL1(RPFLNQ,00),CL3'RPF',AL2((ELRPFX-*)/ELMLNQ2+1)          
         DC    CL6'APOPT ',AL1(01,02,02,RPFPOPT-RPFELD)                         
         DC    CL6'BEDOPT',AL1(01,02,02,RPFEDOPT-RPFELD)                        
         DC    CL6'CPCTS ',AL1(01,01,01,RPFPCTS-RPFELD)                         
         DC    CL6'DRND  ',AL1(01,01,01,RPFRND-RPFELD)                          
         DC    CL6'ERKON ',AL1(02,08,09,RPFRKON-RPFELD)                         
         DC    CL6'FRKBY ',AL1(02,01,01,RPFRKBY-RPFELD)                         
         DC    CL6'GRKCL ',AL1(01,02,02,RPFRKCL-RPFELD)                         
         DC    CL6'HRKOR ',AL1(01,01,01,RPFRKOR-RPFELD)                         
         DC    CL6'1FLT1 ',AL1(01,07,08,RPFFLT1-RPFELD)                         
         DC    CL6'2FLT2 ',AL1(01,07,08,RPFFLT2-RPFELD)                         
         DC    CL6'3FLT3 ',AL1(01,07,08,RPFFLT3-RPFELD)                         
         DC    CL6'4FLT4 ',AL1(01,07,08,RPFFLT4-RPFELD)                         
         DC    CL6'IROPT ',AL1(01,02,02,RPFROPT-RPFELD)                         
         DC    CL6'JOPT1 ',AL1(01,02,02,RPFOPT1-RPFELD)                         
         DC    CL6'KOPT2 ',AL1(01,02,02,RPFOPT2-RPFELD)                         
         DC    CL6'LPOPT2',AL1(01,02,02,RPFPOPT2-RPFELD)                        
         DC    CL6'5FLT5 ',AL1(01,07,08,RPFFLT5-RPFELD)                         
         DC    CL6'MIND  ',AL1(01,02,02,RPFIND-RPFELD)                          
         DC    CL6'NOPT3 ',AL1(01,02,02,RPFOPT3-RPFELD)                         
         DC    CL6'OOPT4 ',AL1(01,02,02,RPFOPT4-RPFELD)                         
         DC    CL6'POPT5 ',AL1(01,02,02,RPFOPT5-RPFELD)                         
         DC    CL6'QBLTRN',AL1(01,01,01,RPFBLTRN-RPFELD)                        
         DC    CL6'RMETHD',AL1(01,01,01,RPFMETHD-RPFELD)                        
         DC    CL6'SDTFMT',AL1(01,02,02,RPFDTFMT-RPFELD)                        
         DC    CL6'TDNOPT',AL1(01,02,02,RPFDNOPT-RPFELD)                        
         DC    CL6'UESTST',AL1(01,01,01,RPFESTST-RPFELD)                        
         DC    CL6'VPOPT3',AL1(01,02,02,RPFPOPT3-RPFELD)                        
ELRPFX   DC    CL6'WNALPR',AL1(01,02,02,RPFNALPR-RPFELD)                        
*                                                                               
ELRFL    DC    X'C5',AL1(RFLLNQ,00),CL3'RFL',AL2((ELRFLX-*)/ELMLNQ2+1)          
         DC    CL6'ASEQ  ',AL1(01,02,02,RFLSEQ-RFLELD)                          
         DC    CL6'BTYPE ',AL1(01,02,02,RFLTYPE-RFLELD)                         
         DC    CL6'CIND  ',AL1(01,02,02,RFLIND-RFLELD)                          
ELRFLX   DC    CL6'DDATA ',AL1(00,06,06,RFLDATA-RFLELD)                         
*                                                                               
ELFFT    DC    X'DB',AL1(FFTLN1Q+1,03)                                          
         DC    CL3'FFT',AL2((ELFFTX-*)/ELMLNQ2+1)                               
         DC    CL6'ATYPE ',AL1(01,02,02,FFTTYPE-FFTELD)                         
         DC    CL6'BSEQ  ',AL1(01,02,02,FFTSEQ-FFTELD)                          
         DC    CL6'CDLEN ',AL1(01,02,02,FFTDLEN-FFTELD)                         
ELFFTX   DC    CL6'DDATA ',AL1(00,01,01,FFTDATA-FFTELD)                         
*                                                                               
ELFF2    DC    X'DB',AL1(FFTLN1Q+1,07)                                          
         DC    CL3'FFT',AL2((ELFF2X-*)/ELMLNQ2+1)                               
ELFF2X   DC    CL6'DDATA ',AL1(00,01,01,FFTDATA-FFTELD)                         
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
NEWKYW   DS    0F                                                               
         DC    AL1(NKWRCVCQ-*)                                                  
         DC    AL2(1),AL1(1,KDWCN+KDWHPNAR,0)                                   
         DCDD  AC#RSRCC,6                                                       
NKWRCVCQ EQU   *                                                                
*                                                                               
         DC    AL1(NKWCSTCQ-*)                                                  
         DC    AL2(1),AL1(1,KDWCN+KDWHPNAR+KDWLV4,0)                            
         DCDD  AC#RSCSC,6                                                       
NKWCSTCQ EQU   *                                                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
MYSEP    EQU   X'6A'               STEREO SEPARATOR = "º"                       
MAXELMBQ EQU   6000                                                             
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
SVREGS   DS    6A                                                               
SVRE     DS    A                                                                
SVRF     DS    A                                                                
SVR0     DS    A                                                                
SVR1     DS    A                                                                
SVR2     DS    A                                                                
SVR3     DS    A                                                                
SVR4     DS    A                                                                
SVR5     DS    A                                                                
SVR6     DS    A                                                                
*                                                                               
AKEYTAB  DS    A                   A(TABLE OF KEYS FOR RECORDS)                 
AKDWTAB  DS    A                   A(TABLE OF NEW KEYWORDS TO DOWNLOAD)         
AELMTAB  DS    A                   A(TABLE OF ELEMENT TO PROCESS)               
AELMBLK  DS    A                   BUILT DATA FROM SCREEN                       
AKEYNTRY DS    A                   A(KEY ENTRY FOUND)                           
AELEM25  DS    A                   A(X'25' ELEMENT)                             
ALASTEL  DS    A                   A(LAST ELEMENT IN ELMBLK)                    
AEOFSRN  DS    A                   A(END OF SCREEN ADDRESS)                     
AEOFBLK  DS    A                   A(END OF AELMBLK DATA)                       
*                                                                               
DEFENTRY DS    A                   A(CURRENT ENTRY IN DEFTAB)                   
*                                                                               
LASTELN  DS    F                                                                
DISPIN2  DS    H                   DISPLACMENT INTO SCREEN FIELD                
SIZEOFEL DS    H                   SIZE OF ELEMENT                              
*                                                                               
UPLCODE  DS    CL5                 SPECIAL CODE                                 
UPLLEN   DS    XL1                 LENGTH OF DATA                               
UPLFLG   DS    XL1                                                              
UPLELM   EQU   X'80'               UPLOAD ELEMENT COMPONENT                     
UPLELCMP EQU   X'40'               UPLOAD ELEMENT COMPONENT                     
UPLDATA  DS    CL255               DATA TO UPLOAD                               
*                                                                               
DWLCODE  DS    CL5                 SPECIAL CODE                                 
DWLLEN   DS    XL1                 LENGTH OF DATA                               
DWFLAG   DS    XL1                                                              
DWLDATA  DS    CL255               DATA TO UPLOAD                               
*                                                                               
EOFDATA  EQU   X'01'                                                            
*                                                                               
CURLINE  DS    XL1                                                              
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
IDKEY#   DS    AL1                                                              
INDKEY   DS    XL1                                                              
TEMPFLDH DS    XL8                                                              
TEMPFLD  DS    CL90                                                             
TMPLEN   DS    XL1                                                              
WORK     DS    CL64                                                             
BYTE     DS    CL1                                                              
SIGN     DS    XL1                                                              
NEGATIVE EQU   X'80'                                                            
SAVEELM  DS    CL1                 SAVE OFF LAST ELEMENT FROM UPLOAD            
TEXTREC  DS    CL1                 Y/N HAVE TEXT FORMAT RECORD                  
CPARMS   DS    XL1                 NUMBER OF PARAMETERS                         
SVELSEQ# DS    XL1                 SAVE AREA FOR ELEMENT SEQUENCE NUM           
KEYSAVE  DS    CL42                                                             
ULSAVE   DS    CL2                 SAVE UNIT LEDGER TO VALIDATE                 
BLOCK    DS    15CL32                                                           
ELEMENT  DS    XL256                                                            
MONLIST  DS    0CL4                                                             
         DSDDL                                                                  
LWSX     DS    0C                                                               
         EJECT ,                                                                
KDWD     DSECT                                                                  
KDWLN    DS    AL1                 LENGTH OF ENTRY                              
KDWVER#  DS    AL2                 VERSION      NUMBER                          
KDWMOD#  DS    AL1                 MODIFICATION NUMBER                          
KDWIND1  DS    XL1                 BUILD KEYWORD INDICATOR                      
KDWCN    EQU   X'80'                 REMOVE LAST CHARACTER OF CODE/NAME         
KDWLV4   EQU   X'40'                 PUT LEVEL VALUE, l=4                       
KDWHDNAR EQU   X'20'                 GET NARRATIVE FROM KEYWORD HEAD#           
KDWHPNAR EQU   X'10'                 GET NARRATIVE FROM KEYWORD HELP#           
KDWIND2  DS    XL1                                                              
KDWCODE  DS    CL6                 KEYWORD CODE                                 
KDWNARR  DS    0CL30               KEYWORD NARRATIVE                            
         EJECT ,                                                                
SECELD   DSECT                                                                  
SECEL    DS    XL1                 LOCAL SECURITY ELEMENT                       
SECELQ   EQU   X'25'                                                            
SECLN    DS    XL1                 ELEMENT LENGTH                               
SECSVAL  DS    0CL8                                                             
SECDSGN  DS    CL1                 YES/NO FOR DESIGN  ACCESS                    
SECRQST  DS    CL1                 YES/NO FOR REQUEST ACCESS                    
         DS    CL6                                                              
SECLNQ   EQU   *-SECELD                                                         
         EJECT ,                                                                
*ACSCRWRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE7D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACSCR15   02/25/15'                                      
         END                                                                    

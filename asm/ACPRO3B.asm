*          DATA SET ACPRO3B    AT LEVEL 004 AS OF 02/24/15                      
*PHASE T60B3BA                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B3B - SESSION ESTIMATE LIST'                                 
T60B3B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B3B**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         MVI   IOOPT,C'Y'                                                       
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BE    SLS2                                                             
         B     XIT                                                              
*                                                                               
*----------------------------------------------------------------------         
* VALREC LOGIC                                                                  
*----------------------------------------------------------------------         
*                                                                               
SLS2     LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ST    R2,RELO                                                          
         BAS   RE,VALHED                                                        
*                                                                               
SLS3     MVI   INTMODE,DISLIST     INITIALIZE INTERNAL MODE                     
*                                                                               
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
*                                                                               
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
*                                                                               
         CLC   LASTSES(2),=X'2C3C' IS LASTSES OK                                
         BE    SLS4                                                             
         MVI   INTMODE,FSTLIST     NO                                           
         XC    LASTSES,LASTSES                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* DISPLAY                                                                       
*----------------------------------------------------------------------         
*                                                                               
SLS4     BAS   RE,SETLIM                                                        
*                                                                               
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    SLS6                                                             
*                                                                               
         BAS   RE,PROCSEL          PROCESS ANY SELECTS                          
*                                                                               
*                                                                               
*----------------------------------------------------------------------         
* DISPLAY LOGIC                                                                 
*----------------------------------------------------------------------         
*                                                                               
SLS6     GOTO1 VCLEARF,DMCB,SLSSEL1H,SLSDATX                                    
         GOTO1 (RF),(R1),(1,SLSSEL1H),SLSDATX                                   
         MVI   LNLISTS,0                                                        
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LASTSES,LASTSES   CLEAR OUT LAST SESSION LISTED                  
*                                                                               
         BAS   RE,LIST                                                          
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
         LA    R2,SLSSEL1H                                                      
         MVI   MYMSGNO1,ILISTDIS                                                
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    SLSX                YES                                          
         LA    R2,SLSCLIH          PUT CURSOR AT FIRST KEY FIELD                
         MVI   MYMSGNO1,IENDLIST                                                
*                                                                               
SLSX     B     INFEXIT                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*----------------------------------------------------------------------         
*                                                                               
VALHED   NTR1                                                                   
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   OPTION,0            NO NAME FIELDS TO BE SHOWN                   
*                                                                               
         LA    R2,SLSCLIH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
*                                                                               
VALHED2  LA    R2,SLSPROH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
*                                                                               
VALHED4  LA    R2,SLSJOBH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         GOTO1 VALJOB                                                           
         MVC   QJOB,JOBNUM                                                      
*                                                                               
         LA    R2,SLSFLTH                                                       
         BAS   RE,TSTKEY                                                        
*                                                                               
         LA    R2,SLSMEDH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   ERROR,BADMEDIA                                                   
         CLI   5(R2),0                                                          
         BE    VALHED5                                                          
         CLI   8(R2),C'T'                                                       
         BE    VALHED5                                                          
         CLI   8(R2),C'R'                                                       
         BNE   ERREND                                                           
*                                                                               
VALHED5  LA    R2,SLSUSEH                                                       
         BAS   RE,TSTKEY                                                        
*                                                                               
VALHED6  OI    SLSCLIH+4,X'20'     SET ON PREV VALID BITS                       
         OI    SLSPROH+4,X'20'                                                  
         OI    SLSJOBH+4,X'20'                                                  
         OI    SLSFLTH+4,X'20'                                                  
         OI    SLSUSEH+4,X'20'                                                  
         OI    SLSMEDH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'80'         THIS TIME                                    
         BO    TSTKY                                                            
         TM    4(R2),X'20'                                                      
         BOR   RE                                                               
TSTKY    MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SET STARTSES AND ENDSES, KEYS TO LIMIT READING                         
*----------------------------------------------------------------------         
SETLIM   NTR1                                                                   
         XC    STARTSES,STARTSES   PRIME                                        
         MVI   ENDSES,X'FF'                                                     
         MVC   ENDSES+1(L'ENDSES-1),ENDSES                                      
*                                                                               
         USING SESRECD,R6                                                       
         LA    R6,STARTSES                                                      
         BAS   RE,SETKEY                                                        
         LA    R6,ENDSES                                                        
         BAS   RE,SETKEY                                                        
         B     XIT                                                              
*                                                                               
SETKEY   EQU   *                                                                
         MVI   SESKTYP,SESKTYPQ                                                 
         MVI   SESKSUB,SESKSUBQ                                                 
         MVC   SESKCUL,CUL                                                      
         MVC   SESKCUL,CUL                                                      
         CLC   QCLI,SPACES                                                      
         BNHR  RE                                                               
         MVC   SESKCLI,QCLI                                                     
         CLC   QPROD,SPACES                                                     
         BNHR  RE                                                               
         MVC   SESKPRO,QPROD                                                    
         CLC   QJOB,SPACES                                                      
         BNHR  RE                                                               
         MVC   SESKJOB,QJOB                                                     
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO LIST SESSION RECORDS                                           
*                                                                               
* AT ENTRY, LNLISTS CONTAINS N'LIST LINES ALREADY ON SCREEN                     
*           AND LASTSES CONTAINS LAST KEY READ OR BINARY ZERO                   
*----------------------------------------------------------------------         
*                                                                               
LIST     NTR1                                                                   
         LA    R2,SLSSEL1H                                                      
         LA    R6,KEY                                                           
         USING SESRECD,R6                                                       
*                                                                               
LIST2    ST    R2,ATHISLIN         INITIALIZE LIST LINE POINTER                 
*                                                                               
         XC    SESKEY,SESKEY                                                    
         MVC   SESKEY(L'STARTSES),STARTSES                                      
*                                                                               
LIST4    OC    LASTSES,LASTSES     TEST RESUMING READ                           
         BZ    LIST10              NO-STARTING FROM BEGINNING                   
*                                                                               
         MVC   SESKEY(L'LASTSES),LASTSES                                        
         MVI   SESKSEQ,X'FF'       BUMP TO NEXT SESION RECORD                   
         B     LIST10                                                           
*                                                                               
LIST10   OI    DMINBTS,X'08'       RETURN DELETES                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ENDSES,KEY                                                       
         BL    LISTX                                                            
*                                                                               
         GOTO1 CATCHIOS                                                         
         B     LIST20                                                           
*                                                                               
*                                                                               
*----------------------------------------------------------------------         
* DISPLAY NEW LIST LINE ON SCREEN                                               
*----------------------------------------------------------------------         
*                                                                               
LIST20   MVC   LASTSES,SESKEY                                                   
         BAS   RE,FILTERIT         FILTER ON FILTER VALUES                      
         BNE   LIST30                                                           
         BAS   RE,FILTPERS         FILTER ON PERSON                             
         BNE   LIST30                                                           
         BAS   RE,FILTMED          FILTER ON MEDIA                              
         BNE   LIST30                                                           
*                                                                               
         L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         BAS   RE,DISSES                                                        
*                                                                               
         MVC   ATHISLIN,ANEXTSEL                                                
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
*                                                                               
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
LIST30   B     LIST4               NEXT SESSION RECORD                          
*                                                                               
LISTX    EQU   *                                                                
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTXX                                                           
         MVC   LASTSES,STARTSES    START OVER NEXT TIME                         
LISTXX   B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR AN SESSION RECORD                   
*----------------------------------------------------------------------         
*                                                                               
DISSES   NTR1  ,                                                                
         USING LISTD,R5                                                         
         LA    R5,LISTAR                                                        
         USING SESRECD,R6                                                       
         L     R6,AIO                                                           
*                                                                               
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
*                                                                               
         L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         MVC   LISCLI,SESKCLI                                                   
         MVC   LISPRO,SESKPRO                                                   
         MVC   LISJOB,SESKJOB                                                   
         MVC   LISTYPE,SESKTYPE                                                 
         ZIC   R0,SESKVER                                                       
         EDIT  (R0),(3,LISVER),ALIGN=LEFT                                       
         MVC   LISMED,SESKMED                                                   
*                                                                               
         TM    ACCOSTAT(R6),X'80'  DELETED                                      
         BNO   DISSES10                                                         
         MVC   LISDESC(13),=C'** DELETED **'                                    
         B     DISSES20                                                         
*                                                                               
DISSES10 MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISSES20                                                         
*                                                                               
         USING NAMELD,R6                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         LTR   R1,R1                                                            
         BM    DISSES20                                                         
         SR    R0,R0                                                            
         LA    R0,L'LISDESC-1                                                   
         CR    R1,R0                                                            
         BL    *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISDESC(0),NAMEREC                                               
*                                                                               
         USING FFTELD,R6                                                        
DISSES20 MVC   LISFLT,SPACES                                                    
         MVI   ELCODE,FFTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISSES50                                                         
         CLI   FFTTYPE,FFTTTFLT    IS THIS SESSION FILTER VALUES                
         BNE   DISSES50            NO                                           
         ZIC   R1,FFTDLEN                                                       
         LTR   R1,R1                                                            
         BZ    DISSES50                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISFLT(0),FFTDATA                                                
*                                                                               
         USING PACELD,R6                                                        
DISSES50 MVI   ELCODE,PACELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   *+10                                                             
         MVC   LISPERS,PACPERS                                                  
*                                                                               
DISSES60 BAS   RE,MOVEFLD                                                       
*                                                                               
DISSESX  B     XIT                                                              
         SPACE 2                                                                
*----------------------------------------------------------------------         
* SUB-ROUTINE TO DISPLAY THE ESTIMATE TOTAL                                     
*----------------------------------------------------------------------         
*                                                                               
DISTOT   NTR1  ,                                                                
*        EDIT  NETSES,(14,LISTAR),2,ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES           
DISTOTX  B     XIT                                                              
         SPACE 2                                                                
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*----------------------------------------------------------------------         
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*----------------------------------------------------------------------         
*                                                                               
PROCSEL  NTR1                                                                   
         LA    R2,SLSSEL1H         R2=A(SELECT FIELD)                           
         XR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    PSELX                                                            
*                                                                               
         BAS   RE,SETLIN           SET FIELD HEADER ADCONS                      
*                                                                               
         USING LISTD,R5                                                         
PSEL2    L     R2,ASEL             RESTORE R2=A(SELECT FIELD)                   
         L     R5,ADATA                                                         
         LA    R5,8(R5)            GET PAST THE HEADER                          
*                                                                               
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    PSEL60                                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    PSEL60              YES                                          
*                                                                               
         LA    R0,NVALSELS         VALIDATE SELECT FIELD                        
         LA    R1,VALSELS                                                       
PSEL10   CLC   8(1,R2),0(R1)                                                    
         BE    PSEL20                                                           
         LA    R1,LVALSELS(R1)                                                  
         BCT   R0,PSEL10                                                        
         B     ERREND              INVALID SELECT FIELD                         
*                                                                               
PSEL20   MVI   PFKEY,0                                                          
         BAS   RE,CHKDEL                                                        
         CLC   BYTE,3(R1)          IS THIS THE TYPE OF RECORD WE WANT           
         BNE   PSELERR             NOPE                                         
*                                                                               
         MVI   8(R2),C'*'                                                       
         OI    6(R2),X'80'                                                      
         XR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         LA    RE,0(RB,RE)                                                      
         BR    RE                                                               
*                                                                               
PSEL60   L     R2,ANEXTSEL                                                      
         BAS   RE,SETLIN                                                        
         BCT   R3,PSEL2                                                         
*                                                                               
PSELX    B     XIT                                                              
*                                                                               
PSELS    GOTO1 VCALL,WORK,RECNSES,ACTNDIS,(3,LISCLI),(3,LISPRO),(6,LISJX        
               OB),(4,LISTYPE),0                                                
*                                                                               
PSELC    GOTO1 VCALL,WORK,RECNSES,ACTNCHA,(3,LISCLI),(3,LISPRO),(6,LISJX        
               OB),(4,LISTYPE),0                                                
*                                                                               
PSELR    GOTO1 VCALL,WORK,RECNSES,ACTNRES,(3,LISCLI),(3,LISPRO),(6,LISJX        
               OB),(4,LISTYPE),(1,LISMED),0                                     
*                                                                               
PSELD    GOTO1 VCALL,WORK,RECNSES,ACTNDEL,(3,LISCLI),(3,LISPRO),(6,LISJX        
               OB),(4,LISTYPE),(1,LISMED),0                                     
*                                                                               
*        COME HERE IF YOU ARE TRYING TO DEL A DELETED OR                        
*        RESTORE A NON-DELETED RECORD                                           
*                                                                               
PSELERR  MVI   ERROR,ALRDEL                                                     
         CLI   BYTE,DELETED        CHKDEL SAID ALREADY DELETED                  
         BE    PSELER10                                                         
         MVI   ERROR,RECNTDEL                                                   
*                                                                               
PSELER10 L     R2,ASEL                                                          
         ST    R2,ACURFORC                                                      
         B     ERREND                                                           
*                                                                               
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO SET ADCONS FOR A LIST FIELD LINE                               
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*----------------------------------------------------------------------         
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         ST    R2,ASEL                                                          
         BAS   RE,BUMP                                                          
         ST    R2,ADATA                                                         
         BAS   RE,BUMP                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* FILTER A SESSION EST FROM THE DISPLAY BASED ON FILTER VALUES                  
*----------------------------------------------------------------------         
FILTERIT NTR1                                                                   
         LA    R2,SLSFLTH          ARE THEY FILTERING                           
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    FILTYES             NO, ACCEPT ALL RECORDS                       
*                                                                               
         MVI   ELCODE,FFTELQ                                                    
         BAS   RE,GETELIO                                                       
FILT10   BNE   FILTNO                                                           
*                                                                               
         USING FFTELD,R6                                                        
         CLI   FFTTYPE,FFTTTFLT    SESSION FILTER VALUE                         
         BE    FILT30              YES                                          
         BAS   RE,NEXTEL                                                        
         B     FILT10                                                           
*                                                                               
*                                                                               
FILT30   SR    R3,R3                                                            
         ICM   R3,1,FFTDLEN        GET NUMBER OF FILTER VALUES                  
         BZ    FILTNO              NO FILTERS ON RECORD, REJECT                 
*                                                                               
         CR    R1,R3                                                            
         BH    FILTNO              NOT ENOUGH FILTERS                           
*                                                                               
         LA    R3,FFTDATA          FILTERS IN RECORD                            
         LA    R2,8(R2)            FILTERS ON SCREEN                            
FILT50   CLI   0(R2),C' '          WILDCARD                                     
         BE    FILT60                                                           
*                                                                               
         CLC   0(1,R3),0(R2)                                                    
         BNE   FILTNO                                                           
FILT60   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,FILT50                                                        
FILTYES  B     YESXIT                                                           
FILTNO   B     NOXIT                                                            
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RETURN WHETHER A SESSION IS DELETED IN BYTE                            
*        ASSUMES ESTIMATE KEY VALUES ARE IN ADATA                               
*----------------------------------------------------------------------         
CHKDEL   NTR1                                                                   
         USING LISTD,R5                                                         
         L     R5,ADATA                                                         
         LA    R5,8(R5)            BUMP PAST FIELD HEADER                       
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         XC    SESKEY,SESKEY                                                    
         MVI   SESKTYP,SESKTYPQ                                                 
         MVI   SESKSUB,SESKSUBQ                                                 
         MVC   SESKCPY(3),CUL                                                   
         MVC   SESKCLI,SPACES                                                   
         MVC   SESKCLI(L'LISCLI),LISCLI                                         
         MVC   SESKPRO,SPACES                                                   
         MVC   SESKPRO(L'LISPRO),LISPRO                                         
         MVC   SESKJOB,LISJOB                                                   
*                                                                               
         MVC   SESKTYPE,LISTYPE                                                 
*                                                                               
         LA    R6,2                                                             
         CLI   LISVER+2,C' '                                                    
         BH    CHKDEL20                                                         
         LA    R6,1                                                             
         CLI   LISVER+1,C' '                                                    
         BH    CHKDEL20                                                         
         LA    R6,0                                                             
CHKDEL20 EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LISVER(0)                                                    
         CVB   R0,DUB                                                           
         MVI   ERROR,INVALID                                                    
         LTR   R0,R0                                                            
         BZ    ERREND              CANNOT BE VERSION ZERO                       
         STC   R0,SESKVER                                                       
         MVC   SESKMED,LISMED                                                   
         MVC   AIO,AIO1                                                         
         OI    DMINBTS,X'08'       RETURN DELETES                               
         GOTO1 READ                                                             
         L     R6,AIO                                                           
         MVI   BYTE,NOTDELED                                                    
         TM    ACCOSTAT(R6),X'80'  DELETED                                      
         BZ    *+8                                                              
         MVI   BYTE,DELETED                                                     
CHKDELX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* FILTER A SESSION EST FROM THE DISPLAY BASED ON PERSON                         
*----------------------------------------------------------------------         
FILTPERS NTR1                                                                   
         LA    R2,SLSUSEH          DO THEY WANT A SPECIFIC PERSON               
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    FILTYES             NO, ACCEPT ALL RECORDS                       
*                                                                               
         MVI   ELCODE,PACELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   FILTNO                                                           
*                                                                               
         USING PACELD,R6                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PACPERS(0),8(R2)                                                 
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* FILTER A SESSION EST FROM THE DISPLAY BASED ON MEDIA                          
*----------------------------------------------------------------------         
FILTMED  NTR1                                                                   
         LA    R2,SLSMEDH          DO THEY WANT A SPECIFIC MEDIA                
         CLI   5(R2),0                                                          
         BZ    FILTYES             NO, ACCEPT ALL RECORDS                       
*                                                                               
         USING SESRECD,R6                                                       
         LA    R6,KEY                                                           
         CLC   SESKMED,8(R2)                                                    
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*              SUPPORTING SUBROUTINES                                           
*----------------------------------------------------------------------         
*                                                                               
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
INFEXIT  ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
*                                                                               
YESXIT   CR    RB,RB                                                            
         B     XIT                                                              
NOXIT    LTR   RB,RB                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
*        TABLE OF VALID SELECT FIELDS                                           
*              FIELD VALUE, A(CODE TO BR TO), VALUE WANTED FROM CHKDEL          
*                                                                               
VALSELS  DS    0CL4                                                             
         DC    C'S',AL2(PSELS-T60B3B),AL1(NOTDELED)                             
         DC    C'C',AL2(PSELC-T60B3B),AL1(NOTDELED)                             
         DC    C'R',AL2(PSELR-T60B3B),AL1(DELETED)                              
         DC    C'D',AL2(PSELD-T60B3B),AL1(NOTDELED)                             
NVALSELS EQU   (*-VALSELS)/LVALSELS                                             
LVALSELS EQU   L'VALSELS                                                        
*                                                                               
*        VALUES RETURNED BY CHKDEL IN BYTE                                      
NOTDELED EQU   C'N'                                                             
DELETED  EQU   C'D'                                                             
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACJOBBERD                                                                      
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENFILE                                                                      
*ACGENBOTH                                                                      
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
RELO     DS    A                                                                
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
CHASW    DS    C                   AUTHORIZE SWITCH                             
*                                                                               
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
ADATA    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
QCLI     DS    CL6                                                              
QPROD    DS    CL6                                                              
QJOB     DS    CL6                                                              
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
LISTD    DSECT                     DSECT TO COVER LISTAR                        
LISCLI   DS    CL3                                                              
         DS    CL1                                                              
LISPRO   DS    CL3                                                              
         DS    CL1                                                              
LISJOB   DS    CL6                                                              
         DS    CL2                                                              
LISTYPE  DS    CL1                                                              
LISVER   DS    CL3                                                              
         DS    CL2                                                              
LISMED   DS    CL1                                                              
         DS    CL6                                                              
LISPERS  DS    CL8                                                              
         DS    CL3                                                              
LISFLT   DS    CL4                                                              
         DS    CL3                                                              
LISDESC  DS    CL27                                                             
LISLEN   EQU   *-LISTD                                                          
*                                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROCBD                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2504                                                     
LSAVES   DS    0D                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LASTSES  DS   CL(SESKSEQ-SESKEY)  LAST SESSION ON SCREEN                        
STARTSES DS   CL(SESKMED-SESKEY)  SESSION KEY TO START                          
ENDSES   DS   CL(SESKMED-SESKEY)  HIGHEST SESSION TO DISPLAY                    
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NLINES   EQU   16                  N'LIST SCREEN LINES                          
LISTFLDS EQU   (ANEXTSEL-ASEL)/4   N'FIELDS ON LIST SCREEN LINE                 
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER EREP SUB-ROUTINE WORKING STORAGE                               
*                                                                               
ERWORKD  DSECT                                                                  
ERSYSRD  DS    A                                                                
ERSAVE   DS    XL(LOCALLN)         SAVED LOCAL WORKING STORAGE                  
ERWORKL  EQU   *-ERWORKD                                                        
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACPRO3B   02/24/15'                                      
         END                                                                    

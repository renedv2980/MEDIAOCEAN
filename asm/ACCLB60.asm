*          DATA SET ACCLB60    AT LEVEL 160 AS OF 08/16/00                      
*PHASE T62160A                                                                  
*&&      SET   NOP=N                                                            
CLB60    TITLE '- PC COMMS - BILL EDIT'                                         
CLB60    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB60**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
         B     SNDEL               SEND ELEMENT                                 
         B     SNDFLD              SEND ELEMENT FIELD                           
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB                                                        
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         ICM   RF,15,ORCVHDRF                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
RCVDIE   DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECIEVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         ICM   RF,15,ORCVHDRL                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         CLI   EDUPLD,0            TEST ANY UPLOADS                             
         BE    EXITY                                                            
         CLI   EDACT,EDACTNEW                                                   
         BE    EXITY                                                            
         GOTO1 PUTBED              PUT BACK LAST RECORD                         
*                                                                               
         PUSH  USING                                                            
         USING BEDRECD,KEYRCV      DELETE ANY RECORDS AFTER THIS                
         MVC   BEDKLVL,BCEFFS                                                   
         TM    EDUPLD,EDSNDPAR     TEST WAS SENT PARAS                          
         BO    RLST02                                                           
         MVC   BEDKLVL,=AL2(BEDKLADV)                                           
         TM    EDUPLD,EDSNDADV     TEST WAS SENT ADVANCES                       
         BO    RLST02                                                           
         XC    BEDKLVL,BEDKLVL                                                  
RLST02   MVC   BEDKWHER,BCEFFS                                                  
         GOTO1 DELRECS,BOPARM,LASTKEY,BEDKEY                                    
         POP   USING                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SNDEL    DS    0H                                                               
         ICM   RF,15,OSNDEL                                                     
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
SNDFLD   DS    0H                                                               
         ICM   RF,15,OSNDFLD                                                    
         BNZR  RF                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* RECEIVE REQUEST DETAILS                                             *         
***********************************************************************         
         SPACE 1                                                                
RCVACT   MVC   EDACT,DATA                                                       
         B     EXITY                                                            
         SPACE 1                                                                
RCVNUM   MVC   BEWBLNO,DATA                                                     
         B     EXITY                                                            
         SPACE 1                                                                
RCVJOB   MVC   BEWJOB,DATA                                                      
         B     EXITY                                                            
*                                                                               
RCVJOBN  MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         SPACE 1                                                                
RCVFMT   MVC   BEWFORM,DATA                                                     
         B     EXITY                                                            
         SPACE 1                                                                
RCVLANG  MVC   BEWLANG,DATA                                                     
         B     EXITY                                                            
         SPACE 1                                                                
RCVUPLD  MVC   EDUPLD,DATA                                                      
         B     EXITY                                                            
         SPACE 1                                                                
RCVDNLD  MVC   EDDNLD,DATA                                                      
         B     EXITY                                                            
         SPACE 1                                                                
RCVBPQ   MVC   BEWBLLPQ,DATA                                                    
         B     EXITY                                                            
         SPACE 1                                                                
RCVPPQ   MVC   BEWPSTPQ,DATA                                                    
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RECEIVED END OF REQUEST DETAILS                                     *         
***********************************************************************         
         SPACE 1                                                                
RCVREQL  DS    0H                                                               
         CLI   EDUPLD,0            ANYTHING BEING SENT BY PC                    
         BE    RREQL02                                                          
         TM    EDUPLD,EDSNDHDR     YES - THEN HEADER MUST BE SENT               
         BO    *+6                 (OR PROGRAM WON'T WORK)                      
         DC    H'0'                                                             
*                                                                               
RREQL02  DS    0H                                                               
         CLI   EDACT,EDACTNEW                                                   
         BNE   RREQL04                                                          
         GOTO1 ASETUP,BOPARM,BEWJOB,0,0                                         
         B     EXITY                                                            
*                                                                               
RREQL04  DS    0H                                                               
         LA    R2,IOKEY                                                         
         USING BEDRECD,R2                                                       
         CLI   EDACT,EDACTLAT      TEST GETTING BILL FOR JOB                    
         BNE   RREQL10                                                          
         XC    BEDKEY,BEDKEY                                                    
         MVI   BEDKTYP,BEDKTYPQ                                                 
         MVC   BEDKCPY,CUABIN                                                   
         MVI   BEDKSUB,BEDKSUBQ                                                 
         MVC   BEDKJOB,BEWJOB                                                   
*                                                                               
RREQL08  GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         CLC   BEDKEY(BEDKJSEQ-BEDKEY),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         OC    BEDKLVL(BEDKSTA-BEDKLVL),BEDKLVL TEST IS BILL HEADER             
         BNZ   *+14                                                             
         OC    BEDKBILD,BEDKBILD   TEST DRAFT BILL                              
         BZ    RREQL20                                                          
         ICM   RF,3,BEDKSEQ        NO - READ FOR NEXT DRAFT BILL                
         LA    RF,1(RF)                                                         
         STCM  RF,3,BEDKSEQ                                                     
         OC    BEDKSEQ,BEDKSEQ     TEST LAST BILL READ                          
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         XC    BEDKLVL(BEDKSTA-BEDKLVL),BEDKLVL                                 
         B     RREQL08                                                          
*                                                                               
RREQL10  DS    0H                                                               
         XC    BEDPAS,BEDPAS       USE PASSIVE KEY TO FIND BILL                 
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,BEWBLNO                                                 
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         CLC   BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
*                                                                               
RREQL20  DS   0H                   GET BILL HEADER RECORD                       
         MVC   BEWHDRDA,BEDKDA                                                  
         CLI   BEDPIND,BEDPIDFT                                                 
         BNE   *+8                                                              
         OI    EDINDS,EDIRDFT      SET RECEIVED DRAFT BILL                      
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         MVC   BEWHDRKY,BEDKEY                                                  
         MVC   KEYRCV,BEDKEY       INITIALIZE KEYRCV                            
         MVC   STARCV,BEDRSTA                                                   
*                                                                               
         LA    R3,BEDRFST          SAVE BLHELD AND ADD RACELD                   
         XR    RF,RF                                                            
RREQL22  CLI   0(R3),0                                                          
         BE    RREQL30                                                          
         USING BLHELD,R3                                                        
         CLI   BLHEL,BLHELQ                                                     
         BNE   RREQL24                                                          
         MVC   BEWBLH,BLHELD                                                    
         B     RREQL28                                                          
         USING RACELD,R3                                                        
RREQL24  CLI   RACEL,RACELQ                                                     
         BNE   RREQL28                                                          
         CLI   RACTYPE,RACTADD                                                  
         BNE   RREQL28                                                          
         MVC   ADDRAC,RACELD                                                    
         DROP  R3                                                               
RREQL28  IC    RF,1(R3)                                                         
         BXH   R3,RF,RREQL22                                                    
         DROP  R2                                                               
*                                                                               
RREQL30  DS    0H                                                               
         PUSH  USING                                                            
         USING BLHELD,BEWBLH                                                    
         MVC   BEWBLNO,BLHBLNO                                                  
         MVC   BEWJOB,BLHJOB                                                    
         MVC   BEWFORM,BLHFORM                                                  
         MVC   BEWLANG,BLHLANG                                                  
         GOTO1 ASETUP,BOPARM,BEWJOB,BLHCUR,BLHRVAL                              
         USING BFMRECD,BEWFMTKY                                                 
         XC    BFMKEY,BFMKEY                                                    
         MVI   BFMKTYP,BFMKTYPQ                                                 
         MVC   BFMKCPY,CUABIN                                                   
         MVI   BFMKSUB,BFMKSUBQ                                                 
         MVC   BFMKFMT,BEWFORM                                                  
         MVC   BFMKLANG,BEWLANG                                                 
         POP   USING                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RECIEVE FIRST FOR ADVANCE                                           *         
***********************************************************************         
         SPACE 1                                                                
RCVADVF  DS    0H                                                               
         TM    EDINDS,EDIBHDR      TEST BILL HEADER DONE                        
         BO    EXITY                                                            
         CLI   EDACT,EDACTNEW      TEST ACTION NEW                              
         BNE   RADVF02                                                          
         BAS   RE,NEWHDR           ADD NEW BILL HEADER                          
         B     EXIT                                                             
*                                                                               
RADVF02  GOTO1 PUTBED              WRITE HEADER RECORD                          
         OI    EDINDS,EDIBHDR                                                   
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE ADVANCE NARRATIVE                                           *         
***********************************************************************         
         SPACE 1                                                                
RCVADVN  DS    0H                                                               
         LH    RE,DATALEN          SET NARRATIVE LENGTH IN ADVELD               
         STC   RE,ELEM+(ADVNARRL-ADVELD)                                        
         MVI   NARRADV,C' '        SAVE NARRATIVE IN W/S                        
         MVC   NARRADV+1(L'NARRADV-1),NARRADV                                   
         LTR   RE,RE                                                            
         BZ    EXITY                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NARRADV(0),DATA                                                  
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE LAST FOR ADVANCE                                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
RCVADVL  DS    0H                                                               
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         USING ADVELD,ELEM         BUILD RECORD FROM ELEMENT                    
         GOTO1 AADVANCE,BOPARM,=C'BLDREC',BEDRECD,ADVELD,NARRADV                
         BNE   EXITN                                                            
         MVC   BEDKEY,BEWHDRKY                                                  
         MVC   BEDKLVL,=AL2(BEDKLADV)                                           
*                                                                               
         MVC   BEDKSEQ,ADVSEQ                                                   
         OI    PUTINDS,PUTIRECI                                                 
         MVC   KEYRCV,BEDKEY                                                    
         GOTO1 PUTBED                                                           
         IC    RE,ADVSEQ           INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STC   RE,ADVSEQ                                                        
         CLI   ADVSEQ,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* RECIEVE RECORD KEY DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
RCVKEYF  DS    0H                                                               
         CLI   EDUPLD,0            TEST WRITING RECORD                          
         BE    EXITY                                                            
         CLI   EDACT,EDACTNEW      TEST ADDING NEW RECORD                       
         BNE   RCVKEYF2                                                         
         DC    H'0'                NO REASON TO RECEIVE HEADER                  
*                                                                               
RCVKEYF2 GOTO1 PUTBED              WRITE PREVIOUS RECORD                        
         B     EXITY                                                            
*                                                                               
K        USING BEDRECD,KEYRCV                                                   
S        USING BEDKSTA,STARCV                                                   
RCVKEY1  DS    0H                  * LEVEL (COVER/MAIN/SUMMARY) *               
         MVC   K.BEDKLVL,DATA                                                   
         XC    K.BEDKWHER(BEDKSTA-BEDKWHER),K.BEDKWHER                          
         XC    S.BEDKSTA,S.BEDKSTA                                              
         MVI   SECTSEQ,0           RESET SECTION SEQUENCE NUMBER                
         B     EXITY                                                            
*                                                                               
RCVKEY2  DS    0H                  * WHERE *                                    
         MVC   K.BEDKWHER,DATA                                                  
         XC    K.BEDKSSEQ(BEDKSTA-BEDKSSEQ),K.BEDKSSEQ                          
         XC    S.BEDKSTA,S.BEDKSTA                                              
         B     EXITY                                                            
*                                                                               
RCVKEY3  DS    0H                  * BODY OF BILL SECTION NUMBER *              
         MVC   K.BEDKWHER,=AL2(BEDKWBDQ)                                        
         IC    RE,SECTSEQ          INCREMENT SECTION SEQUENCE NUMBER            
         LA    RE,1(RE)                                                         
         STC   RE,SECTSEQ                                                       
         STC   RE,K.BEDKSSEQ                                                    
         XC    K.BEDKPARA(BEDKSTA-BEDKPARA),K.BEDKPARA                          
         XC    S.BEDKSTA,S.BEDKSTA                                              
         MVC   S.BEDKSECT,DATA                                                  
         XC    PARASEQ,PARASEQ     CLEAR PARAGRAPH SEQUENCE NUMBER              
         B     EXITY                                                            
*                                                                               
RCVKEY4  DS    0H                  * SECTION PARAGRAPH LEVEL *                  
         ICM   RE,3,PARASEQ        INCREMENT PARAGRAPH SEQUENCE NUMBER          
         LA    RE,1(RE)                                                         
         STCM  RE,3,PARASEQ                                                     
         STCM  RE,3,K.BEDKPARA                                                  
         MVC   S.BEDKSLVL,DATA                                                  
         MVI   S.BEDKSSUB,0                                                     
         B     EXITY                                                            
*                                                                               
RCVKEY5  DS    0H                  * FIELD VALUE FOR SUB-SECT LEVEL *           
         MVC   S.BEDKSSUB,DATA                                                  
         B     EXITY                                                            
         DROP  K,S                                                              
         EJECT                                                                  
***********************************************************************         
* RECIEVE BFMELD TRAILING SPACES                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BFMELD,ELEM                                                      
RCVSPACE DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,BFMLN            RF = L(ELEMENT)                              
         LA    R1,BFMELD(RF)       R1 = A(END OF ELEMENT)                       
         XR    RE,RE                                                            
         ICM   RE,1,DATA                                                        
         BZ    EXITY                                                            
         AR    RF,RE                                                            
         STC   RF,BFMLN            SET NEW L(ELEMENT)                           
*                                                                               
         MVI   0(R1),C' '          PUT TRAILING SPACES IN                       
         LA    R1,1(R1)                                                         
         BCT   RE,*-8                                                           
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* RECIEVE RACELD ELEMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RACELD,ELEM                                                      
RCVRAC   DS    0H                  ONLY RCVTIME IS SENT                         
         MVI   RACTYPE,RACTCHA                                                  
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CUPASS                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,BCTODAYP                                                 
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE BLHELD ELEMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVBLHF  DS    0H                  SET ELEM TO CURRENT DETAILS OF BLHEL         
         CLI   EDACT,EDACTNEW                                                   
         BE    EXITY                                                            
*                                                                               
         XR    RF,RF               SET ELEM TO CURRENT DETAILS OF BLHEL         
         IC    RF,BEWBLH+(BLHLN-BLHELD)                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ELEM(0),BEWBLH                                                   
         CLI   ELEM+(BLHLN-BLHELD),BLHLN2Q                                      
         BNL   *+8                                                              
         MVI   ELEM+(BLHLN-BLHELD),BLHLN2Q                                      
         B     EXITY                                                            
*                                                                               
RCVBLHL  DS    0H                  PUT DETAILS INTO SAVED ELEMENT               
         XC    BEWBLH,BEWBLH                                                    
         XR    RF,RF                                                            
         IC    RF,ELEM+(BLHLN-BLHELD)                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BEWBLH(0),ELEM                                                   
*                                                                               
         B     ADDELPOS            ADD ELEM TO THE RECORD                       
         EJECT                                                                  
***********************************************************************         
* SEND BILL RECORD DETAILS                                            *         
***********************************************************************         
         SPACE 1                                                                
SNDEDIT  DS    0H                                                               
         CLI   EDACT,EDACTUPD                                                   
         BE    SNDUPD                                                           
         CLI   EDACT,EDACTDRA                                                   
         BE    SNDDRA                                                           
         CLI   EDACT,EDACTPUT      EXIT IF BILL WRITTEN TO FILE                 
         BE    SNDPUT                                                           
         CLI   EDACT,EDACTGET      TEST GET EXISTING BILL                       
         BE    SNDGET                                                           
         CLI   EDACT,EDACTLAT      TEST GET LATEST BILL FOR JOB                 
         BE    SNDGET                                                           
         CLI   EDACT,EDACTNEW                                                   
         BE    SNDNEW                                                           
         CLI   EDACT,EDACTPQU                                                   
         BE    SNDPQU                                                           
         B     EXITN                                                            
         SPACE 1                                                                
SNDNEW   DS    0H                  CREATE NEW BILL                              
         GOTO1 NEWHDR              ADD BILL HEADER IF NECC.                     
         BNE   SNDFMTN                                                          
*                                                                               
         MVI   OVRLAY,OC#BEBUP     BUILD PAGE PANELS                            
         GOTO1 CALLOVR,BOPARM,=C'ADD'                                           
         BNE   SNDFMTN                                                          
*                                                                               
         TM    BEWBLH+(BLHINDS1-BLHELD),BLHIAFRM                                
         BZ    SNEW02                                                           
         MVI   OVRLAY,OC#BEAUT     AUTO-FORM BODY OF BILL                       
         GOTO1 CALLOVR                                                          
         BNE   SNDFMTN                                                          
*                                                                               
SNEW02   DS    0H                                                               
         BAS   RE,REFTOTS          ADD BILL TOTALS                              
         BNE   SNDFMTN                                                          
         B     SNDBILL                                                          
*                                                                               
SNDFMTN  OI    CSINDSG1,CSINDUNW   UNDO ANY BILL RECORDS ADDED                  
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* SEND BILL DETAILS                                                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SNDGET   DS    0H                                                               
         TM    EDINDS,EDIRDFT      TEST RECEIVED DRAFT BILL                     
         BZ    *+12                                                             
         BAS   RE,REFTOTS          REFRESH TOTALS                               
         BNE   EXITN                                                            
*                                                                               
SNDBILL  DS    0H                                                               
         TM    EDDNLD,EDSNDHDR                                                  
         BZ    EXITY                                                            
         USING BEDRECD,IOKEY                                                    
         XC    BEDPAS,BEDPAS       USE PASSIVE KEY TO FIND BILL                 
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,BEWBLNO                                                 
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         CLC   BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
*                                  SEND BILL NUMBER AND JOB                     
         GOTO1 ASNDHDR,BOPARM,MH#BED                                            
         GOTO1 ASNDDATA,(R1),2,BEDPBLNO                                         
         GOTO1 (RF),(R1),3,BEDPJOB                                              
         GOTO1 (RF),(R1),6,BEDKDA                                               
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASNDREC,BOPARM,AIO1 SEND BILL HEADER DETAILS                     
         TM    EDDNLD,EDSNDADV+EDSNDPAR                                         
         BZ    EXITY                                                            
         L     RF,AIO1                                                          
         MVC   BEWHDRKY,0(RF)      SAVE KEY OF HEADER RECORD                    
*                                                                               
         L     RF,AIO1  - BOUND TO SCREW UP IO1 THOUGH (DOH)                    
         MVC   BEDKEY,0(RF)                                                     
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
SBILL02  GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BNE   SBILL10                                                          
         CLC   BEDKEY(BEDKLVL-BEDKEY),IOKEYSAV                                  
         BNE   SBILL10                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   BEDKLVL,=AL2(BEDKLADV)                                           
         BNE   SBILL04             TEST ADVANCE RECORD                          
         TM    EDDNLD,EDSNDADV                                                  
         BZ    SBILL02                                                          
         GOTO1 AADVANCE,BOPARM,=C'CNVEL',AIO1,BOELEM,NARRADV                    
         GOTO1 ASNDELEM,BOPARM,ADVELQ,BOELEM                                    
         B     SBILL02                                                          
*                                                                               
SBILL04  DS    0H                  TEST SAME KEY EXCEPT FOR SEQ. NO             
         TM    EDDNLD,EDSNDPAR                                                  
         BZ    EXITY                                                            
         CLC   BEDKEY(BEDKSEQ-BEDKEY),IOKEYSAV                                  
         BE    SBILL06                                                          
         GOTO1 SNDKEY              SEND KEY DETAILS                             
SBILL06  DS    0H                                                               
         GOTO1 ASNDREC,BOPARM,AIO1 SEND RECORD DETAILS                          
         B     SBILL02                                                          
*                                                                               
SBILL10  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BILL DRAFT/UPDATE                                                   *         
***********************************************************************         
         SPACE 1                                                                
SNDDRA   LA    RE,PBDRAFTQ                                                      
         B     *+8                                                              
SNDUPD   LA    RE,PBLIVEQ                                                       
         PUSH  USING                                                            
         USING PBPARMS,SVPBPARM                                                 
         XC    PBPARMS,PBPARMS                                                  
         STC   RE,PBMODE                                                        
         MVI   PBTYPES,PBBILQ                                                   
         MVC   PBDRAFT#,BEWBLNO                                                 
         MVC   CSBILNUM,BEWBLNO                                                 
*                                                                               
         OI    BCINDS1,BCPCBILL                                                 
         GOTO1 APOSTIT,BOPARM,PBLKD                                             
         BNE   EXIT                                                             
*                                                                               
         GOTO1 ASNDHDR,BOPARM,4    SEND BACK DRAFT/UPDATE DETAILS               
*                                                                               
         L     R3,AREP             SEND PQ ID                                   
         USING REPD,R3                                                          
         MVC   THISPQID,BCSPACES                                                
         MVC   THISPQID(L'REPSUBID),REPSUBID                                    
         MVI   THISPQID+L'REPSUBID,C','                                         
         LA    RF,THISPQID+L'REPSUBID+1                                         
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1          
         DROP  R3                                                               
         GOTO1 ASNDDATA,BOPARM,1,THISPQID                                       
*                                                                               
         CLI   EDACT,EDACTUPD                                                   
         BNE   SUPD02                                                           
         GOTO1 ASNDDATA,BOPARM,2,PBLIVE#                                        
*                                                                               
SUPD02   DS    0H                                                               
         MVC   THISPQID,BCSPACES   SEND POSTINGS PQ ID                          
         MVC   THISPQID(L'CSJRNRID),CSJRNRID                                    
         MVI   THISPQID+L'CSJRNRID,C','                                         
         LA    RF,THISPQID+L'CSJRNRID+1                                         
         EDIT  (2,CSJRNRNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1          
         GOTO1 ASNDDATA,BOPARM,3,THISPQID                                       
*                                                                               
         TM    BEWINDS,BEWIUREP    TEST UPDATED A REPLICATE BILL                
         BZ    SUPD04                                                           
         GOTO1 ASNDDATA,BOPARM,4,BEWREPDA SEND NEW D/A                          
*                                                                               
SUPD04   DS    0H                                                               
         TM    EDDNLD,EDSNDTOT     TEST UPDATED TOTALS REQUIRED                 
         BZ    SNDUPDX                                                          
         TM    BEWINDS,BEWIUTOT    TEST TOTALS UPDATED                          
         BZ    SNDUPDX                                                          
         BAS   RE,SNDTOT                                                        
*                                                                               
SNDUPDX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SEND ANYTHING REQUIRED AFTER A PUT                                  *         
***********************************************************************         
         SPACE 1                                                                
SNDPUT   DS    0H                                                               
         TM    EDDNLD,EDSNDTOT     TEST UPDATED TOTALS REQUIRED                 
         BZ    EXITY                                                            
         TM    EDINDS,EDIRDFT      TEST RECEIVED DRAFT BILL                     
         BZ    EXITY                                                            
         BAS   RE,REFTOTS          REFRESH TOTALS                               
         TM    BEWINDS,BEWIUTOT    TEST TOTALS UPDATED                          
         BZ    EXITY                                                            
         BAS   RE,SNDTOT                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND TOTALS PARAS ONLY                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SNDTOT   NTR1  ,                                                                
K        USING BEDRECD,IOKEY                                                    
         MVC   K.BEDKEY,BEWHDRKY                                                
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASNDREC,BOPARM,AIO1 SEND HEADER DETAILS                          
         MVC   K.BEDKEY,BEWHDRKY                                                
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
*                                                                               
STOT02   DS    0H                                                               
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BNE   STOT10                                                           
         CLC   K.BEDKEY(BEDKLVL-BEDKEY),IOKEYSAV                                
         BNE   STOT10                                                           
         CLC   K.BEDKWHER,=AL2(BEDKWBTQ)                                        
         BE    STOT04                                                           
         CLC   K.BEDKWHER,=AL2(BEDKWVAQ)                                        
         BE    STOT04                                                           
         CLC   K.BEDKWHER,=AL2(BEDKWPBQ)                                        
         BNE   STOT08                                                           
STOT04   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         GOTO1 SNDKEY                                                           
         GOTO1 ASNDREC,BOPARM,AIO1 SEND RECORD DETAILS                          
*                                                                               
STOT08   DS    0H                                                               
         B     STOT02                                                           
*                                                                               
STOT10   DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BILL REPRINT                                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SNDPQU   DS    0H                                                               
         MVC   CSBILNUM,BEWBLNO                                                 
         OI    BCINDS1,BCPCBILL                                                 
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0               CALL PRINT ROUTINE                           
         GOTO1 VCOLY,BODMCB,('OC#BEPRT',0),0,0                                  
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
*                                                                               
         L     R3,AREP             SEND PQ ID                                   
         USING REPD,R3                                                          
         GOTO1 ASNDHDR,BOPARM,4                                                 
         MVC   THISPQID,REPSUBID                                                
         MVI   THISPQID+L'REPSUBID,C','                                         
         LA    RF,THISPQID+L'REPSUBID+1                                         
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1          
         GOTO1 ASNDDATA,BOPARM,1,THISPQID                                       
         DROP  R3                                                               
*                                                                               
SNDPQUX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SEND RECORD KEY                                                     *         
***********************************************************************         
         SPACE 1                                                                
SNDKEY   NTR1  ,                                                                
         GOTO1 ASNDHDR,BOPARM,1                                                 
*                                                                               
OLD      USING BEDRECD,IOKEYSAV                                                 
NEW      USING BEDRECD,IOKEY                                                    
*                                                                               
         DS    0H                  TEST FOR CHANGE IN LEVEL                     
         CLC   NEW.BEDKLVL,OLD.BEDKLVL                                          
         BE    SKEY02                                                           
         GOTO1 ASNDDATA,BOPARM,1,NEW.BEDKLVL                                    
         XC    OLD.BEDKEY,OLD.BEDKEY                                            
*                                                                               
SKEY02   DS    0H                  TEST FOR CHANGE IN WHERE                     
         CLC   NEW.BEDKWHER,OLD.BEDKWHER                                        
         BE    SKEY04                                                           
         XC    OLD.BEDKEY,OLD.BEDKEY                                            
         CLI   NEW.BEDKSSEQ,0      TEST FOR BILL BODY SECTION                   
         BNE   SKEY04                                                           
         GOTO1 ASNDDATA,BOPARM,2,NEW.BEDKWHER                                   
         B     EXIT                                                             
*                                                                               
SKEY04   DS    0H                  TEST FOR CHANGE IN SECTION NUMBER            
         CLC   NEW.BEDKSSEQ,OLD.BEDKSSEQ                                        
         BE    SKEY06                                                           
         GOTO1 ASNDDATA,BOPARM,3,NEW.BEDKSECT                                   
         B     EXIT                                                             
*                                                                               
SKEY06   DS    0H                  SEND SECTION LEVEL                           
         GOTO1 ASNDDATA,BOPARM,4,NEW.BEDKSLVL                                   
         GOTO1 (RF),(R1),5,NEW.BEDKSSUB                                         
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  OLD,NEW                                                          
         EJECT                                                                  
***********************************************************************         
* SEND TEXT PLUS NUMBER OF TRAILING SPACES                            *         
***********************************************************************         
         SPACE 1                                                                
SNDTEXT  DS    0H                                                               
         LH    R0,DATALEN                                                       
         LTR   R0,R0               DON'T SEND IF ZERO LENGTH                    
         BZ    EXITN                                                            
*                                                                               
         LA    RF,DATA             FIND NUMBER OF TRAILING SPACES               
         AH    RF,DATALENX                                                      
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LTR   R0,R0                                                            
         BZ    STEXT02                                                          
         GOTO1 ASNDDATA,BOPARM,AMDEL,((R0),DATA)                                
*                                                                               
STEXT02  DS    0H                                                               
         LH    RE,DATALEN                                                       
         SR    RE,R0                                                            
         BZ    EXITN                                                            
         STC   RE,BOWORK1                                                       
         GOTO1 ASNDDATA,BOPARM,22,BOWORK1                                       
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND RACPERS - GET PERSON RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
SNDPERS  DS    0H                                                               
         GOTO1 AGETPID,DATA                                                     
         GOTO1 ASNDDATA,BOPARM,AMDEL,BCWORK                                     
         B     EXITN               CC=NOT EQUAL ALREADY SENT                    
         SPACE 1                                                                
***********************************************************************         
* SEND MONTH OF SERVICE                                               *         
***********************************************************************         
         SPACE 1                                                                
SNDBMOS  DS    0H                                                               
         OC    DATA(2),DATA        ??                                           
         BNZ   *+10                                                             
         MVC   DATA(2),BCTODAYP                                                 
         MVI   DATA+2,X'01'        DATA = PWOS DATE                             
         GOTO1 ASNDDATA,BOPARM,AMDEL,(3,DATA)                                   
         B     EXITN               CC = NOT EQUAL, AS ALREADY SENT              
         SPACE 1                                                                
***********************************************************************         
* TEST IF REPLICATE BILL                                              *         
***********************************************************************         
SNDEXPD  DS    0H                                                               
         TM    ELEM+(BLHINDS1-BLHELD),BLHIREP                                   
         BO    EXITN               RETURN CC NOT EQUAL IF REP BILL              
         B     EXITY               RETURN CC EQUAL - SEND EXPIRY DATE           
         SPACE 1                                                                
***********************************************************************         
* SEND BILLING CURRENCY INO                                          *          
***********************************************************************         
         SPACE 1                                                                
SNDCUR   DS    0H                                                               
         LA    R3,CSCURCPY                                                      
         USING CURTABD,R3                                                       
         CLC   CURTCUR,DATA        TEST SENDING COMPANY CURRENCY                
         BE    SCUR02                                                           
         LA    R3,CSCURBIL         TEST SENDING BILLING CURRENCY                
         BE    SCUR02                                                           
         LA    R3,BOWORK1                                                       
         GOTO1 VBLDCUR,BOPARM,DATA,CURTABD,ACOM                                 
*                                                                               
SCUR02   DS    0H                                                               
         GOTO1 ASNDDATA,BOPARM,09,CURTCUR  CURRENCY CODE                        
         GOTO1 (RF),(R1),26,CURTDECP       NUMBER OF DEC. PLACES                
         CLI   CUCTRY,CTRYGER                                                   
         BNE   SCUR04                                                           
         GOTO1 (RF),(R1),27,CURTCUR        CURRENCY PREFIX                      
         GOTO1 (RF),(R1),28,2              TRAILING BLANKS AFTER PREFIX         
         B     SNDCURX                                                          
SCUR04   GOTO1 (RF),(R1),27,CURTSYMB       CURRENCY PREFIX                      
*                                                                               
SNDCURX  B     EXITN               CC = NOT EQUAL BECAUSE ALREADY SENT          
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* SEND ADVANCE NARRATIVE                                              *         
***********************************************************************         
         SPACE 1                                                                
SNDADVN  DS    0H                                                               
         XR    RF,RF                                                            
         ICM   RF,1,ELEM+(ADVNARRL-ADVELD)                                      
         BZ    EXITN                                                            
         GOTO1 ASNDDATA,BOPARM,10,((RF),NARRADV)                                
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REFRESH BILL TOTALS OF DRAFT BILL                        *         
*                                                                     *         
* NTRY: BEWBLNO = BILL NUMBER                                         *         
***********************************************************************         
         SPACE 1                                                                
REFTOTS  NTR1  ,                                                                
*                                                                               
         PUSH  USING                                                            
         USING PBPARMS,SVPBPARM                                                 
         OI    BCINDS1,BCPCBILL    ADD/REFRESH BILL TOTALS                      
         XC    PBPARMS,PBPARMS                                                  
         MVI   PBMODE,PBTOTQ                                                    
         MVI   PBTYPES,PBBILQ                                                   
         MVC   PBDRAFT#,BEWBLNO                                                 
         GOTO1 APOSTIT,BOPARM,PBLKD                                             
         BNE   EXITN                                                            
         POP   USING                                                            
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD BILL HEADER RECORD FOR NEW BILL                      *         
***********************************************************************         
         SPACE 1                                                                
NEWHDR   NTR1  ,                                                                
         TM    EDINDS,EDIBHDR      DON'T ADD IT TWICE                           
         BO    EXITY                                                            
*                                                                               
         PUSH  USING                                                            
         USING BLHELD,BEWBLH                                                    
         MVC   BEWJOB,BLHJOB                                                    
         MVC   BEWFORM,BLHFORM                                                  
         MVC   BEWLANG,BLHLANG                                                  
         POP   USING                                                            
*                                                                               
         MVI   OVRLAY,OC#BEHDR     BUILD BILL HEADER DETAILS                    
         GOTO1 CALLOVR                                                          
         BNE   EXITN                                                            
*                                                                               
         OI    EDINDS,EDIBHDR                                                   
         MVC   KEYRCV,BEWHDRKY                                                  
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT TO RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
ADDELEND LA    R3,ADDEND           ADD ELEMENT AT END OF RECORD                 
         B     ADDEL00                                                          
ADDELPOS XR    R3,R3               ADD ELEMENT AT NORMAL POSITION               
*                                                                               
ADDEL00  L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         TM    PUTINDS,PUTIRECI                                                 
         BO    ADDEL02                                                          
         XC    BEDRECD(BEDRFST+1-BEDRECD),BEDRECD                               
         MVC   BEDKEY,KEYRCV                                                    
         MVC   BEDRSTA,STARCV                                                   
         MVC   BEDRLEN,=AL2(BEDRFST+1-BEDRECD)                                  
         OI    PUTINDS,PUTIRECI                                                 
*                                                                               
ADDEL02  DS    0H                                                               
         XR    RF,RF               TEST RECORD WILL BE TOO BIG                  
         ICM   RF,3,BEDRLEN                                                     
         XR    RE,RE                                                            
         IC    RE,ELEM+(BFMLN-BFMELD)                                           
         AR    RF,RE                                                            
         CHI   RF,2000                                                          
         BNL   ADDEL04                                                          
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,ELEM,(R3)                    
         CLI   12(R1),0                                                         
         BE    ADDELX                                                           
         DC    H'0'                                                             
*                                                                               
ADDEL04  DS    0H                                                               
         OI    BEDRSTAT,BEDSCONT   SET RECORD CONTINUED                         
         BAS   RE,PUTBED           WRITE CURRENT RECORD                         
KEY      USING BEDRECD,KEYRCV                                                   
         IC    RE,KEY.BEDKSEQ      INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY.BEDKSEQ                                                   
         CLI   KEY.BEDKSEQ,0                                                    
         BNE   ADDEL00             ADD ELEMENT TO NEW RECORD                    
         DC    H'0'                                                             
         DROP  KEY                                                              
*                                                                               
ADDELX   DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT BED RECORD TO FILE                                   *         
***********************************************************************         
         SPACE 1                                                                
PUTBED   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         TM    PUTINDS,PUTIRECI    TEST ANY ELEMENTS ADDED TO RECORD            
         BO    PBED02                                                           
         TM    PUTINDS,PUTIFST     TEST FIRST RECORD                            
         BO    EXIT                                                             
         DC    H'0'                YES - MUST HAVE ELEMENTS                     
*                                                                               
PBED02   CLC   BEDKEY,KEYRCV       TEST FOR SCREW UP                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PUTINDS,PUTIFST     TEST PUT FIRST RECORD                        
         BO    PBED10                                                           
         OC    BEDKLVL,BEDKLVL     TEST HEADER RECORD                           
         BNZ   PBED20                                                           
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,ADDRAC,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PBED20                                                           
*                                  DELETE RECORDS IN BETWEEN                    
PBED10   GOTO1 DELRECS,BOPARM,LASTKEY,BEDRECD                                   
*                                                                               
PBED20   DS    0H                                                               
         GOTO1 WRTREC                                                           
         OI    PUTINDS,PUTIFST                                                  
         NI    PUTINDS,FF-PUTIRECI                                              
         MVC   LASTKEY,BEDKEY                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE RECORD                                             *         
*                                                                     *         
* NTRY: IO1 = RECORD TO BE WRITTEN                                    *         
* EXIT: RECORD WRITTEN/ADDED TO FILE                                  *         
***********************************************************************         
         SPACE 1                                                                
WRTREC   NTR1  ,                                                                
DIR      USING BEDRECD,IOKEY                                                    
         L     R2,AIO1                                                          
NEW      USING BEDRECD,R2                                                       
         L     R3,AIO2                                                          
OLD      USING BEDRECD,R3                                                       
*                                                                               
         MVC   DIR.BEDKEY,NEW.BEDKEY                                            
         GOTO1 AIO,IORDD+IOACCDIR+IO2                                           
         BE    WREC02                                                           
         TM    IOERR,IOEDEL                                                     
         BO    WREC02                                                           
         TM    IOERR,IOERNF        TEST RECORD NOT ON FILE                      
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    NEW.BEDRSTA,BEDSDELT TEST NEW RECORD DELETED                     
         BO    WRTRECX             YES - DON'T ADD IT THEN                      
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    WRTRECX                                                          
         DC    H'0'                                                             
*                                                                               
WREC02   DS    0H                  TEST CHANGE IN STATUS AREA                   
         CLC   DIR.BEDKSTA,NEW.BEDRSTA                                          
         BE    WREC04                                                           
         GOTO1 AIO,IORDUPD+IOACCDIR+IO2   YES - UPDATE DIRECTORY REC            
         MVC   DIR.BEDKSTA,NEW.BEDRSTA                                          
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WREC04   DS    0H                                                               
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                TEST FOR ANY CHANGES TO RECORD               
         CLC   NEW.BEDRLEN,OLD.BEDRLEN                                          
         BNE   WREC06                                                           
         LA    RE,NEW.BEDRECD                                                   
         XR    RF,RF                                                            
         ICM   RF,3,NEW.BEDRLEN                                                 
         LA    R0,OLD.BEDRECD                                                   
         LR    R1,RF                                                            
         CLCL  R0,RE                                                            
         BE    WRTRECX                                                          
*                                                                               
WREC06   DS    0H                                                               
         LA    RE,NEW.BEDRECD      COPY RECORD TO IO2                           
         XR    RF,RF                                                            
         ICM   RF,3,NEW.BEDRLEN                                                 
         LA    R0,OLD.BEDRECD                                                   
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRTRECX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  DIR,NEW,OLD                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE ALL INTERMEDIATE RECORDS IN BETWEEN BUT NOT       *         
* INCLUDING KEY1 AND KEY2                                             *         
*                                                                     *         
* NTRY: P1 = A(KEY 1)                                                 *         
*       P2 = A(KEY 2)                                                 *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  NTR1  ,                                                                
         LM    RE,RF,0(R1)                                                      
KEY      USING BEDRECD,IOKEY                                                    
         MVC   KEY.BEDKEY,0(RE)                                                 
         MVC   DRUPPER,0(RF)                                                    
U        USING BEDRECD,DRUPPER                                                  
         GOTO1 INCKEY,KEY.BEDKEY                                                
*                                                                               
         OC    KEY.BEDKLVL,KEY.BEDKLVL TEST KEY IS HEADER RECORD                
         BNZ   DRECS10                                                          
         OC    U.BEDKLVL,U.BEDKLVL TEST UPPER ISN'T                             
         BZ    DRECS10                                                          
         TM    EDUPLD,EDSNDADV     TEST ADVANCES BEING SENT                     
         BO    DRECS10                                                          
         MVC   KEY.BEDKLVL,=AL2(BEDKLADV)  NO - DON'T DELETE THEM               
         MVC   KEY.BEDKWHER,BCEFFS                                              
*                                                                               
DRECS10  DS    0H                                                               
         CLC   KEY.BEDKEY,DRUPPER                                               
         BE    EXIT                                                             
         BL    *+6                                                              
         DC    H'0'                RECORDS MUST COME IN ASCENDING ORDER         
*                                                                               
         LA    R1,IOHIGH+IOACCDIR+IO3+IOLOCK                                    
         B     *+8                                                              
DRECS12  LA    R1,IOSEQ+IOACCDIR+IO3+IOLOCK                                     
         GOTO1 AIO                                                              
         BNE   DELRECSX                                                         
         CLC   KEY.BEDKEY,DRUPPER                                               
         BNL   DELRECSX                                                         
         OI    KEY.BEDKSTA,BEDSDELT                                             
*                                                                               
         CLI   KEY.BEDKTYP,BEDKTYPQ                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY.BEDKCPY,CUABIN                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY.BEDKSUB,BEDKSUBQ                                             
         BE    *+14                                                             
         CLI   KEY.BEDKSUB,BEDPSUBQ                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         OI    BEDRSTA-BEDRECD(RF),BEDSDELT                                     
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DRECS12                                                          
*                                                                               
DELRECSX B     EXIT                                                             
         DROP  KEY,U                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INCREASE RECORD KEY VALUE BY 1                           *         
*                                                                     *         
* NTRY: R1 = A(RECORD KEY)                                            *         
***********************************************************************         
         SPACE 1                                                                
INCKEY   NTR1  ,                                                                
         LA    R1,L'ACCKEY-1(R1)                                                
         LA    R0,L'ACCKEY                                                      
*                                                                               
IKEY02   IC    RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   INCKEYX                                                          
         BCTR  R1,0                                                             
         BCT   R0,IKEY02                                                        
         DC    H'0'                                                             
*                                                                               
INCKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL OVERLAY                                             *         
*                                                                     *         
* NTRY: OVRLAY = OVERLAY NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
CALLOVR  NTR1  ,                                                                
         LR    R3,R1                                                            
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL         (MUST USE AOVERWRK)                          
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLC   OVRLAY,LOVRLAY      TEST OVERLAY ALREADY LOADED                  
         BE    COVR02                                                           
         GOTO1 VCOLY,BOWORK2,(OVRLAY,0),0,0                                     
         MVC   AOVRLAY,0(R1)                                                    
*                                                                               
COVR02   ICM   RF,15,AOVRLAY                                                    
         LR    R1,R3                                                            
         BASR  RE,RF                                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
         SPACE 1                                                                
*                                  ** EDIT SEND/RECEIVE REQUEST **              
M#BED    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#BED) =(03)   ELEMENT CODE                                 
         DC    AL2(M#BEDX+1-M#BED) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(RCVREQL-CLB60)  LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDEDIT-CLB60)  SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR ACTION (SEE EDACT)          
         DC    CL5'ACT  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVACT-CLB60)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR BILL NUMBER                 
         DC    CL5'BILL#'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHBLNO)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVNUM-CLB60)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE FOR JOB CODE                    
         DC    CL5'JOB '           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BCJOBCOD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB60)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE FOR FORMAT NUMBER               
         DC    CL5'FMTNO'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'PBCKFMT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVFMT-CLB60)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE FOR FORMAT LANGUAGE             
         DC    CL5'FLANG'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'PBCKLANG)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVLANG-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             DISK ADDRESS OF BILL                         
         DC    CL5'DA  '           TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BEDKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             DISK ADDRESS OF BILL                         
         DC    CL5'UPLD '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'EDUPLD)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVUPLD-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             DISK ADDRESS OF BILL                         
         DC    CL5'DNLD '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'EDDNLD)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDNLD-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             DISK ADDRESS OF BILL                         
         DC    CL5'BLLPQ'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'REPSUBID)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBPQ-CLB60)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             DISK ADDRESS OF BILL                         
         DC    CL5'PSTPQ'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'REPSUBID)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVPPQ-CLB60)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BEDX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** UPDATE/DRAFT DETAILS **                   
M#UPD    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(4)              ELEMENT CODE                                 
         DC    AL2(M#UPDX+1-M#UPD) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'PQID'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(9)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'LVBNO'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BLHBLNO)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'PPQID'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(9)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'REPDA'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#UPDX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** ADVANCE DETAILS **                        
M#ADV    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(ADVELQ) =(05)   ELEMENT CODE                                 
         DC    AL2(M#ADVX+1-M#ADV) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(ADVELQ,ADVLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(RCVADVF-CLB60)  FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(RCVADVL-CLB60)  LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'INDS1 '         TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVINDS1)     DATA LENGTH                                  
         DC    AL1(ADVINDS1-ADVELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WC '            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVWC)        DATA LENGTH                                  
         DC    AL1(ADVWC-ADVELD)   DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'CULA '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVCULA)      DATA LENGTH                                  
         DC    AL1(ADVCULA-ADVELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'DATE '          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVDATE)      DATA LENGTH                                  
         DC    AL1(ADVDATE-ADVELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'REF '           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVREF)       DATA LENGTH                                  
         DC    AL1(ADVREF-ADVELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'HRS '           TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVHRS)       DATA LENGTH                                  
         DC    AL1(ADVHRS-ADVELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'RATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVRATE)      DATA LENGTH                                  
         DC    AL1(ADVRATE-ADVELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'NET '           TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVNET)       DATA LENGTH                                  
         DC    AL1(ADVNET-ADVELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'COM '           TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVCOM)       DATA LENGTH                                  
         DC    AL1(ADVCOM-ADVELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'NARR '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVADVN-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(SNDADVN-CLB60)  SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'CMRAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVCORT)      DATA LENGTH                                  
         DC    AL1(ADVCORT-ADVELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#ADVX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** RECORD KEY **                             
M#KEY    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(1)              ELEMENT CODE                                 
         DC    AL2(M#KEYX+1-M#KEY) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVKEYF-CLB60)  FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'LVL  '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BEDKLVL)      DATA LENGTH                                  
         DC    AL1(BEDKLVL-BEDKEY) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY1-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WHER '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BEDKWHER)     DATA LENGTH                                  
         DC    AL1(BEDKWHER-BEDKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY2-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'SECT '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BEDKSECT)     DATA LENGTH                                  
         DC    AL1(BEDKSECT-BEDKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY3-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'SLVL '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BEDKSLVL)     DATA LENGTH                                  
         DC    AL1(BEDKSLVL-BEDKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY4-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'SSUB '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BEDKSSUB)     DATA LENGTH                                  
         DC    AL1(BEDKSSUB-BEDKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY5-CLB60)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#KEYX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BLHELD **                                 
M#BLH    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(BLHELQ)         ELEMENT CODE                                 
         DC    AL2(M#BLHX+1-M#BLH) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(BLHELQ,BLHLN2Q) ELEMENT CODE/LENGTH                          
         DC    AL2(RCVBLHF-CLB60)  FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(RCVBLHL-CLB60)  LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHJOB)       DATA LENGTH                                  
         DC    AL1(BLHJOB-BLHELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'BLNO '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHBLNO)      DATA LENGTH                                  
         DC    AL1(BLHBLNO-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'BILD '          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE                                    
         DC    AL1(L'BLHBILD)      DATA LENGTH                                  
         DC    AL1(BLHBILD-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD+MDINNULL) INDICATORS                                
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'EXPD '          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE                                    
         DC    AL1(L'BLHEXPD)      DATA LENGTH                                  
         DC    AL1(BLHEXPD-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDEXPD-CLB60)  SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'TRND '          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE                                    
         DC    AL1(L'BLHTRND)      DATA LENGTH                                  
         DC    AL1(BLHTRND-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'DUED '          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE                                    
         DC    AL1(L'BLHDUED)      DATA LENGTH                                  
         DC    AL1(BLHDUED-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'FORM '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BLHFORM)      DATA LENGTH                                  
         DC    AL1(BLHFORM-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'LANG '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BLHLANG)      DATA LENGTH                                  
         DC    AL1(BLHLANG-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'CUR  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHCUR)       DATA LENGTH                                  
         DC    AL1(BLHCUR-BLHELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDCUR-CLB60)   SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
*        DC    AL1(MDELDL2)        ITEM LENGTH                                  
*        DC    AL2(10)             MAPPING CODE                                 
*        DC    CL5'RIND '          TEXT IDENTIFIER                              
*        DC    AL1(MDTCHQ)         DATA TYPE                                    
*        DC    AL1(L'BLHRIND)      DATA LENGTH                                  
*        DC    AL1(BLHRIND-BLHELD)  DATA DISPLACEMENT                           
*        DC    AL1(MDIELFLD)       INDICATORS                                   
*        DC    AL2(0)              RECEIVE ROUTINE                              
*        DC    AL2(0)              SEND ROUTINE                                 
*        DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'RATE '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BLHRATE)      DATA LENGTH                                  
         DC    AL1(BLHRATE-BLHELD)  DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
*        DC    AL1(MDELDL2)        ITEM LENGTH                                  
*        DC    AL2(12)             MAPPING CODE                                 
*        DC    CL5'RSHF '          TEXT IDENTIFIER                              
*        DC    AL1(MDTCHQ)         DATA TYPE                                    
*        DC    AL1(L'BLHRSHF)      DATA LENGTH                                  
*        DC    AL1(BLHRSHF-BLHELD)  DATA DISPLACEMENT                           
*        DC    AL1(MDIELFLD)       INDICATORS                                   
*        DC    AL2(0)              RECEIVE ROUTINE                              
*        DC    AL2(0)              SEND ROUTINE                                 
*        DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'BREF '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHBREF)      DATA LENGTH                                  
         DC    AL1(BLHBREF-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD+MDINNULL)                                           
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'BMOS '          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE                                    
         DC    AL1(L'BLHBMOS)      DATA LENGTH                                  
         DC    AL1(BLHBMOS-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDBMOS-CLB60)  SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
*        DC    AL1(MDELDL2)        ITEM LENGTH                                  
*        DC    AL2(15)             MAPPING CODE                                 
*        DC    CL5'DSCI '          TEXT IDENTIFIER                              
*        DC    AL1(MDTCHQ)         DATA TYPE                                    
*        DC    AL1(L'BLHDSCI)      DATA LENGTH                                  
*        DC    AL1(BLHDSCI-BLHELD) DATA DISPLACEMENT                            
*        DC    AL1(MDIELFLD)       INDICATORS                                   
*        DC    AL2(0)              RECEIVE ROUTINE                              
*        DC    AL2(0)              SEND ROUTINE                                 
*        DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(16)             MAPPING CODE                                 
         DC    CL5'DSC  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'BLHDSC)       DATA LENGTH                                  
         DC    AL1(BLHDSC-BLHELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
*        DC    AL1(MDELDL2)        ITEM LENGTH                                  
*        DC    AL2(17)             MAPPING CODE                                 
*        DC    CL5'SCHI '          TEXT IDENTIFIER                              
*        DC    AL1(MDTCHQ)         DATA TYPE                                    
*        DC    AL1(L'BLHSCHI)      DATA LENGTH                                  
*        DC    AL1(BLHSCHI-BLHELD) DATA DISPLACEMENT                            
*        DC    AL1(MDIELFLD)       INDICATORS                                   
*        DC    AL2(0)              RECEIVE ROUTINE                              
*        DC    AL2(0)              SEND ROUTINE                                 
*        DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(18)             MAPPING CODE                                 
         DC    CL5'SCH  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'BLHSCH)       DATA LENGTH                                  
         DC    AL1(BLHSCH-BLHELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*&&NOP                                                                          
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(19)             MAPPING CODE                                 
         DC    CL5'PDATE'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHPDATE)     DATA LENGTH                                  
         DC    AL1(BLHPDATE-BLHELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE                                 
         DC    CL5'PTIME'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHPTIME)     DATA LENGTH                                  
         DC    AL1(BLHPTIME-BLHELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE                                 
         DC    CL5'PNUM '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHPNUM)      DATA LENGTH                                  
         DC    AL1(BLHPNUM-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*&&                                                                             
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(22)             MAPPING CODE                                 
         DC    CL5'INDS1'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BLHINDS1)     DATA LENGTH                                  
         DC    AL1(BLHINDS1-BLHELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(23)             MAPPING CODE                                 
         DC    CL5'DUEDF'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BLHDUEDF)     DATA LENGTH                                  
         DC    AL1(BLHDUEDF-BLHELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(24)             MAPPING CODE                                 
         DC    CL5'LVNO '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHLVNO)      DATA LENGTH                                  
         DC    AL1(BLHLVNO-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD+MDINNULL)                                           
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(25)             MAPPING CODE                                 
         DC    CL5'NADV '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BLHNADV)      DATA LENGTH                                  
         DC    AL1(BLHNADV-BLHELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD+MDINNULL)                                           
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(26)             MAPPING CODE                                 
         DC    CL5'CURDP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'CURTDECP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(27)             MAPPING CODE                                 
         DC    CL5'CURPR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'CURTSYMB)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(28)             MAPPING CODE                                 
         DC    CL5'CURTB'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BLHX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** NAMELD **                                 
M#NAM    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(NAMELQ)         ELEMENT CODE                                 
         DC    AL2(M#NAMX+1-M#NAM) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(NAMELQ,NAMLN1Q) ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELPOS-CLB60) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'NAME '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(NAMEREC-NAMELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#NAMX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** SPAELD **                                 
M#SPA    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(SPAELQ)         ELEMENT CODE                                 
         DC    AL2(M#SPAX+1-M#SPA) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(SPAELQ,SPALNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELPOS-CLB60) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'TYPE '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'SPATYPE)      DATA LENGTH                                  
         DC    AL1(SPATYPE-SPAELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'ULACC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'SPAAULA)      DATA LENGTH                                  
         DC    AL1(SPAAULA-SPAELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#SPAX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** SCIELD **                                 
M#SCI    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(SCIELQ)         ELEMENT CODE                                 
         DC    AL2(M#SCIX+1-M#SCI) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(SCIELQ,SCILN1Q) ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELPOS-CLB60) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'TYPE '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'SCITYPE)      DATA LENGTH                                  
         DC    AL1(SCITYPE-SCIELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'AMNT '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'SCIAMNT)      DATA LENGTH                                  
         DC    AL1(SCIAMNT-SCIELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'ADMN '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'SCIADMN)      DATA LENGTH                                  
         DC    AL1(SCIADMN-SCIELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#SCIX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BMAELD **                                 
M#BMA    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(BMAELQ)         ELEMENT CODE                                 
         DC    AL2(M#BMAX+1-M#BMA) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(BMAELQ,BMALNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELPOS-CLB60) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'MOA  '          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE                                    
         DC    AL1(L'BMAMOA)       DATA LENGTH                                  
         DC    AL1(BMAMOA-BMAELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDBMOS-CLB60)  SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'INDS '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BMAINDS)      DATA LENGTH                                  
         DC    AL1(BMAINDS-BMAELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'PC   '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'BMAPC)        DATA LENGTH                                  
         DC    AL1(BMAPC-BMAELD)   DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'AMT  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'BMAAMT)       DATA LENGTH                                  
         DC    AL1(BMAAMT-BMAELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BMAX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BFPELD **                                 
M#BFP_RCVHDRL  EQU ADDELEND-CLB60                                               
       ++INCLUDE ACCLB5#BFP                                                     
*                                                                               
*                                  ** BODY SECTION HEADER ELEMENT **            
M#PGH    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(PGHELQ)         ELEMENT CODE                                 
         DC    AL2(M#PGHX+1-M#PGH) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(PGHELQ,PGHLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELPOS-CLB60) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'NET  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'PGHNET)       DATA LENGTH                                  
         DC    AL1(PGHNET-PGHELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'COM  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'PGHCOM)       DATA LENGTH                                  
         DC    AL1(PGHCOM-PGHELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#PGHX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BFMELD AND BFXELD **                      
M#BFM_RCVHDRL  EQU ADDELEND-CLB60                                               
M#BFM_RCVSPACE EQU RCVSPACE-CLB60                                               
M#BFM_SNDTEXT  EQU SNDTEXT-CLB60                                                
       ++INCLUDE ACCLB5#BFM                                                     
*                                                                               
*                                  ** FIELD WITHIN TEXT ELEMENT **              
M#FWT    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(FWTELQ)         ELEMENT CODE                                 
         DC    AL2(M#FWTX+1-M#FWT) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(FWTELQ,FWTLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELPOS-CLB60) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'FLD  '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'FWTFLD)       DATA LENGTH                                  
         DC    AL1(FWTFLD-FWTELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'AMT  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE                                    
         DC    AL1(L'FWTAMT)       DATA LENGTH                                  
         DC    AL1(FWTAMT-FWTELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#FWTX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** RECORD ACTIVITY ELEMENT **                
M#RAC_RCVHDRF  EQU RCVRAC-CLB60                                                 
M#RAC_RCVHDRL  EQU ADDELPOS-CLB60                                               
M#RAC_SNDPERS  EQU SNDPERS-CLB60                                                
       ++INCLUDE ACCLB5#RAC                                                     
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   BEWSOLY                                                          
EDACT    DS    CL1                 ACTION                                       
EDACTGET EQU   C'G'                GET BILL FROM BILL NUMBER                    
EDACTPUT EQU   C'P'                SAVE BILL TO FILE                            
EDACTRD  EQU   C'R'                READ BILL                                    
EDACTLAT EQU   C'L'                GET LATEST BILL FOR JOB                      
EDACTDRA EQU   C'D'                CREATE DRAFT BILL                            
EDACTUPD EQU   C'U'                UPDATE BILL                                  
EDACTNEW EQU   C'N'                CREATE NEW BILL                              
EDACTPQU EQU   C'Q'                SEND LIVE BILL TO PRINT QUEUE                
*                                                                               
EDUPLD   DS    XL1                 UPLOAD (PC -> MAINFRAME) INDICATOR           
EDDNLD   DS    XL1                 DOWNLOAD (MAINFRAME -> PC) INDICATOR         
EDSNDHDR EQU   X'80'               UP/DOWNLOAD BILL HEADER                      
EDSNDADV EQU   X'40'               UP/DOWNLOAD ADVANCES                         
EDSNDPAR EQU   X'20'               UP/DOWNLOAD PARAGRAPHS                       
EDSNDTOT EQU   X'10'               DOWNLOAD TOTAL PARAS (IF CHANGED)            
*                                                                               
EDINDS   DS    XL1                 INDICATOR BYTE                               
EDIBHDR  EQU   X'80'               BILL HEADER HAS BEEN WRITTEN TO FILE         
EDIRDFT  EQU   X'40'               RECEIVED BILL IS A DRAFT                     
*                                                                               
KEYRCV   DS    XL(L'BEDKEY)        BILL EDIT KEY RECEIVED                       
STARCV   DS    XL(L'BEDKSTA)       BILL EDIT STATUS AREA RECEIVED               
*                                                                               
PUTINDS  DS    XL1                 PUT RECORD INDICATORS                        
PUTIROF  EQU   X'80'               RECORD PREVIOUSLY ON FILE                    
PUTIFST  EQU   X'40'               FIRST RECORD PUT TO FILE                     
PUTIRECI EQU   X'20'               RECORD INITIALIZED                           
PUTIOVER EQU   X'10'               TEST FOR OVERWITE EXISTING RECORD            
SECTSEQ  DS    XL1                 SEQUENCE NUMBER FOR BEDKSSEQ                 
PARASEQ  DS    XL2                 SEQUENCE NUMBER FOR BEDKPARA                 
ADVSEQ   DS    XL1                 SEQUENCE NUMBER FOR ADVANCES                 
ADDRAC   DS    XL(RACLNQ)          SAVED RACELD FOR ADD                         
*                                                                               
OVRLAY   DS    XL1                 OVERLAY TO BE LOADED BY CALLOVR              
AOVRLAY  DS    AL4                                                              
LOVRLAY  DS    XL1                 LAST OVERLAY LOADED                          
*                                                                               
LASTKEY  DS    XL(L'BEDKEY)        LAST KEY (USED BY PUTBED)                    
*                                                                               
DRUPPER  DS    XL(L'ACCKEY)        UPPER KEY (USED BY DELRECS)                  
*                                                                               
NARRADV  DS    CL200               NARRATIVE FOR ADVANCE                        
*                                                                               
SVPBPARM DS    XL(L'PBPARMS)                                                    
THISPQID DS    CL9                                                              
*                                                                               
         DS    (BEWSOLYL-(*-BEWSOLY))X                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160ACCLB60   08/16/00'                                      
         END                                                                    

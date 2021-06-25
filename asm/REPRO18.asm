*          DATA SET REPRO18    AT LEVEL 001 AS OF 04/28/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A18A                                                                  
T80A18   TITLE 'REPRO18 - PROPROSAL RECORDS - DAYPART ACT. OVERLAY'             
PRO18    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO18*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
*****************************                                                   
** KEY SCREEN CODE EQUATES **                                                   
*****************************                                                   
KYDEF    EQU   0                                                                
         SPACE 1                                                                
******************************                                                  
** DATA SCREEN CODE EQUATES **                                                  
******************************                                                  
DTDEF    EQU   C'4'                                                             
         EJECT                                                                  
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         L     RF,=A(TABLEOO)                                                   
         A     RF,BORELO           KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             UNPROT ON NTRSES                     
         OI    GSINDSL1,GSINOIO+GSIXKEY    WE'LL DO THE IO'S                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                                    
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
KEY      LM    R0,R2,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE KEY FIELDS ON THE SCREEN                         
***********************************************************************         
KEYFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   DON'T KNOW IF KEY CHANGED YET          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE KEY FIELDS ON THE SCREEN                   
***********************************************************************         
KEYLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                                 
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
RECRD    LM    R0,R2,SVPARMS                                                    
         LA    RF,RECRDTBL                                                      
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
RECRDTBL DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE CONTROLLER CALLS THE I/O ACTION                                    
***********************************************************************         
RECFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DS    0H                                                               
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFRWRT)      WRITE                      
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE I/O CALL TO WRITE THE RECORD                                       
***********************************************************************         
RFRWRT   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    NEWDPTS,NEWDPTS                                                  
         XC    DELDPTS,DELDPTS                                                  
***************                                                                 
* BOOK EXTENTSION ELEMENT(S)                                                    
***************                                                                 
         LA    R2,SAVXBKS                                                       
         USING XBOKLIN,R2                                                       
         LA    R3,SAVBKS                                                        
         USING BOOKLIN,R3                                                       
RFRW002  DS    0H                                                               
         OC    0(L'SAVBK,R3),0(R3)              BOOK?                           
         BZ    RFRW030                          NO - NEXT                       
*                                                                               
         LA    R6,MINEKEY                                                       
         USING RPRBXELD,R6                                                      
         MVI   MINEKEY,RPRBXELQ                                                 
         MVC   MINEKEY+1(1),BKLNIORD                                            
         BAS   RE,MINIOHI                                                       
*                                                                               
         L     R6,MINELEM                        ARLEADY THERE                  
         CLI   RPRBXEL,RPRBXELQ                                                 
         BNE   *+14                                                             
         CLC   MINEKEY+1(7),2(R6)                                               
         BE    RFRW004                           YES - GO PARSE IT              
*                                                                               
         OC    0(L'SAVXBK,R2),0(R2)                                             
         BZ    RFRW030                                                          
*                                                                               
         XC    0(256,R6),0(R6)     ADD NEW ELEMENT                              
         MVI   RPRBXEL,RPRBXELQ                                                 
         MVI   RPRBXLEN,RPRBXLNQ                                                
         MVC   RPRBXIOR,BKLNIORD                                                
         MVC   RPRBXDPT,XBLNDPT                                                 
*                                                                               
         BAS   RE,MINIOADD         ADD THE BOOK ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,BKLNDORD         SLOT IN NEWDPTS                              
         BCTR  R1,0                                                             
         MH    R1,=Y(L'NEWDPT)                                                  
         LA    R1,NEWDPTS(R1)                                                   
         MVI   0(R1),FF            ALL NEW FLAG                                 
*                                                                               
         B     RFRW030             NEXT LINE                                    
*                                                                               
RFRW004  DS    0H                  PROCESS EXISTING ELEMENT                     
         OC    0(L'SAVXBK,R2),0(R2)                                             
         BZ    RFRW020                                                          
*                                                                               
         ZIC   RE,BKLNDORD         SLOT IN NEWDPTS                              
         BCTR  RE,0                                                             
         MH    RE,=Y(L'NEWDPT)                                                  
         LA    RE,NEWDPTS(RE)                                                   
*                                                                               
         LA    R4,XBLNDPT          NEW LIST                                     
RFRW006  DS    0H                                                               
         LA    R1,RPRBXDPT         OLD LIST                                     
RFRW007  DS    0H                                                               
         CLC   0(1,R4),0(R1)       MATCH?                                       
         BE    RFRW008             YES - OLD DAYPART                            
         LA    R1,1(R1)                                                         
         LA    R0,RPRBXDPT+L'RPRBXDPT                                           
         CR    R1,R0                                                            
         BL    RFRW007                                                          
*                                                                               
         MVC   0(1,RE),0(R4)       NEW DAYPART                                  
         LA    RE,1(RE)                                                         
         B     RFRW010                                                          
*                                                                               
RFRW008  DS    0H                                                               
         MVI   0(R1),0             EXISTING DAYPART MATCHED                     
*                                                                               
RFRW010  DS    0H                                                               
         LA    R4,1(R4)                                                         
         LA    R0,XBLNDPT+L'XBLNDPT                                             
         CR    R4,R0                                                            
         BL    RFRW006                                                          
*                                                                               
         ZIC   RF,BKLNDORD       SLOT IN DELDPTS                                
         BCTR  RF,0                                                             
         MH    RF,=Y(L'DELDPT)                                                  
         LA    RF,DELDPTS(RF)                                                   
*                                                                               
         LA    R1,RPRBXDPT         OLD LIST - DELETES REMAIN                    
RFRW012  DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   0(1,RF),0(R1)                                                    
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         LA    R0,RPRBXDPT+L'RPRBXDPT                                           
         CR    R1,R0                                                            
         BL    RFRW012                                                          
*                                                                               
         MVC   RPRBXDPT,XBLNDPT                                                 
*                                                                               
         BAS   RE,MINIOWRT         UPDATE THE BOOK ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     RFRW030             NEXT                                         
*                                                                               
RFRW020  DS    0H                  ELEMENT DELETED                              
         ZIC   RF,BKLNDORD         SLOT IN DELDPTS                              
         BCTR  RF,0                                                             
         MH    RF,=Y(L'DELDPT)                                                  
         LA    RF,DELDPTS(RF)                                                   
         MVI   0(RF),FF            SET CLEAR ALL FLAGS                          
         MVC   1(L'RPRBXDPT,RF),RPRBXDPT                                        
*                                                                               
         BAS   RE,MINIODEL         DELETE IT                                    
*                                                                               
RFRW030  DS    0H                  DO NEXT                                      
         LA    R2,L'SAVXBK(R2)                                                  
         LA    R3,L'SAVBK(R3)                                                   
         LA    RE,SAVBKS+L'SAVBKS                                               
         CR    R3,RE                                                            
         BL    RFRW002                                                          
         DROP  R2,R3                                                            
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         MVC   0(L'MINXBKS,RE),SAVXBKS                                          
****************************************************                            
** UPDATE DETAIL LINES FOR NEW & DELETED DAYPARTS **                            
****************************************************                            
         OC    NEWDPTS,NEWDPTS                                                  
         BNZ   *+14                                                             
         OC    DELDPTS,DELDPTS                                                  
         BZ    RFRWX               NOTHING TO DO HERE                           
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   RFRW300                                                          
RFRW100  L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ                                                   
         BNE   RFRW300                                                          
*                                                                               
         LA    R2,NEWDPTS                                                       
         LA    R3,DELDPTS                                                       
         LA    R4,SAVBKS                                                        
         USING BOOKLIN,R4                                                       
*                                                                               
RFRW102  DS    0H                                                               
         OC    0(L'SAVBK,R4),0(R4)                                              
         BZ    RFRW299                                                          
*                                                                               
         LA    RF,X'80'            NO FOUND                                     
         ZIC   RE,BKLNIORD                                                      
         SRL   RF,0(RE)                                                         
         STC   RF,BOBYTE1          GET CORRECT MASK FOR BOOK                    
*                                                                               
         OC    0(L'DELDPT,R3),0(R3)                                             
         BZ    RFRW200             NO DELETED DAYPARTS FOR THIS BOOK            
*                                                                               
         LA    RE,0(R3)                                                         
         LA    RF,L'DELDPT(R3)                                                  
         CLI   0(R3),X'FF'         DAYPARTS CLEARED?                            
         BNE   *+8                 NO                                           
         LA    RE,1(RE)            CLEAR SURPRESSED BOOKS                       
*                                                                               
RFRW104  DS    0H                                                               
         CLC   RPRDTDPT,0(RE)      DELETED DAYPART?                             
         BE    RFRW106             YES - CLEAR FLAGS                            
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    RFRW104                                                          
         B     RFRW108                                                          
*                                                                               
RFRW106  DS    0H                                                               
         MVI   BOBYTE2,FF                                                       
         XC    BOBYTE2,BOBYTE1                                                  
         NC    RPRDTBKS,BOBYTE2                                                 
*                                                                               
RFRW108  DS    0H                                                               
         CLI   0(R3),X'FF'         DAYPARTS CLEARED?                            
         BE    RFRW299             NEXT BOOK                                    
*                                                                               
RFRW200  DS    0H                  CHECK NEWDPTS                                
         OC    0(L'NEWDPT,R2),0(R2)                                             
         BZ    RFRW299             NO NEW DAYPARTS FOR THIS BOOK                
*                                                                               
         CLI   0(R2),X'FF'         ALL NEW DAYPARTS?                            
         BNE   RFRW202             NO                                           
*                                                                               
         ZIC   RE,BKLNDORD                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'SAVXBK)                                                  
         LA    RE,SAVXBKS(RE)                                                   
         LA    RE,XBLNDPT-XBOKLIN(RE)                                           
         LA    RF,L'XBLNDPT(RE)                                                 
         B     RFRW204                                                          
*                                                                               
RFRW202  DS    0H                                                               
         LA    RE,0(R2)                                                         
         LA    RF,L'NEWDPT(R2)                                                  
*                                                                               
RFRW204  DS    0H                                                               
         CLC   RPRDTDPT,0(RE)      NEW DAYPART?                                 
         BE    RFRW206             YES - SUPPRESS THIS BOOK                     
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    RFRW204                                                          
         B     RFRW208                                                          
*                                                                               
RFRW206  DS    0H                                                               
         OC    RPRDTBKS,BOBYTE1    SUPPRESS THIS BOOK                           
*                                                                               
RFRW208  DS    0H                                                               
************************                                                        
** PROCCESS NEXT BOOK **                                                        
************************                                                        
RFRW299  DS    0H                                                               
         LA    R4,L'SAVBK(R4)                                                   
         LA    R2,L'NEWDPT(R2)                                                  
         LA    R3,L'DELDPT(R3)                                                  
         LA    R0,SAVBKS+L'SAVBKS                                               
         CR    R4,R0                                                            
         BL    RFRW102                                                          
         DROP  R4                                                               
*                                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RFRW100                                                          
*                                                                               
RFRW300  DS    0H                                                               
*                                                                               
RFRWX    B     EXITOK                                                           
         DROP  R5,R6                                                            
         SPACE 2                                                                
***********************************************************************         
* AFTER THE CONTROLLER CALLS THE I/O ACTION                                     
***********************************************************************         
RECLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DS    0H                                                               
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLRWRT)      WRITE                      
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE I/O CALL TO WRITE THE RECORD                                        
***********************************************************************         
RLRWRT   DS    0H                                                               
         BAS   RE,MINIOCLS                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECTS FOR KEY DATA OR RECORD DATA                                      
*                                                                               
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                            
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION                   
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                            
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                                        
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                           
*                                                                               
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                                    
***********************************************************************         
DATA     ICM   R1,15,SVPARMS2      DOING ACTION ON SPECIFIC DATA OBJ?           
         BNZ   DATA10              YES                                          
***********************************************************************         
************** DOING A GLOBAL ACTION ON ENTIRE RECORD *****************         
***********************************************************************         
         L     R2,SVPARMS4         R2 = A(RECORD)                               
         SR    R1,R1                                                            
         IC    R1,SVPARMS3         R1 = GLOBAL ACTION                           
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE DATA FIELDS ON THE SCREEN                        
***********************************************************************         
DTAFRST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
DFDDIS   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         XC    0(L'MINBKS,RE),0(RE)                                             
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         XC    0(L'MINLBLS,RE),0(RE)                                            
*                                                                               
         XC    SAVBKS,SAVBKS                                                    
         XC    SAVLBLS,SAVLBLS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ    GET THE BOOK ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   DFDDBKX                                                          
*                                                                               
         USING BOOKLIN,R4                                                       
*                                                                               
DFDDBK10 L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
         ZIC   R4,RPRBKDOR         DISPLAY ORDER NUMBER                         
         BCTR  R4,0                                                             
         LR    RE,R4                                                            
         MH    R4,=Y(L'SAVBK)                                                   
         LA    R4,SAVBKS(R4)                                                    
         MVC   BKLNIORD,RPRBKIOR                                                
         MVC   BKLNDORD,RPRBKDOR                                                
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    DFDDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
*                                                                               
         ZIC   R4,RPRBKDOR                                                      
         BCTR  R4,0                                                             
         MH    R4,=Y(L'MINBK)                                                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS-TWAD)                                               
         AR    R4,R0                                                            
         MVC   BKLNIORD,RPRBKIOR                                                
         MVC   BKLNDORD,RPRBKDOR                                                
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
         B     DFDDBK50                                                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
DFDDBK20 DS    0H                                                               
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
         MVC   BKLNUPGD,RPRBKBKS    UPGRADE FORMULA                             
         MVC   BKLNXBKS,RPRBKXBK    EXTRA BASE BOOKS                            
*                                                                               
         MH    RE,=Y(L'SAVLBL)                                                  
         LA    RE,SAVLBLS(RE)                                                   
         MVC   0(L'SAVLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
         ZIC   R4,RPRBKDOR                                                      
         BCTR  R4,0                                                             
         LR    RE,R4                                                            
         MH    R4,=Y(L'MINBK)                                                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS-TWAD)                                               
         AR    R4,R0                                                            
         MVC   BKLNIORD,RPRBKIOR                                                
         MVC   BKLNDORD,RPRBKDOR                                                
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
         MVC   BKLNUPGD,RPRBKBKS    UPGRADE FORMULA                             
         MVC   BKLNXBKS,RPRBKXBK    EXTRA BASE BOOKS                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         MH    RE,=Y(L'MINLBL)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINLBLS-TWAD)                                              
         AR    RE,R0                                                            
         MVC   0(L'MINLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
DFDDBK50 BAS   RE,MINIOSEQ                                                      
         BNE   DFDDBKX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRBKELQ      BOOK ELEMENT STILL?                          
         BE    DFDDBK10                                                         
*                                                                               
DFDDBKX  DS    0H                                                               
***************                                                                 
* BOOK EXTENSION ELEMENT(S)                                                     
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         XC    0(L'MINXBKS,RE),0(RE)                                            
         XC    SAVXBKS,SAVXBKS                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBXELQ    GET THE BOOK EXTENSION ELEMENT               
         BAS   RE,MINIOHI                                                       
         BNE   RDBDBXX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBXELD,R6                                                      
*                                                                               
RDBDBX10 CLI   0(R6),RPRBXELQ                                                   
         BNE   RDBDBX20                                                         
*                                                                               
         ZIC   R1,RPRBXIOR         INTERNAL ORDER NUMBER                        
         LA    RE,SAVBKS                                                        
         LA    R0,SAVBKS+L'SAVBKS                                               
         USING BOOKLIN,RE                                                       
         SR    RF,RF                                                            
RDBDBX12 CLM   R1,1,BKLNIORD                                                    
         BE    RDBDBX14                                                         
         LA    RE,L'SAVBK(RE)                                                   
         LA    RF,1(RF)                                                         
         CR    RE,R0                                                            
         BL    RDBDBX12                                                         
         B     RDBDBX16            NO BOOK IGNORE                               
         DROP  RE                                                               
*                                                                               
RDBDBX14 LR    R1,RF                                                            
         MH    R1,=Y(L'MINXBK)                                                  
         AR    R1,RA                                                            
         AH    R1,=Y(MINXBKS-TWAD)                                              
         USING XBOKLIN,R1                                                       
         MVC   XBLNDPT,RPRBXDPT                                                 
         MVC   XBLNFLG,RPRBXFLG                                                 
         DROP  R1                                                               
*                                                                               
RDBDBX16 BAS   RE,MINIOSEQ                                                      
         BE    RDBDBX10                                                         
*                                                                               
RDBDBX20 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         MVC   SAVXBKS,0(RE)                                                    
*                                                                               
RDBDBXX  DS    0H                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
         LR    R2,RA                                                            
         AH    R2,=Y(MINDPTS-TWAD)                                              
         XC    0(L'MINDPTS,R2),0(R2)                                            
         USING DPTLIN,R2                                                        
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ    GET THE DAYPART ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   DFDDDPX                                                          
*                                                                               
DFDDDP10 L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
         MVC   DPLNDPT,RPRDPDPT                                                 
         MVC   DPLNFLG,RPRDPFLG                                                 
         MVC   DPLNCPP,RPRDPTAB                                                 
         DROP  R2                                                               
*                                                                               
         LA    R2,L'MINDPT(R2)                                                  
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   DFDDDPX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRDPELQ      DAYPART ELEMENT STILL?                       
         BE    DFDDDP10                                                         
*                                                                               
DFDDDPX  DS    0H                                                               
************                                                                    
* STATIONS *                                                                    
************                                                                    
         LR    R4,RA                                                            
         AH    R4,=Y(MINSTAS-TWAD)                                              
         XC    0(L'MINSTAS,R4),0(R4)                                            
         USING STALIN,R4                                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BE    *+6                 NEED AT LEAST ONE                            
         DC    H'0'                                                             
         CLI   RPRSTEL,RPRSTELQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RPRSTICD,1          PRIMARY ?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   STLNFLG,RPRSTFLG                                                 
         MVC   STLNSTA,RPRSTSTA                                                 
         MVC   STLNIORD,RPRSTICD                                                
         DROP  R4                                                               
*                                                                               
DFDDCSX  DS    0H                                                               
         DROP  R5,R6                                                            
*                                                                               
DFDDIS99 MVI   ONBOOKLN,0                                                       
         MVI   ONBKHLN,0                                                        
         MVI   ONINFOLN,0                                                       
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         MVI   ONBOOKLN,0                                                       
         MVI   ONBKHLN,0                                                        
         MVI   ONINFOLN,0                                                       
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         MVC   SAVBKS,0(RE)                                                     
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         MVC   SAVLBLS,0(RE)                                                    
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         MVC   SAVXBKS,0(RE)                                                    
*                                                                               
DFDVALX  B     EXITOK                                                           
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         L     RF,=A(KNOWTAB)      TABLE OF KNOWN OBJECTS                       
         A     RF,BORELO                                                        
         USING KNOWTABD,RF                                                      
*                                                                               
DATA20   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA30              YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA20                                                           
***********************************                                             
* WE KNOW OF THIS DATA OBJECT                                                   
***********************************                                             
DATA30   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         DROP  RF                                                               
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FRRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRREL                                                        
         USING FRRELD,R3           R3=A(FRREL ON RECORD)                        
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTRACT NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CONDTA   DS    0H                                                               
         LA    RF,CONTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CONTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCON)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCON)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
DISCON   DS    0H                                                               
         USING RPROKMST,R2                                                      
         ZAP   BOWORK1+20(5),=P'99999999' EDIT USES 17 BYTES OF WORK            
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),RPROKCON                                           
         SP    BOWORK1+20(5),BOWORK1+10(5)                                      
         DROP  R2                                                               
         EDIT  (P5,BOWORK1+20),(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,      X        
               DUB=BODUB1                                                       
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A CONTRACT FIELD                                                     
***********************************************************************         
VALCON   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQCNT',CNTPROFS)                 
         TM    CONPROFS+CNTMEDTB,CNTMEDTA                                       
         BZ    VALCONX                                                          
*                                                                               
         TM    CCONFLG1,CCONSARQ                                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(601)                                                
         B     EXITL                                                            
*                                                                               
VALCONX  OI    FVIIND,FVIVAL       VALIDATED                                    
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
NTRCON   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY CONTRACT NUMBER?              
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    NTRCONX             NO                                           
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
NTRCONX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROPOSAL NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRODTA   DS    0H                                                               
         LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PROTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRPRO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A PROPOSAL FIELD                                                      
***********************************************************************         
DISPRO   DS    0H                                                               
         USING RPROKEY,R2                                                       
         ZIC   RE,RPROKPRO                                                      
         LA    R0,X'FF'                                                         
         SR    R0,RE                                                            
         EDIT  (R0),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1                
         DROP  R2                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A PROPOSAL FIELD                                                     
***********************************************************************         
VALPRO   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         GOTOX (VALPROQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
*                                                                               
VALPROX  B     EXITOK                                                           
***********************************************************************         
* PASS PROPOSAL NUMBER TO NEXT SESSION                                          
***********************************************************************         
NTRPRO   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY PROPOSAL NUMBER?              
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         CLI   0(RE),0                                                          
         BE    NTRPROX             NO                                           
*                                                                               
         MVC   BOBYTE1,0(RE)                                                    
         XI    BOBYTE1,X'FF'                                                    
         EDIT  (B1,BOBYTE1),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
NTRPROX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STADTA   DS    0H                                                               
         LA    RF,STATBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTA)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISSTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         USING STALIN,RE                                                        
         MVC   FVIFLD(L'STLNSTA),STLNSTA                                        
         DROP  RE                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SATELLITE STATION                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STLDTA   DS    0H                                                               
         LA    RF,STLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTL)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SATELLITE STATION                                                     
***********************************************************************         
DISSTL   DS    0H                                                               
         OI    FVATRB,FVAPROT                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD) PRIMARY STARTION ENTRY                       
         USING STALIN,RE                                                        
         MVI   FVIFLD,C'N'                                                      
         TM    STLNFLG,RPRSTSTL                                                 
         BZ    *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         B     EXITOK                                                           
         DROP  RE                                                               
***********************************************************************         
* DATA OBJECT FOR INFORMATION FIELD                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
INFODTA  DS    0H                                                               
         LA    RF,INFOTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
INFOTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISINFO)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY INFORMATION FIELD                                                     
***********************************************************************         
DISINFO  DS    0H                                                               
         OI    FVATRB,FVAPROT                                                   
         ZIC   R1,ONINFOLN                                                      
         LA    R0,1(R1)                                                         
         STC   R0,ONINFOLN                                                      
*                                                                               
         MH    R1,=Y(L'INFOMSG)                                                 
         LA    R1,INFOMSG(R1)                                                   
         MVC   FVIFLD(L'INFOMSG),0(R1)                                          
*                                                                               
         CLI   ONINFOLN,2          DAYPART LINE?                                
         BNE   DISINFOX            NO                                           
*                                                                               
         LR    R2,RA                                                            
         AH    R2,=Y(MINDPTS-TWAD)                                              
         USING DPTLIN,R2                                                        
         LA    R0,L'MINDPTS(R2)                                                 
         LA    R1,FVIFLD+50                                                     
DISINFO2 CLI   DPLNDPT,C' '                                                     
         BNH   DISINFO4                                                         
         MVC   0(1,R1),DPLNDPT                                                  
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)                                                         
         LA    R2,L'MINDPT(R2)                                                  
         CR    R2,R0                                                            
         BL    DISINFO2                                                         
         DROP  R2                                                               
*                                                                               
DISINFO4 DS    0H                                                               
         BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
*                                                                               
DISINFOX B     EXITOK                                                           
*                                                                               
INFOMSG  DS    0CL75                                                            
         DC    CL50'ENTER IN DAYPARTS FIELD THOSE DAYPARTS FOR WHICH Y'         
         DC    CL25'OU WANT TO SUPPRESS THE'                                    
         DC    CL50'BOOK FROM PRINTING.    PROPOSAL HEADER DAYPARTS: '          
         DC    CL25' '                                                          
***********************************************************************         
* DATA OBJECT FOR BOOKS'S HEADING                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BKHDTA   DS    0H                                                               
         LA    RF,BKHTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BKHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBKH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY HEADING FOR THE RATING FIELD                                          
***********************************************************************         
DISBKH   DS    0H                                                               
         OI    FVATRB,FVAPROT                                                   
         ZIC   R1,ONBKHLN          ON WHICH BOOK                                
         LA    R0,1(R1)                                                         
         STC   R0,ONBKHLN                                                       
*                                                                               
         LR    R2,R1                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         MH    R2,=Y(L'SAVLBL)                                                  
         LA    R2,SAVLBLS(R2)                                                   
*                                                                               
         OC    0(BKLNLENQ,R1),0(R1)                                             
         BE    DISBKHX                                                          
*                                                                               
         OC    0(L'SAVLBL,R2),0(R2)             USER DEFINED BOOK?              
         BZ    *+14                             NO                              
         MVC   FVIFLD(L'SAVLBL),0(R2)                                           
         B     DISBKHX                                                          
*                                                                               
         USING BOOKLIN,R1                                                       
         XC    BOWORK2,BOWORK2                                                  
         MVC   BOWORK2(L'FVIHDR),FVIHDR                                         
         ZIC   RE,BOWORK2                                                       
         LA    RE,5(RE)                                                         
         STC   RE,BOWORK2                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK    JUST SHOW THE 1ST (PRIMARY BOOK)            
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         DROP  R1                                                               
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BOWORK1),BOWORK2,          X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,BOWORK2                                                       
         LA    RE,BOWORK2(RE)                                                   
         TM    BOWORK2+1,X'02'       EXT FIELD HDR?                             
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   DSHXBK05                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DSHXBK05                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DSHXBK05 DS    0H                                                               
         LA    RF,BOWORK2+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),BOWORK2+8                                              
*                                                                               
DISBKHX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BOOK/DAYPART ASSOCIATION                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BDADTA   DS    0H                                                               
         LA    RF,BDATBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BDATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBDA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBDA)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BOOK/DAYPART ASSOCIATION FIELD                                        
***********************************************************************         
DISBDA   DS    0H                                                               
         ZIC   R1,ONBOOKLN                                                      
         LA    R0,1(R1)                                                         
         STC   R0,ONBOOKLN                                                      
*                                                                               
         OI    FVIIND,FVIVAL                                                    
         XC    FVIFLD,FVIFLD                                                    
*                                                                               
         MH    R1,=Y(L'SAVXBK)                                                  
         LA    R1,SAVXBKS(R1)                                                   
         USING XBOKLIN,R1                                                       
         MVC   FVIFLD(L'XBLNDPT),XBLNDPT                                        
         DROP  R1                                                               
*                                                                               
DISBDAX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE BOOK/DAYPART ASSOCIATION FIELD                                       
***********************************************************************         
VALBDA   DS    0H                                                               
         ZIC   R3,ONBOOKLN                                                      
         LA    R0,1(R3)                                                         
         STC   R0,ONBOOKLN                                                      
*                                                                               
         LTR   R3,R3                                                            
         BNZ   *+10                                                             
         XC    SAVXBKS,SAVXBKS                                                  
*                                                                               
         LR    R2,R3                                                            
         MH    R3,=Y(L'SAVXBK)                                                  
         LA    R3,SAVXBKS(R3)                                                   
         MH    R2,=Y(L'SAVBK)                                                   
         LA    R2,SAVBKS(R2)                                                    
*                                                                               
         OC    0(L'SAVBK,R2),0(R2)                                              
         BNZ   VALBDA10                                                         
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     VALBDAX                                                          
*                                                                               
VALBDA10 DS    0H                                                               
         USING XBOKLIN,R3                                                       
         XC    XBLNDPT,XBLNDPT                                                  
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALBDAX                                                          
         CLI   FVILEN,L'XBLNDPT                                                 
         BH    EXITNV                                                           
*                                                                               
         ZIC   R2,FVILEN                                                        
VALBDA12 DS    0H                                                               
         ZIC   RE,FVILEN           CHECK FOR REPEATS                            
         LA    R0,FVIFLD(RE)                                                    
         LA    RE,FVIFLD-1(R2)                                                  
         LA    RF,1(RE)                                                         
VALBDA14 CR    RF,R0                                                            
         BNL   VALBDA16                                                         
         CLC   0(1,RE),0(RF)                                                    
         BE    VALBDAER                                                         
         LA    RF,1(RF)                                                         
         B     VALBDA14                                                         
*                                                                               
VALBDA16 LA    R0,FVIFLD-1(R2)                                                  
         GOTO1 =A(VALDPT),BODMCB,(R0),RR=BORELO                                 
         BNE   VALBDAER                                                         
*                                                                               
         BCT   R2,VALBDA12                                                      
*                                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   XBLNDPT(0),FVIFLD                                                
         DROP  R3                                                               
*                                                                               
VALBDAX  XC    FVERRNDX,FVERRNDX                                                
         B     EXITOK                                                           
*                                                                               
VALBDAER BCTR  R2,0                                                             
         STC   R2,FVERRNDX                                                      
         B     EXITNV                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAYPART                                                              
*                                                                               
*  PARAMETER 1 A(DAYPART CODE)                                                  
*                                                                               
*   USE HARDCODED TABLE IF THE CCONDPMQ BIT IS ON IN CCONFLG1                   
*     OTHERWISE USE THE DAYPART RECORDS                                         
***********************************************************************         
VALDPT   NTR1                                                                   
         L     R2,0(R1)                                                         
         TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    VALDPT60            YES                                          
*                                                                               
****************************************                                        
** READ DAYPART RECORD FOR VALIDATION **                                        
****************************************                                        
*                                                                               
         MVC   BODMCB(2),CUAALF    GET PARENT REP                               
         GOTOX (GETPRNT,AREPRO01),BODMCB                                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RRDPKEY,RE                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,BODMCB                                                  
         MVC   RRDPKDPT,0(R2)                                                   
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RRDPKEY),IOKEYSAV                                        
         BNE   EXITNV                                                           
*                                                                               
         B     VALDPT90                                                         
*                                                                               
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
VALDPT60 LR    R3,RB               VALIDATE THE DAYPART CODE                    
         AH    R3,=Y(DPTTABLE-PRO18)                                            
*                                                                               
VALDPT65 CLI   0(R3),X'FF'         DID WE HIT THE END OF DPTTABLE?              
         BE    EXITNV              YES, INVALID DAYPART CODE                    
*                                                                               
         CLC   0(1,R2),3(R3)                                                    
         BE    VALDPT90                                                         
         LA    R3,L'DPTTABLE(R3)                                                
         B     VALDPT65                                                         
*                                                                               
VALDPT90 DS    0H                  COPY THE 1-BYTE DAYPART CODE                 
*                                                                               
VALDPTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                            
***********************************************************************         
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(NTRXIN)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                                      
***********************************************************************         
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTROUT   DS    0H                                                               
         CLI   SREC,O#MAX          CHECK FOR CONTROLLER                         
         BNH   EXITOK                                                           
*                                                                               
         OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SCRN     LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRNTBL                                                       
         B     ITER                                                             
*                                                                               
SCRNTBL  DC    AL1(SSET),AL1(0,0,0),AL4(SETSCR)                                 
         DC    AL1(SKSET),AL1(0,0,0),AL4(SETKSCR)                               
         DC    AL1(SMOD),AL1(0,0,0),AL4(MODSCR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET THE KEY SCREEN CODE                                                       
***********************************************************************         
SETKSCR  DS    0H                                                               
         MVI   GSSKCODE,0          CLEAR THE CODE                               
*                                                                               
         MVI   GSSKCODE,KYDEF      SET TO DEFAULT SCREEN CODE                   
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE DATA SCREEN CODE                                                      
***********************************************************************         
SETSCR   DS    0H                                                               
         MVI   GSSMCODE,0          CLEAR THE CODE                               
*                                                                               
         MVI   GSSMCODE,DTDEF      SET TO DEFAULT SCREEN CODE                   
*                                                                               
SETSCRX  B     EXITOK                                                           
***********************************************************************         
* MODIFY THE SCREEN FIELDS (PULL OUT THE KEYS FROM AKYFLD)                      
***********************************************************************         
MODSCR   DS    0H                                                               
         L     R5,ATWA             SCREEN                                       
         LA    R5,64(R5)           SKIP HEADER                                  
*                                                                               
MODSCR10 CLI   0(R5),0             END OF SCREEN?                               
         BE    MODSCRX             YES, WE'RE DONE                              
*                                                                               
         TM    1(R5),X'02'         EXTENDED HEADER?                             
         BZ    MODSCRNX            NO, SKIP TO NEXT FIELD                       
*                                                                               
MODSCR20 LR    RF,R5               RF = A(EXTENDED FIELD HDR)                   
         ZIC   R0,0(R5)                                                         
         AR    RF,R0                                                            
         SH    RF,=H'8'                                                         
         USING FVIXHDR,RF                                                       
         ZIC   RE,FVIXUS2          RE = FIELD #                                 
         DROP  RF                                                               
         BCTR  RE,0                MAKE IT ZERO-BASED                           
         LTR   RE,RE                                                            
         BM    MODSCRNX            SKIP FLUFF                                   
*                                                                               
         SLL   RE,2                MULTIPLY BY 4                                
         L     R6,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R6,0(RE,R6)         A(THIS FIELD ENTRY)                          
         L     RF,0(R6)            THIS FIELD ENTRY                             
         USING FDRELD,RF                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         ZICM  RE,FDRNUM,2                                                      
         DROP  RF                                                               
*                                                                               
         LA    RF,KNOWTAB2                                                      
         USING KNOWTABD,RF                                                      
MODSCR30 CLC   KNOWID,=AL2(EOT)    REACH END OF KEY WANTED LIST?                
         BE    MODSCRNX            YES, CHECK NEXT SCREEN FIELD                 
         CLM   RE,3,KNOWID         IS THIS A WANTED KEY?                        
         BE    MODSCR40            YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     MODSCR30                                                         
         DROP  RF                                                               
*                                                                               
MODSCR40 SR    R0,R0               SEE IF WE HAVE DATA FOR THIS FIELD           
         L     RF,AKYFLD                                                        
         ZICM  R1,0(RF),2          R1 = A(AFTER LAST KEY FIELD ENTRY)           
         AR    R1,RF                                                            
         LA    RF,2(RF)            RF = A(1ST ENTRY IN KEY FIELD TBL)           
         USING KEYELD,RF                                                        
MODSCR45 CLI   KEYEL,KEYELQ                                                     
         BNE   MODSCRNX            NO DATA FOR THIS KEY FIELD                   
*                                                                               
         CLM   RE,3,KEYNUM         MATCH ON THIS FIELD?                         
         BE    MODSCR50                                                         
         IC    R0,KEYLN            NO                                           
         AR    RF,R0                                                            
         CR    RF,R1                                                            
         BNL   MODSCRNX                                                         
         B     MODSCR45                                                         
*                                                                               
MODSCR50 ZIC   R1,KEYLN            COPY THE DATA OVER TO THE FIELD              
         SH    R1,=Y(KEYLN1Q+1)                                                 
         BM    MODSCRNX                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R5),8(R5)       IS THERE ANYTHING HERE?                      
         BNZ   MODSCRNX            YES                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),KEYDATA                                                  
         LA    R1,1(R1)                                                         
         STC   R1,5(R5)                                                         
         OI    6(R5),X'80'         AND TRANSMIT IT                              
         DROP  RF                                                               
*                                                                               
MODSCRNX ZIC   R0,0(R5)            BUMP TO NEXT SCREEN FIELD                    
         AR    R5,R0                                                            
         B     MODSCR10                                                         
*                                                                               
MODSCRX  B     EXITOK                                                           
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(00002),AL4(FLDPROT)   PROPOSAL                               
         DC    AL2(EOT)                                                         
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                                  
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
PFKEY    LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKYTBL                                                       
         B     ITER                                                             
*                                                                               
PFKYTBL  DC    AL1(PFREC),AL1(0,0,0),AL4(RECPFK)                                
         DC    AL1(PFACT),AL1(0,0,0),AL4(ACTPFK)                                
         DC    AL1(PFUSER),AL1(0,0,0),AL4(USRPFK)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER NAME FOR THE PFKEY                                           
***********************************************************************         
USRPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD VERB                            
***********************************************************************         
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR A MINIO ELEMENT                                        
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINRD',(R5))                                     
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINHI',(R5))                                     
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINSEQ',(R5))                                    
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,PSSAV               COMING FROM WORK/UPDATE?                  
         USING SSAVD,R5                                                         
         CLC   =C'WORUPD',SDATA                                                 
         BNE   MINWRTX                NO                                        
         MVC   SDATA(7),=C'PROCHAY'   YES, DRASTIC CHANGE TO PROPOSAL           
*                                                                               
MINWRTX  B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINADD',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINDEL',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         ZIC   RF,MINNBUF          NUMBER OF BUFFERS BEING USED                 
         L     RE,MINBUFF          A(1ST BUFFER BEING USED BY MINIO)            
         USING RPROHDRD,RE                                                      
MNIODEL2 CLC   RPRORLEN,=Y(RPROR1ST-RPROHDRD)   ANY ELEMENTS IN RECORD?         
         BH    MNIODEL4                         YES, CHECK NEXT BUFFER          
*                                                                               
         MVC   RPRORLEN,=Y(RPROR1ST-RPROHDRD+2) DMDALINK: MIN IS 36             
         OI    RPRORSTA,X'80'                   MARK FOR DELETE                 
         LA    R1,RPROR1ST                                                      
         XC    0(3,R1),0(R1)                    FAKE ELEMENT                    
*                                                                               
MNIODEL4 AH    RE,MINFRCLM         BUMP TO NEXT MINIO BUFFER                    
         BCT   RF,MNIODEL2         LOOP UNTIL ALL BUFFERS CHECKED               
*                                                                               
MNIODELX B     EXITOK                                                           
         DROP  R5,RE                                                            
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         TM    MNIOFLAG,MNIOCLSQ   DO WE NEED TO?                               
         BZ    EXITOK              NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINCLS',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
***************                                                                 
* INFO EXITS                                                                    
***************                                                                 
EXITENTR MVC   FVMSGNO,=AL2(84)                                                 
         MVI   FVOMTYP,C'I'                                                     
         B     EXITL               EXIT WITH ENTER DATA                         
***************                                                                 
* ERROR EXITS                                                                   
***************                                                                 
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITRCNF MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD NOT ON FILE                 
EXITRCDL MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITL               EXIT WITH RECORD IS DELETED                  
EXITRCAE MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXITL               EXIT WITH RECORD ALREADY EXISTS              
EXITCRES MVC   FVMSGNO,=AL2(FVFXRES)                                            
         B     EXITL               EXIT WITH RECORD CAN'T BE RESTORED           
INVLUPGD MVC   FVMSGNO,=AL2(INVUPGRD)                                           
         B     EXITL               INVALID UPGRADE EXPRESSION                   
EXITIOBK MVC   FVMSGNO,=AL2(INVOVBOK)                                           
         B     EXITL               INVALID OVERRIDE BOOK                        
EXITIBKE MVC   FVMSGNO,=Y(INVBKEXP)                                             
         B     EXITL               INVALID BOOK EXPRESSION                      
EXITNMOR MVC   FVMSGNO,=AL2(NMOREPRO)                                           
         B     EXITL               NO MORE PROPOSALS FOR THIS CONTRACT          
EXITDUPL MVC   FVMSGNO,=AL2(401)                                                
         B     EXITL               DUPLICATE ENTRY NOT ALLOWED                  
EXITSLCK MVC   FVMSGNO,=AL2(GE$SLOCK)                                           
         B     EXITL               EXIT WITH SECURITY LOCKOUT                   
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* HARDCODED DAYPART TABLE                                                       
***********************************************************************         
DPTTABLE DS    0CL4                                                             
         DC    CL4'MNGM'           MORNING                                      
         DC    CL4'DAYD'           DAYTIME                                      
         DC    CL4'ELYE'           EARLY FRINGE                                 
         DC    CL4'ENWR'           EARLY NEWS                                   
         DC    CL4'ACCA'           PRIME ACCESS                                 
         DC    CL4'LNWT'           LATE NEWS                                    
         DC    CL4'LTEL'           LATE FRINGE                                  
         DC    CL4'WKDW'           WEEKEND                                      
         DC    CL4'KIDK'           KIDS                                         
         DC    CL4'FRGF'           FRINGE                                       
         DC    CL4'NWSN'           NEWS                                         
         DC    CL4'PRIP'           PRIME                                        
         DC    CL4'MOVV'           MOVIES                                       
         DC    CL4'SPES'           SPECIALS                                     
         DC    CL4'SPOJ'           SPORTS                                       
         DC    CL4'SPSO'           SOAPS                                        
         DC    CL4'COMU'           COMPETITIVE                                  
         DC    CL4'LOCX'           LOCAL                                        
         DC    CL4'OTHY'           OTHER                                        
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN OBJECTS TYPES                                                  
***********************************************************************         
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
* RECORD (PROTECTED PORTION)                                                    
         DC    AL2(00007),AL4(STADTA)    STATION                                
         DC    AL2(00048),AL4(STLDTA)    SATELLITE - UNPROTECTED                
         DC    AL2(00083),AL4(INFODTA)   INFORMATIONAL FIELDS                   
* RECORD (INPUT PORTION)                                                        
         DC    AL2(00082),AL4(BKHDTA)    BOOK HEADINGS                          
         DC    AL2(00079),AL4(BDADTA)    ASSOCIATED DAYPART FIELD               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
*                                                                               
CNTPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
CONPROFS DS    CL8                 PROFILE BITS                                 
*                                                                               
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1MXWRT EQU   X'20'                - NEED MINIO WRT ON DETAIL UPDATE           
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MNIOFLAG DS    XL1                 MINIO FLAG                                   
MNIOCLSQ EQU   X'80'               - A CHANGE WAS MADE, CLOSE MINIO             
*                                                                               
SAVBKS   DS    0XL(NUMBKS*BKLNLENQ)  DISPLAY BOOKS AND UPGRADES                 
SAVBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
SAVLBLS  DS    0XL(NUMBKS*5)         DISPLAY USER DEFINED LABELS                
SAVLBL   DS    (NUMBKS)XL5                                                      
*                                                                               
SAVXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
SAVXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
*                                                                               
ONBOOKLN DS    XL1                 WHICH BOOK                                   
ONBKHLN  DS    XL1                 WHICH BOOK HEADING                           
ONINFOLN DS    XL1                 WHICH INFO HEADING                           
*                                                                               
NEWDPTS  DS    0XL(NUMBKS*L'XBLNDPT)    NEW DAYPART THIS TIME                   
NEWDPT   DS    (NUMBKS)XL(L'XBLNDPT)                                            
*                                                                               
DELDPTS  DS    0XL(NUMBKS*(L'XBLNDPT+1))    DELETED DAYPART THIS TIME           
DELDPT   DS    (NUMBKS)XL(L'XBLNDPT+1)                                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
*                                                                               
MINSTAS  DS    0XL(NUMSTAS*STLNLENQ) SAVED STATIONS                             
MINSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
MINBKS   DS    0XL(NUMBKS*BKLNLENQ)  SAVED BOOKS AND UPGRADES                   
MINBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
MINLBLS  DS    0XL(NUMBKS*5)         SAVED USER DEFINED LABELS                  
MINLBL   DS    (NUMBKS)XL5                                                      
*                                                                               
MINXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
MINXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
*                                                                               
MINDPTS  DS    0CL(NUMDPTS*DPLNLENQ) SAVED DAYPARTS                             
MINDPT   DS    (NUMDPTS)XL(DPLNLENQ)                                            
         DS    XL(DPLNLENQ)          EOT                                        
*                                                                               
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
* FAFACTS                                                                       
* RECNTPROF                                                                     
* FASYSLSTD                                                                     
* DDDDEQUS                                                                      
* DDCOMFACS                                                                     
* CTMSGEQUS                                                                     
* FASELIST                                                                      
* FASYSFAC                                                                      
* DDSCANBLKD                                                                    
* DDFLDHDR                                                                      
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDDDEQUS                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTMSGEQUS                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001REPRO18   04/28/97'                                      
         END                                                                    

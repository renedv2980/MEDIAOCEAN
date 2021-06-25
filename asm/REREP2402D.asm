*          DATA SET REREP2402D AT LEVEL 160 AS OF 05/01/02                      
*PHASE RE2402D,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'MODULE TO DELETE RATE CARDS AND RDETAIL'                        
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
*        USES RCARDDELD - LIST OF RATE CARDS TO DELETE            *             
*                       - WHEN CHANGED, THIS MUST BE RELINKED     *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         L     RF,=V(HELLO)                                                     
         L     RE,RELO                                                          
         AR    RF,RE                                                            
         ST    RF,VHELLO                                                        
*                                                                               
         L     RF,=V(RECUP)                                                     
         L     RE,RELO                                                          
         AR    RF,RE                                                            
         ST    RF,VRECUP                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    HDRKEY,HDRKEY                                                    
         XC    ZCOUNT,ZCOUNT                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
*                                                                               
         CLI   RCARDAGY,X'FF'      ANY AGENCY FILTER?                           
         BE    *+10                                                             
         MVC   RINVKREP,RCARDAGY                                                
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         CLI   KEY,X'12'                                                        
         BNE   PC200               DONE W/ HEADERS - GO DEL RATE CARDS          
*                                                                               
         CLI   RCARDAGY,X'FF'      ANY AGENCY FILTER?                           
         BE    *+14                                                             
         CLC   RINVKREP,RCARDAGY                                                
         BNE   PC200                                                            
*                                                                               
         CLI   RINVKSRC,C'M'       MARKET FACT?                                 
         BE    PCSEQ                                                            
         CLI   RINVKSRC,C'S'       STATION FACT?                                
         BE    PCSEQ                                                            
*                                                                               
         CLI   RINVKSRC,0          INVENTORY HEADER?                            
         BNE   PCSEQ                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,INVIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         LA    R6,INVIO                                                         
         NI    MYFLAG,X'FF'-PRNTHDR                                             
*                                                                               
         MVI   ELCODE,X'06'        GET AVAIL RATE ELEMENT IN HEADER             
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
         USING RIMAELEM,R6                                                      
*                                                                               
         MVC   HDRKEY,KEY          SAVE AWAY HDR KEY                            
         XC    EQUTABLE,EQUTABLE                                                
         LA    R4,EQUTABLE                                                      
*                                                                               
         B     *+12                                                             
*                                                                               
PC50     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PC70                                                             
*                                                                               
PC53     DS    0H                                                               
         CLI   RCARDAGY,X'FF'      ANY AGENCY FILTER?                           
         BE    *+14                                                             
         CLC   RIMAREP,RCARDAGY                                                 
         BNE   PC50                                                             
*                                                                               
         LA    R3,RCARDDEL         LIST OF RATE CARDS TO DELETE                 
PC55     DS    0H                                                               
         CLI   0(R3),X'FF'         FINISHED COMPARING RATE CARDS?               
         BE    PC50                                                             
*                                                                               
         CLC   RIMACDE,0(R3)       REMOVE THIS RATE CARD?                       
         BNE   PC60                                                             
*                                                                               
         TM    MYFLAG,PRNTHDR                                                   
         BO    PC57                                                             
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(2),INVIO+10                                                    
         MVC   P+3(5),INVIO+12      PRINT HEADER INFO                           
         MVC   P+9(4),INVIO+17                                                  
         GOTO1 HEXOUT,DMCB,INVIO+21,P+15,3,=C'TOG'                              
         GOTO1 REPORT                                                           
         OI    MYFLAG,PRNTHDR                                                   
*                                                                               
PC57     DS    0H                                                               
         MVC   P(8),RIMACDE                                                     
         GOTO1 HEXOUT,DMCB,RIMANUM,P+10,1,=C'TOG'                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   0(1,R4),RIMANUM     SAVE AWAY TABLE OF EQUATE NUMBERS            
         LA    R4,1(R4)                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(X'02',INVIO),(R6),0,0                               
*                                                                               
         LA    R6,INVIO                                                         
         BAS   RE,GETEL                                                         
*                                                                               
         B     PC53                                                             
*                                                                               
PC60     DS    0H                                                               
         LA    R3,8(R3)                                                         
         B     PC55                                                             
         DROP  R6                                                               
*                                                                               
PC70     DS    0H                                                               
         OC    EQUTABLE,EQUTABLE                                                
         BZ    PCSEQ                                                            
         MVI   0(R4),X'FF'         DENOTE END OF EQU TABLE                      
*                                                                               
         GOTO1 HEXOUT,DMCB,EQUTABLE,P,21,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,INVIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE HEADER RECORD                         
         DC    H'0'                                                             
*                                                                               
         MVC   P(5),=C'RECUP'                                                   
         GOTO1 REPORT                                                           
*                                                                               
INVSEQ   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         CLC   KEY(24),HDRKEY      FALLS UNDER THIS INV HDR?                    
         BNE   PC150                                                            
*                                                                               
         CLI   RINVKSRC,C'Z'       RDETAIL RECORD?                              
         BNE   INVSEQ                                                           
*                                                                               
         LA    R4,EQUTABLE                                                      
PC100    DS    0H                                                               
         CLI   0(R4),X'FF'         FIND A MATCH IN EQUATE TABLE?                
         BE    INVSEQ              NO                                           
*                                                                               
         CLC   RINVKNUM,0(R4)                                                   
         BE    PC110                                                            
*                                                                               
         LA    R4,1(R4)                                                         
         B     PC100                                                            
*                                                                               
PC110    DS    0H                  DELETE THIS RDETAIL RECORD                   
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA      PRINT HEADER AND X'06' ELEMENTS             
         MVC   P+9(4),RINVKINV                                                  
         GOTO1 HEXOUT,DMCB,RINVKNUM,P+15,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RINVKYR,P+18,1,=C'TOG'                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,KEY                                                           
         OI    27(R6),X'80'        MARK KEY FOR DELETION                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY,0                          
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(10),=C'DELETE KEY'                                             
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,INVIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,INVIO                                                         
         OI    29(R6),X'80'        MARK RECORD FOR DELETION                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,INVIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE HEADER RECORD                         
         DC    H'0'                                                             
*                                                                               
         MVC   P(10),=C'DELETE REC'                                             
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,ZCOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ZCOUNT                                                        
*                                                                               
         B     INVSEQ                                                           
*                                                                               
PC150    DS    0H                                                               
         B     PC20                ALREADY ON NEXT HEADER                       
         DROP  R6                                                               
*                                                                               
PC200    DS    0H                  DELETE RATE CARDS HERE                       
         LA    R3,RCARDDEL                                                      
PC210    DS    0H                                                               
         CLI   0(R3),X'FF'         FINISHED DELETING ALL RATE CARDS?            
         BE    PCX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         XC    RCOUNT,RCOUNT                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING RARTKEY,R6                                                       
         MVI   KEY,X'3E'                                                        
         MVC   RARTKCOD,0(R3)      MOVE IN RATE CARD NAME                       
*                                                                               
         CLI   RCARDAGY,X'FF'      ANY AGENCY FILTER?                           
         BE    *+10                                                             
         MVC   RARTKREP,RCARDAGY                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC220                                                            
         DC    H'0'                                                             
*                                                                               
PC220    DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(2),RARTKREP                                                    
         MVC   P+3(8),RARTKCOD                                                  
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,KEY                                                           
         OI    27(R6),X'80'        MARK KEY FOR DELETION                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY,0                          
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(15),=C'DELETE RATE KEY'                                        
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,INVIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,INVIO                                                         
         OI    29(R6),X'80'        MARK RECORD FOR DELETION                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,INVIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE HEADER RECORD                         
         DC    H'0'                                                             
*                                                                               
         MVC   P(15),=C'DELETE RATE REC'                                        
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,RCOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RCOUNT                                                        
*                                                                               
         LA    R3,8(R3)            BUMP TO NEXT RATE CARD TO DEL                
         B     PC210                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(28),=C'# OF RDETAIL DELETED=======>'                           
*                                                                               
         EDIT  ZCOUNT,(10,P+30),ZERO=NOBLANK,ALIGN=LEFT                         
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(28),=C'# OF RATE CARDS DELETED====>'                           
*                                                                               
         EDIT  RCOUNT,(10,P+30),ZERO=NOBLANK,ALIGN=LEFT                         
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
VHELLO   DS    A                                                                
VRECUP   DS    A                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
WRTOFILE DS    XL1                 WRITE TO FILE (Y OR N)                       
*                                                                               
MYFLAG   DS    XL1                                                              
PRNTHDR  EQU   X'01'               PRINT HEADER INFO                            
*                                                                               
ZCOUNT   DS    F                   # OF RDETAIL DELETED                         
RCOUNT   DS    F                   # OF RATE CARDS DELETED                      
HDRKEY   DS    CL27                                                             
ELEM     DS    XL50                                                             
ELCODE   DS    X                                                                
*                                                                               
EQUTABLE DS    XL21                TABLE OF EQUATES TO DELETE                   
*                                                                               
         DC    CL5'INVIO'                                                       
INVIO    DS    XL2000              HEADER RECORD TO CHANGE W/ NEW X'06'         
*                                                                               
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
       ++INCLUDE RCARDDELD                                                      
*                                                                               
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENARTE                                                      
*                                                                               
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160REREP2402D05/01/02'                                      
         END                                                                    

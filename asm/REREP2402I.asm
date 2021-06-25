*          DATA SET REREP2402I AT LEVEL 127 AS OF 05/01/02                      
*PHASE RE2402I,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'UPDATE RATE RECORDS W/ VALID STATIONS'                          
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
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
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
*                                                                               
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
         MVI   RINVKTYP,X'12'                                                   
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
         LA    R4,KEY                                                           
         CLI   RINVKTYP,X'12'                                                   
         BNE   PCX                                                              
*                                                                               
         OC    RINVKSTA,RINVKSTA   ANY STATION                                  
         BZ    PCSEQ                                                            
*                                                                               
         CLI   RINVKSRC,0          ONLY HEADERS                                 
         BNE   PCSEQ                                                            
*                                                                               
         MVC   HDRKEY,KEY                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IO1,DMWORK                
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO1                                                           
         LA    R6,IO1                                                           
*                                                                               
         MVI   ELCODE,X'06'        GET ALL RATE CARDS FOR THIS HDR              
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
         USING RIMAELEM,R6                                                      
*                                                                               
         LA    R3,TEMP                                                          
         XCEF  (R3),2000                                                        
         USING HDR06D,R3                                                        
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA                                                  
         MVC   P+10(4),RINVKINV                                                 
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+15,3,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
         B     *+12                                                             
*                                                                               
PC50     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PC100                                                            
*                                                                               
         MVC   HDRRCDE,RIMACDE     RATE CODE                                    
         MVC   HDREQU,RIMANUM      EQUATE NUMBER                                
*                                                                               
         LA    R3,HDR06DL(R3)                                                   
         B     PC50                                                             
         DROP  R6                                                               
*                                                                               
PC100    DS    0H                                                               
         LA    R3,TEMP                                                          
*                                                                               
PC110    DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    PC200                                                            
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(24),HDRKEY                                                   
         MVI   RINVKSRC,C'Z'       GET RDET REC'S FOR YEARS                     
         MVC   RINVKNUM,HDREQU     EQUATE NUMBER                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC120                                                            
         DC    H'0'                                                             
*                                                                               
PC1SEQ   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC120    DS    0H                                                               
         CLC   KEY(24),HDRKEY      STILL SAME INV REC?                          
         BNE   PC150                                                            
         CLI   RINVKSRC,C'Z'                                                    
         BNE   PC150                                                            
         CLC   RINVKNUM,HDREQU     SAME EQUATE #                                
         BNE   PC150                                                            
*                                                                               
         MVC   STAHLD,RINVKSTA     STATION                                      
         MVC   AGYHLD,RINVKREP     AGENCY                                       
*                                                                               
         MVC   RATCODE,HDRRCDE     RATE CODE TO CHANGE                          
         MVC   RATEYR,RINVKYR      YEAR TO UPDATE WITH                          
*                                                                               
         MVC   P(8),RATCODE                                                     
         GOTO1 HEXOUT,DMCB,RATEYR,P+10,1,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,GETRATE          GET RATE RECORD                              
         BNE   PC140               RATE RECORD DOESN'T EXIST                    
*                                                                               
         BAS   RE,GET03EL          GET THE 03 ELEMENT, IF EXISTS                
*                                                                               
         TM    MYFLAG,FOUND03      GOT IT?                                      
         BO    *+8                                                              
         BAS   RE,NEW03EL          BUILD NEW 03 ELEMENT                         
*                                                                               
         BAS   RE,UP03EL           UPDATE 03 ELEMENT                            
*                                                                               
         MVC   P(12),=C'CHANGED ===>'                                           
         MVC   P+14(2),KEY+10                                                   
         MVC   P+17(8),KEY+12                                                   
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,IO1,DMWORK                
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE RATE RECORD                           
         DC    H'0'                                                             
*                                                                               
         MVC   P(6),=C'PUTREC'                                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
PC140    DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC1SEQ                                                           
         DC    H'0'                                                             
*                                                                               
PC150    DS    0H                                                               
         LA    R3,HDR06DL(R3)                                                   
         B     PC110                                                            
*                                                                               
PC200    DS    0H                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PCSEQ                                                            
         DC    H'00'                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT======>'                                           
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         DROP  R3,R4                                                            
***************************************************************                 
*     GET RATE RECORD FOR ELEMENT LOOKUP                                        
***************************************************************                 
GETRATE  NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RARTREC,R4                                                       
*                                                                               
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGYHLD     AGENCY                                       
         MVC   RARTKCOD,RATCODE    RATE CODE                                    
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETR10                                                           
*                                                                               
         MVC   P(9),=C'NORATEREC'                                               
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),SAVEKEY                                                  
         LTR   RB,RB                                                            
         B     GETRATEX                                                         
*                                                                               
GETR10   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IO1,DMWORK                
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(7),=C'GOTRATE'                                                 
         GOTO1 REPORT                                                           
*                                                                               
         CR    RB,RB                                                            
*                                                                               
GETRATEX DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
***************************************************************                 
*     GET THE 03 ELEMENT IF EXISTS                                              
***************************************************************                 
GET03EL  NTR1                                                                   
         NI    MYFLAG,X'FF'-FOUND03                                             
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R6,IO1                                                           
         MVI   ELCODE,X'03'        GET 03 ELEMENT FOR THIS STA                  
         BAS   RE,GETEL                                                         
         BNE   GET03ELX                                                         
         B     *+12                                                             
         USING RASTELEM,R6                                                      
*                                                                               
GET0310  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GET03ELX                                                         
*                                                                               
         CLC   RASTSTA,STAHLD      SAME STATION?                                
         BNE   GET0310                                                          
         OI    MYFLAG,FOUND03                                                   
*                                                                               
         ZIC   R5,RASTLEN                                                       
         BCTR  R5,0                                                             
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         GOTO1 VRECUP,DMCB,(X'02',IO1),(R6),0,0                                 
*                                                                               
         MVC   P(5),=C'RECUP'                                                   
         GOTO1 REPORT                                                           
*                                                                               
GET03ELX DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
***************************************************************                 
*     BUILD NEW 03 ELEMENT                                                      
***************************************************************                 
NEW03EL  NTR1                                                                   
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R4,ELEM                                                          
         USING RASTELEM,R4                                                      
*                                                                               
         MVI   RASTCODE,X'03'                                                   
         MVI   RASTLEN,RASTLENQ    INITIAL LENGTH                               
*                                                                               
         MVC   RASTSTA,STAHLD      STATION                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,RASTCDTE)                                  
         XC    RASTCDTE+3(3),RASTCDTE+3                                         
*                                                                               
         MVC   P(7),=C'NEW03EL'                                                 
         GOTO1 REPORT                                                           
*                                                                               
NEW03ELX DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
***************************************************************                 
*     UPDATE 03 ELEMENT WITH NEW YEAR                                           
***************************************************************                 
UP03EL   NTR1                                                                   
         LA    R4,ELEM                                                          
         USING RASTELEM,R4                                                      
*                                                                               
         LA    R4,RASTYR                                                        
*                                                                               
UP0310   CLI   0(R4),0             IS YEAR IN ELEMENT?                          
         BE    UP0330              NO                                           
         CLC   0(1,R4),RATEYR      SAME YEAR?                                   
         BE    UP0340              YES                                          
*                                                                               
         LA    R4,1(R4)                                                         
         B     UP0310                                                           
*                                                                               
UP0330   DS    0H                                                               
         MVC   0(1,R4),RATEYR      MOVE IN YEAR                                 
*                                                                               
         LA    R4,ELEM                                                          
         ZIC   RF,RASTLEN                                                       
         LA    RF,1(RF)                                                         
         STC   RF,RASTLEN                                                       
*                                                                               
UP0340   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),IO1,ELEM,=C'ADD=CODE'                 
*                                                                               
         MVC   P(5),ELEM+2                                                      
         GOTO1 HEXOUT,DMCB,ELEM+13,P+7,5,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(6),=C'UP03EL'                                                  
         GOTO1 REPORT                                                           
*                                                                               
UP03ELX  DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
***************************************************************                 
*                                                                               
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
COUNT    DS    F                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
VHELLO   DS    F                                                                
VRECUP   DS    F                                                                
*                                                                               
MYFLAG   DS    XL1                                                              
FOUND03  EQU   X'01'               FOUND 03 EL FOR THIS STA                     
*                                                                               
         DC    CL8'SAVEKEYS'                                                    
HDRKEY   DS    XL27                                                             
SAVEKEY  DS    XL27                                                             
TEMPKEY  DS    XL27                                                             
*                                                                               
         DC    CL8'RDETAILS'                                                    
RATCODE  DS    CL8                                                              
RATEYR   DS    XL1                                                              
STAHLD   DS    CL5                                                              
AGYHLD   DS    CL2                                                              
*                                                                               
         DC    CL8'ELEM'                                                        
ELEM     DS    XL50                                                             
ELCODE   DS    XL1                                                              
*                                                                               
         DC    CL8'CODEYRS'                                                     
CODEYRS  DS    XL100                                                            
*                                                                               
         DC    CL8'IO1'                                                         
IO1      DS    XL2000                                                           
*                                                                               
         DC    CL8'TEMP'                                                        
TEMP     DS    XL2000                                                           
*                                                                               
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
*                                                                               
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENARTE                                                      
*                                                                               
HDR06D   DSECT                                                                  
HDRRCDE  DS    CL8                 RATE CODE                                    
HDREQU   DS    XL1                 EQUATE NUMBER                                
HDRSTAT  DS    XL1                 STATUS BIT                                   
HDRDONE  EQU   X'01'               THIS RATE CODE COMPLETE                      
HDR06DL  EQU   *-HDRRCDE                                                        
*                                                                               
RAT03D   DSECT                                                                  
RAT#YRS  DS    XL1                 # OF YEARS W/ THIS RATE CODE                 
RATRYRS  DS    0C                  YEARS                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127REREP2402I05/01/02'                                      
         END                                                                    

*          DATA SET REREP2402Q AT LEVEL 127 AS OF 05/01/02                      
*PHASE RE2402Q,*                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'SCAN FOR VALID STATIONS WITH NO RATES'                          
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
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BOLOCK AREA)                  
         USING TSARD,R2                                                         
         XC    0(TSARDL,R2),0(R2)                                               
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         OI    TSRECI,TSRVAR       SET VARIABLE LENGTH                          
         LA    R0,TSKLENQ          SET KEY LENGTH                               
         STC   R0,TSKEYL                                                        
         LA    R0,2000             SET MAX RECORD LENGTH                        
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF,(R2)                                                    
         DROP  R2                                                               
*                                                                               
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
         XC    BADCOUNT,BADCOUNT                                                
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
         BNE   PC1000                                                           
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
*&&DO                                                                           
         GOTO1 REPORT                                                           
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA                                                  
         MVC   P+10(4),RINVKINV                                                 
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+15,3,=C'TOG'                              
         GOTO1 REPORT                                                           
*&&                                                                             
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
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
*                                                                               
         XC    TLST(L'TSKEY+4),TLST                                             
         MVC   TSKREP,AGYHLD                                                    
         MVC   TSKCARD,RATCODE                                                  
*                                                                               
         LA    R0,TLST+2                                                        
         ST    R0,TSAREC                                                        
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    RDREC022            YES - ADD THE RECORD                         
*                                                                               
         CLC   TSKREP,AGYHLD       CORRECT REP?                                 
         BNE   RDREC022            NO - ADD THE RECORD                          
         CLC   TSKCARD,RATCODE     CORRECT REP?                                 
         BE    RDREC030            RECORD FOUND - UPDATE IT                     
*                                                                               
RDREC022 DS    0H                                                               
         XC    TLST(L'TSKEY+4),TLST                                             
         MVC   TSKREP,AGYHLD                                                    
         MVC   TSKCARD,RATCODE                                                  
         XC    TSREC(255),TSREC                                                 
*                                                                               
         MVC   TSRBLEN,=H'8'       ADD FIRST INFO BLOCK                         
         LA    RE,TSKEY+TSROVQ                                                  
         XC    0(8,RE),0(RE)                                                    
         MVC   0(4,RE),STAHLD                                                   
         MVC   4(4,RE),HDRKEY+(RINVKINV-RINVKEY)                                
*                                                                               
         LA    R0,TLST+2                                                        
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         SR    R0,R0                                                            
         ICM   R0,3,TSRBLEN                                                     
         AHI   R0,TSROVQ+2                                                      
         STCM  R0,3,TSLEN                                                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    RDREC040                                                         
         DC    H'0'                NEED TO EXPAND BUFFER                        
*                                                                               
RDREC030 DS    0H                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TSRBLEN                                                     
         LA    RF,8                                                             
         DR    R0,RF                                                            
         LR    R0,R1                                                            
         LA    RE,TSKEY+TSROVQ                                                  
*                                  SEE IF WE ALREADY HAVE THIS STATION          
RDREC032 DS    0H                                                               
         CLC   0(4,RE),STAHLD                                                   
         BE    RDREC042                                                         
         LA    RE,8(RE)                                                         
         BCT   R0,RDREC032                                                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TSRBLEN                                                     
         LA    R0,8(RF)                                                         
         STCM  R0,3,TSRBLEN                                                     
         LA    RE,TSKEY+TSROVQ(RF)                                              
*                                                                               
         XC    0(8,RE),0(RE)                                                    
         MVC   0(4,RE),STAHLD                                                   
         MVC   4(4,RE),HDRKEY+(RINVKINV-RINVKEY)                                
*                                                                               
         LA    R0,TLST+2                                                        
         ST    R0,TSAREC           UPDATE RECORD                                
         SR    R0,R0                                                            
         ICM   R0,3,TSRBLEN                                                     
         AHI   R0,TSROVQ+2                                                      
         STCM  R0,3,TSLEN                                                       
         MVI   TSOFFACT,TSAPUT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    RDREC040                                                         
         DC    H'0'                WHAT?                                        
         DROP  R2                                                               
*                                                                               
RDREC040 DS    0H                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*&&DO                                                                           
         MVC   P(02),AGYHLD                                                     
         MVC   P+10(8),RATCODE                                                  
         MVC   P+25(4),STAHLD                                                   
         MVC   P+30(4),HDRKEY+(RINVKINV-RINVKEY)                                
         GOTO1 REPORT                                                           
*&&                                                                             
RDREC042 DS    0H                                                               
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
         DROP  R3,R4                                                            
*                                                                               
PC1000   DS    0H                                                               
*                                                                               
*--------------------*                                                          
* PRINT TSAR RECORDS *                                                          
*--------------------*                                                          
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
*                                                                               
         LA    R0,TLST+2           GET TO THE START OF THE FILE                 
         ST    R0,TSAREC                                                        
         XC    TLST(L'TSKEY+4),TLST                                             
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
PRREC010 DS    0H                                                               
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    PCX                 YES - EXIT                                   
         DROP  R2                                                               
*                                                                               
         MVC   P(2),TSKREP                                                      
         MVC   P+10(8),TSKCARD                                                  
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TSRBLEN                                                     
         LA    RF,8                                                             
         DR    R0,RF                                                            
         LR    R2,R1                                                            
         LA    R3,TSKEY+TSROVQ                                                  
         LA    R4,P+30                                                          
*                                                                               
PRREC020 DS    0H                                                               
         MVC   0(4,R4),0(R3)                                                    
         MVC   5(4,R4),4(R3)                                                    
         LA    R3,8(R3)                                                         
         LA    R4,10(R4)                                                        
         BCT   R2,PRREC020                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   AGYHLD,TSKREP                                                    
         MVC   RATCODE,TSKCARD                                                  
         BAS   RE,GETRATE                                                       
         BE    PRREC028                                                         
*                                                                               
         MVC   P+10(10),=CL10'NOT FOUND'                                        
         B     PRREC040                                                         
*                                                                               
PRREC028 DS    0H                                                               
         LA    R6,IO1                                                           
         USING RARTREC,R6                                                       
         MVC   P+10(8),RARTKCOD                                                 
         LA    R4,P+30                                                          
         MVI   ELCODE,X'03'                                                     
         USING RASTELEM,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   PRREC040                                                         
*                                                                               
PRREC030 DS    0H                                                               
         MVC   0(4,R4),RASTSTA                                                  
         LA    R4,10(R4)                                                        
         BAS   RE,NEXTEL                                                        
         BE    PRREC030                                                         
         DROP  R6                                                               
*                                                                               
PRREC040 DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         B     PRREC010                                                         
         DROP  R2                                                               
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT======>'                                           
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
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
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXIT                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IO1,DMWORK                
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETRATEX DS    0H                                                               
         CR    RB,RB                                                            
         B     EXIT                                                             
***************************************************************                 
*                                                                               
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
COUNT    DS    F                                                                
BADCOUNT DS    F                                                                
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
         DS    0D                                                               
         DC    CL8'TSAR REC'                                                    
**************************                                                      
* TSAR RECORD DEFINITION *                                                      
**************************                                                      
TLST     EQU   *                                                                
TSNUM    DS    XL2                                                              
TSLEN    DS    XL2                                                              
TSKEY    EQU   *                                                                
TSKREP   DS    CL2                 REPCODE                                      
TSKCARD  DS    CL8                 RATE CARD NAME                               
TSKLENQ  EQU   *-TSKEY             KEY LENGTH                                   
*                                                                               
TSREC    EQU   *                                                                
TSRBLEN  DS    XL2                 LENGTH OF STATION BUFFER                     
TSROVQ   EQU   *-TSKEY             RECORD LENGTH                                
         DS    XL(1000-TSROVQ)     ROOM FOR THE REST OF THE RECORD              
*                                                                               
         DS    0D                                                               
         DC    CL8'IO1'                                                         
IO1      DS    XL2000                                                           
*                                                                               
         DS    0D                                                               
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
       ++INCLUDE REREPTSAR                                                      
       ++INCLUDE DDTSARD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127REREP2402Q05/01/02'                                      
         END                                                                    

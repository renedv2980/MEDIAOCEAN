*          DATA SET SPREPPA02  AT LEVEL 112 AS OF 08/29/00                      
*PHASE SPPA02A                                                                  
*INCLUDE TSCAN                                                                  
         TITLE 'SPREPPA02 - PROGRAM NAME ANALYSIS'                              
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1   RATING SOURCE- A=ARB,N=NSI                                     
*                                                                               
*                                                                               
SPPA02   CSECT                                                                  
         NMOD1 0,SPPA02,RR=R5                                                   
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         STM   R9,RB,SVRGS                                                      
         ST    R5,RELO                                                          
*                                                                               
         L     RC,=A(GLAREA)                                                    
         USING GLOBALD,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP050                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         B     EXIT                                                             
*                                                                               
DAYTIMS  DC    X'7F',AL2(0600,0545)   DAY AND TIME                              
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
* RUN FIRST                                                                     
*                                                                               
SP050    DS    0H                                                               
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(NAMTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,15                                                            
         LA    R4,15                                                            
         LH    R5,=Y(10000)                                                     
         STM   R0,R5,NTPARS                                                     
*                                                                               
         L     RF,=A(STNTAB)                                                    
         ST    RF,ASTNTAB                                                       
*                                                                               
         L     RF,=A(HLHOOK)                                                    
         ST    RF,HEADHOOK                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
SP100    DS    0H                                                               
*                                                                               
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   CPROF+3,C'0'                                                     
         CLI   QOPT1,C'A'          A=ARB, N=NSI                                 
         BNE   *+8                                                              
         MVI   CPROF+3,C'1'                                                     
         SPACE 2                                                                
*                                  BUILD STATION TABLE                          
*                                  -------------------                          
SP122    DS    0H                                                               
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,QOPT1      RATING SOURCE                                
         PACK  DUB,QBOOK1(2)       CONVERT BOOK                                 
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
*                                                                               
         XC    STNCNTR,STNCNTR                                                  
         MVC   ANXTSTN,ASTNTAB                                                  
         XC    DBSELRMK,DBSELRMK                                                
         MVI   DBFUNCT,DBGETSM                                                  
         GOTO1 DEMAND,DMCB,ADBLOCK,SVSTN                                        
         CLI   DBERROR,0                                                        
         BE    SP122C                                                           
         CLI   DBERROR,X'80'                                                    
         BE    SP122C                                                           
         B     SP122C              NO-OP                                        
         DC    H'0'                                                             
         SPACE 2                                                                
*                                  GET HH AUDIENCES                             
*                                  ----------------                             
SP122C   DS    0H                                                               
         L     R6,ASTNTAB                                                       
         USING STNTABD,R6                                                       
         L     R4,ADBLOCK                                                       
         MVI   DBFUNCT,DBGETDEM                                                 
*                                                                               
         MVI   DBLSTSBF,1          TIME PERIOD DEMOS                            
*                                                                               
SP123    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP124F                                                           
         MVC   DBSELSTA,STNID                                                   
         MVC   DBSELRMK,STNRMK                                                  
         XC    DBSELMK,DBSELMK                                                  
*                                                                               
SP123M   DS    0H                                                               
         MVC   DBSELDAY(5),DAYTIMS                                              
*                                                                               
         GOTO1 DEMAND,DMCB,ADBLOCK,SVPGN                                        
         CLI   DBERROR,0                                                        
         BE    SP123N                                                           
         CLI   DBERROR,X'80'                                                    
         BE    SP123N                                                           
         B     SP123N              NO-OP                                        
         DC    H'0'                                                             
*                                                                               
SP123N   DS   0H                                                                
*                                                                               
SP123R   DS    0H                                                               
         LA    R6,STNLEN(R6)                                                    
         B     SP123                                                            
*                                                                               
SP124F   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         L     R2,NTPARS+4                                                      
         L     R3,NTPARS+8                                                      
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE,0                                                           
*                                                                               
SP130    DS    0H                                                               
         CLC   BYTE,0(R2)          TEST NEW TYPE                                
         BE    SP131                                                            
         MVC   BYTE,0(R2)                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R2),C'M'          MOVIES                                       
         BNE   *+20                                                             
         MVC   P(5),=C'MOVIES'                                                  
         MVC   P2(5),=C'-----'                                                  
         B     SP130D                                                           
*                                                                               
         CLI   0(R2),C'N'          NEWS                                         
         BNE   *+20                                                             
         MVC   P(4),=C'NEWS'                                                    
         MVC   P2(4),=C'----'                                                   
         B     SP130D                                                           
*                                                                               
         CLI   0(R2),C'S'          SPORTS                                       
         BNE   *+20                                                             
         MVC   P(6),=C'SPORTS'                                                  
         MVC   P2(6),=C'------'                                                 
         B     SP130D                                                           
*                                                                               
         CLI   0(R2),C'O'          OTHER                                        
         BNE   *+20                                                             
         MVC   P(17),=C'OTHER KNOWN TYPES'                                      
         MVC   P2(17),=C'-----------------'                                     
         B     SP130D                                                           
*                                                                               
         CLI   0(R2),C'Z'          UNKNOWN                                      
         BNE   *+20                                                             
         MVC   P(13),=C'UNKNOWN TYPES'                                          
         MVC   P2(13),=C'-------------'                                         
         B     SP130D                                                           
*                                                                               
         DC    H'0'                                                             
*                                                                               
SP130D   DS    0H                                                               
         GOTO1 REPORT                                                           
         LA    R4,P                                                             
         LA    R5,8                                                             
*                                                                               
SP131    DS    0H                                                               
         MVC   0(14,R4),1(R2)                                                   
         LA    R4,16(R4)                                                        
         BCT   R5,SP132                                                         
*                                                                               
         GOTO1 REPORT                                                           
         LA    R4,P                                                             
         LA    R5,8                                                             
*                                                                               
SP132    DS    0H                                                               
         A     R2,NTPARS+12                                                     
         BCT   R3,SP130                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* SAVE STATION ID'S                                                             
         DS    0D                                                               
         USING *,RF                                                             
SVSTN    NTR1  BASE=SVRB                                                        
         LM    R9,RB,SVRGS                                                      
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R5,DBAREC                                                        
         USING BSKEY,R5                                                         
*                                                                               
         CLI   BSSTAT,C'0'         SKIP MARKETS                                 
         BNL   EXIT                                                             
         OC    BSKMKT,BSKMKT       SKIP SPILL                                   
         BNZ   EXIT                                                             
*                                                                               
         L     R6,ANXTSTN                                                       
         USING STNTABD,R6                                                       
         L     R1,STNCNTR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,STNCNTR                                                       
         XC    STNID(STNLEN),STNID CLEAR ENTRY                                  
         MVC   STNID,BSSTAT                                                     
         MVC   STNRMK,BSRMKT                                                    
         LA    R6,STNLEN(R6)                                                    
         MVI   0(R6),X'FF'                                                      
         ST    R6,ANXTSTN                                                       
         B     EXIT                                                             
         SPACE 2                                                                
* SAVE PROGRAM NAMES IN STATION TABLE                                           
         DS    0D                                                               
         USING *,RF                                                             
SVPGN    NTR1  BASE=SVRB                                                        
         LM    R9,RA,SVRGS                                                      
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
*                                                                               
         L     R5,DBAQUART         QTR HOUR ELEM                                
         USING QHELEM,R5                                                        
         CLI   1(R5),7             TEST NAME PRESENT                            
         BNH   EXIT                                                             
         MVC   WORK,SPACES                                                      
         ZIC   RF,1(R5)                                                         
         SH    RF,=H'7'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),QHPNAME                                                
         OC    WORK+1(15),SPACES                                                
*                                                                               
         LA    R5,PGMWDS                                                        
         MVI   SPTSEND,2           FIX BREAK BETWEEN SPORTS AND                 
         MVI   SPTSEND+1,C'M'      MOVIES (NOT USED IN OTHER PGMS)              
*                                                                               
SVP4     DS    0H                                                               
         CLI   0(R5),X'FF'         EOL                                          
         BE    SVP8                                                             
*                                                                               
         ZIC   R2,0(R5)                                                         
         GOTO1 =V(TSCAN),DMCB,WORK,17,((R2),2(R5)),((R2),2(R5))                 
         CLI   DMCB+16,0           TEST FOUND                                   
         BNE   SVP6                YES                                          
*                                                                               
         LA    R5,2(R2,R5)         NEXT ENTRY                                   
         B     SVP4                                                             
*                                                                               
SVP6     DS    0H                                                               
         MVC   WORK(1),1(R5)       SET TYPE BYTE                                
         B     SVP9                                                             
*                                                                               
SVP8     DS    0H                                                               
         MVI   WORK,C'Z'           SET TYPE NOT KNOWN                           
*                                                                               
SVP9     DS    0H                                                               
         GOTO1 BINSRCH,NTPARS,(1,WORK)                                          
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* HEADLINE HOOK                                                                 
         DS    0D                                                               
         USING *,RF                                                             
HLHOOK   NTR1  BASE=SVRB                                                        
         LM    R9,RA,SVRGS                                                      
         DROP  RF                                                               
         MVC   HEAD3(11),=C'*** ARB ***'                                        
         MVC   HEAD4(11),=C'-----------'                                        
         CLI   QOPT1,C'A'                                                       
         BE    *+10                                                             
         MVC   HEAD3+4(3),=C'ACN'                                               
*                                                                               
         MVC   WORK(4),QBOOK1                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(6,DUB)                                         
         MVC   HEAD6(5),=C'BOOK='                                               
         MVC   HEAD6+6(6),DUB                                                   
*                                                                               
         B     EXIT                                                             
*                                                                               
       ++INCLUDE SPMPPGMS                                                       
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
SPPA02   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
AMKTTAB  DS    A                                                                
ASTNTAB  DS    A                                                                
ANXTMKT  DS    A                                                                
ANXTSTN  DS    A                                                                
NTPARS   DS    6F                                                               
*                                                                               
DPGFILE  DC    CL8'SPPA05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
MCNTR    DS    F                                                                
STNCNTR  DS    F                                                                
*                                                                               
         SPACE 2                                                                
         DS    0F                                                               
SVRGS    DS    0XL12                                                            
SVR9     DS    F                                                                
SVRA     DS    F                                                                
SVRB     DS    F                                                                
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
         SPACE 2                                                                
*                                                                               
STNTABD  DSECT                                                                  
STNID    DS    CL5                                                              
STNRMK   DS    XL2                                                              
STNLEN   EQU   *-STNTABD                                                        
         SPACE 3                                                                
*                                                                               
         CSECT                                                                  
         DS    0D                                                               
NAMTAB   DS    0X                                                               
         ORG   *+(10000*16)                                                     
         DS    0D                                                               
STNTAB   DS    0X                                                               
         ORG   *+(2000*STNLEN)                                                  
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 2                                                                
       ++INCLUDE SPDRVWRKD                                                      
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
DBLKD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112SPREPPA02 08/29/00'                                      
         END                                                                    

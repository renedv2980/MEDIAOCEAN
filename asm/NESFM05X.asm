*          DATA SET NESFM05X   AT LEVEL 017 AS OF 05/01/02                      
*PHASE T31C05A                                                                  
         TITLE 'T31C05 NETWORK MARKET RECORD'                                   
T31C05   CSECT                                                                  
         NMOD1 0,T31C05                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING T31C05,RB,R7                                                     
         SPACE 3                                                                
         L     R6,AIO                                                           
         USING MKTKEY,R6                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
         CLI   MODE,XRECPUT        WRITE RECORDS                                
         BE    WR                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*----SETSTAT SETS VALUES FOR STATION FILE READS                                 
SETSTAT  DS    0H                                                               
         MVC   LKEY,=H'17'         SET VALUES FOR STAFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         MVI   NFILE,C'T'                                                       
         BR    RE                                                               
         EJECT                                                                  
DK       DS    0H                                                               
         MVI   MKTMED,C'N'                   * MEDIA                            
         FOUT  MKTMEDH                                                          
         SPACE                                                                  
         MVC   MKTMKT,MKTKMKT                                                   
         FOUT  MKTMKTH                                                          
         BAS   RE,SETSTAT                                                       
         B     EXIT                                                             
         EJECT                                                                  
DR       DS    0H                                                               
         BAS   RE,SETSTAT                                                       
         FOUT  MKTNMEH,MKTNAME,24                                               
         B     EXIT                                                             
         EJECT                                                                  
VK       DS    0H                                                               
         MVI   SVKEY,C'M'                                                       
         XC    SVKEY+2(4),SVKEY+2                                               
         MVC   SVKEY+6(2),AGENCY                                                
         MVI   SVKEY+8,C'0'                                                     
         MVC   SVKEY+9(8),SVKEY+8                                               
         SPACE                                                                  
         LA    R2,MKTMEDH          MEDIA CODE                                   
         GOTO1 VALIMED                                                          
         MVC   SVKEY+1(1),8(R2)                                                 
         SPACE                                                                  
         LA    R2,MKTMKTH          MKT CODE                                     
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VRERR                                                            
         B     VK200                                                            
         SPACE                                                                  
VK100    MVC   SVKEY+2(4),8(R2)                                                 
         SPACE                                                                  
VK200    MVC   KEY,SVKEY                                                        
         BAS   RE,SETFLINF                                                      
         LA    R2,MKTMEDH                                                       
         BAS   RE,SETSTAT                                                       
         B     EXIT                                                             
         EJECT                                                                  
VR       DS    0H                                                               
         BAS   RE,SETSTAT                                                       
         LA    R2,MKTNMEH                                                       
         GOTO1 ANY                                                              
         MVC   MKTNAME,8(R2)                                                    
*                                                                               
VREXT    BAS   RE,SETSTAT                                                       
         MVI   IOOPT,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS *                                                                
         SPACE                                                                  
LR       DS    0H                                                               
         SPACE                                                                  
         BAS   RE,SETSTAT                                                       
*                                                                               
         MVI   NLISTS,X'0A'        SET NUM OF LIST LINES                        
         MVC   AIO,AIO1                                                         
         OC    KEY(17),KEY                                                      
         BNZ   LR200                                                            
         BAS   RE,SETKEY                                                        
*                                                                               
LR100    BAS   RE,SETSTAT                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         MVC   KEY(17),MKTKEY                                                   
*                                                                               
         CLC   SVKEY(2),KEY                                                     
         BNE   LREXT                                                            
         SPACE                                                                  
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         MVC   MKCODE,MKTKMKT                                                   
         MVC   MKNAM,MKTNAME       MKT NAME                                     
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         GOTO1 LISTMON                                                          
LR200    ZIC   RE,KEY+(MKTKMKT+3-MKTKEY)                                        
         LA    RE,1(RE)            NEXT REP CODE THAN STORE AGENCY              
         STC   RE,KEY+(MKTKMKT+3-MKTKEY)                                        
         MVC   KEY+(MKTKAGY-MKTKEY)(2),AGENCY                                   
         MVC   SVKEY,KEY                                                        
         BAS   RE,SETFLINF                                                      
         B     LR100               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* PRINTING THE LINE                                                             
PR       DS    0H                                                               
         SPACE                                                                  
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,P+10                                                          
         USING PLINED,R4                                                        
         MVC   PRMED,MKTKMED       REP NAME                                     
         MVC   PRMKT,MKTKMKT                                                    
         MVC   PRMNAM,MKTNAME                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE                                                                  
PREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* WRITE RECORDS *                                                               
         SPACE                                                                  
WR       DS    0H                                                               
         SPACE                                                                  
         BAS   RE,SETSTAT                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',AIO,AIO,0                     
         MVI   IOOPT,C'N'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* SET KEY & SVKEY                                                               
SETKEY   MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),MKTMED                                                  
         MVI   KEY+8,C'0'                                                       
         MVC   KEY+9(8),KEY+8                                                   
         SPACE                                                                  
         CLI   MKTMKTH+5,0         REP CODE                                     
         BE    *+16                                                             
         MVC   KEY+2(4),MKTMKT                                                  
         MVC   KEY+6(2),AGENCY                                                  
         MVC   SVKEY(17),KEY                                                    
         BR    RE                                                               
         SPACE 2                                                                
* SET GENCON FILE STATUS INDICATORS                                             
SETFLINF MVC   LKEY,=H'17'         SET VALUES FOR STAFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
         MVI   NFILE,C'T'          STATION FILE                                 
         BR    RE                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,42,C'NETWORK MARKET RECORDS'                                  
         SSPEC H2,42,C'----------------------'                                  
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRMED(5),=C'MEDIA'                                               
         MVC   PRMED+132(5),=5C'-'                                              
         MVC   PRMKT(6),=C'MARKET'                                              
         MVC   PRMKT+132,=6C'-'                                                 
         MVC   PRMNAM(11),=C'MARKET NAME'                                       
         MVC   PRMNAM+132(24),=24C'-'                                           
         B     EXIT                                                             
         SPACE 2                                                                
PCDTAB   DS    0H                                                               
         DC    AL1(14),C'CANAD',AL1(7),C'ANA NAN'                               
         DC    AL1(11),C'CANAD',AL1(5),C'NNNNN'   ALSO ALLOW ZIPS               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
VRERR    GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL1                                                              
PRMED    DS    CL1                                                              
         DS    CL8                                                              
PRMKT    DS    CL4                                                              
         DS    CL8                                                              
PRMNAM   DS    CL24                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
MKCODE   DS    CL4                                                              
         DS    CL8                                                              
MKNAM    DS    CL24                                                             
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC8D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C10 WORK AREA  *******                  
WORKAREA DS    0CL100                                                           
SVKEY2   DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENCOM                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017NESFM05X  05/01/02'                                      
         END                                                                    

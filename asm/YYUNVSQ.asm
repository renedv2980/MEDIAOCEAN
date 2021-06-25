*          DATA SET YYUNVSQ    AT LEVEL 004 AS OF 08/13/03                      
*PHASE YYUNVSQA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DMDMGRL                                                                
       ++INCLUDE DMGREQUS                                                       
         TITLE 'YYUNVSQ - GET VOLSEQ AND DEVICE # FOR A DSN'                    
         PRINT NOGEN                                                            
YYUNVOLS CSECT                                                                  
         NBASE 0,YYUNVSQ,=A(R13CHAIN),R9                                        
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,GETDSNS            GET DSN FOR MST AND DIR                    
         CLI   ERROR,X'00'                                                      
         BNE   ERREXIT                                                          
*                                                                               
         USING TABLED,R8                                                        
         USING DTFPHD,R3                                                        
         LA    R3,DEBFILE                                                       
*                                                                               
         MVC   22(7,R3),=C'DIRFILE'  OVERRIDE FILE NAME W/DIR FILE              
         B     *+10                                                             
MST      MVC   22(7,R3),=C'MSTFILE'  OVERRIDE FILE NAME W/MST FILE              
*                                                                               
         XC    WORK(20),WORK         USE WORK SO PARAM LIST UNTOUCHED           
         GOTO1 =V(DATAMGR),WORK,DADDS,DAOPEN,,,(R3)                             
*                                                                               
         L     R4,DTFADCB          GET DCB ADDRESS                              
         L     R5,44(R4)           GET DEB ADDRESS                              
         ZICM  R7,16(R5),1         GET NUMBER OF EXTENTS                        
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         LA    R5,32(R5)           POINT TO FIRST EXTENT                        
         USING DEBDASD,R5          MAP THE EXTENT AREA                          
*                                                                               
NEXT     ZICM  R6,DEBUCBA,3        GET THE UCB ADDRESS                          
         USING UCBOB,R6                                                         
         PRINT GEN                                                              
         UCBDEVN DEVN=CHAN                                                      
         PRINT NOGEN                                                            
*                                                                               
         LH    R8,TABCNT                                                        
         MHI   R8,L'TABLE                                                       
         LA    R8,TABLE(R8)                                                     
         MVC   TABIDN,CHAN         INPUT DEVICE NUMBER                          
         MVC   TABIVS,UCBVOLI      INPUT VOL SER                                
*                                                                               
         MVC   VOL(2),=C'FC'                                                    
         MVC   VOL+2(4),CHAN                                                    
*                                                                               
         CLI   CHAN,C'0'           OLD DEVICE?                                  
         BNE   *+8                                                              
         MVI   VOL+2,C'3'          PLUG IN THE "3"                              
*                                                                               
         MVI   BYTE,X'00'                                                       
         MODESET MF=(E,SUPSTATE)                                                
         UCBLOOK VOLSER=VOL,UCBPTR=UCBPTR,DYNAMIC=YES,PIN,RANGE=ALL,   +        
               TEXT=TEXTLOOK,PTOKEN=PTOKLOOK                                    
         LTR   RF,RF                                                            
         BZ    PROB                                                             
         ST    RF,FULL                                                          
         OI    ERROR,X'80'                                                      
         MVI   BYTE,X'80'         THIS FC???? VOLSEQ IS NOT DEFINED             
PROB     MODESET MF=(E,PROBSTAT)                                                
*                                                                               
         CLI   BYTE,X'80'                                                       
         BNE   NOWARN                                                           
*                                                                               
         MVC   OPMSG1+18(6),VOL                                                 
         MVC   OPMSG1+36(6),TABIVS                                              
         CLC   22(7,R3),=C'MSTFILE'                                             
         BNE   *+20                                                             
         MVC   OPMSG1+63(10),MSTDSN                                             
         MVC   OPMSG2(44),MSTDSN                                                
         B     *+16                                                             
         MVC   OPMSG1+63(10),DIRDSN                                             
         MVC   OPMSG2(44),DIRDSN                                                
*                                                                               
         MVC   P(L'OPMSG1),OPMSG1                                               
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',(L'OPMSG1,OPMSG1)                     
         MVC   P(L'OPMSG2),OPMSG2                                               
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',(L'OPMSG2,OPMSG2)                     
         GOTO1 =V(PRINTER)                                                      
         B     NXTEXT                                                           
NOWARN   DS    0H                                                               
*                                                                               
         LH    RE,TABCNT                                                        
         AHI   RE,1                                                             
         STH   RE,TABCNT                                                        
*                                                                               
         L     R6,UCBPTR                                                        
         USING UCBOB,R6                                                         
         PRINT GEN                                                              
         UCBDEVN DEVN=CHAN                                                      
         PRINT NOGEN                                                            
*                                                                               
         MVC   TABFIL,22(R3)       FILE TYPE                                    
         MVC   TABODN,CHAN         OUTPUT DEVICE NUMBER                         
         MVC   TABOVS,UCBVOLI      OUTPUT VOL SER                               
         DROP  R6                                                               
*                                                                               
         MODESET MF=(E,SUPSTATE)                                                
         UCBPIN UNPIN,PTOKEN=PTOKLOOK                                           
         MODESET MF=(E,PROBSTAT)                                                
*                                                                               
NXTEXT   LA    R5,16(R5)           NEXT EXTENT                                  
         BCT   R7,NEXT                                                          
*                                                                               
         LA    R3,DEBFILE                                                       
         XC    WORK(20),WORK       USE WORK SO PARAM LIST UNTOUCHED             
         GOTO1 =V(DATAMGR),WORK,DADDS,DACLOSE,,,(R3)                            
*                                                                               
         CLC   22(7,R3),=C'MSTFILE'                                             
         BNE   MST                 DO THE MST FILE ALSO                         
*                                                                               
         BRAS  RE,PRTTAB                                                        
         BRAS  RE,DUPOFF     REMOVE DUP VOLSEQ WITHIN THE SAME DS               
         BRAS  RE,PRTTCD                                                        
         BRAS  RE,DUPOFF2    REMOVE ALL DUP VOLSEQ                              
         BRAS  RE,PRTTDS                                                        
*                                                                               
         CLI   ERROR,X'00'                                                      
         BNE   ERREXIT                                                          
*                                                                               
         XBASE                                                                  
*                                                                               
ERREXIT  LA    RF,8                                                             
         XBASE RC=(RF)                                                          
*                                                                               
*                                                                               
*                                                                               
*GET DSN FOR MST AND DIR                                                        
*                                                                               
GETDSNS  NTR1                                                                   
         LA    R5,JFCB                                                          
         USING INFMJFCB,R5                                                      
*                                                                               
         RDJFCB DIRFILE                                                         
         LTR   RF,RF                                                            
         BNZ   GDSNERR                                                          
         MVC   DIRDSN,JFCBDSNM                                                  
*                                                                               
         RDJFCB MSTFILE                                                         
         LTR   RF,RF                                                            
         BNZ   GDSNERR                                                          
         MVC   MSTDSN,JFCBDSNM                                                  
         DROP  R5                                                               
         B     GDSNX                                                            
*                                                                               
GDSNERR  OI    ERROR,X'40'         EITHER MST/DIR DD NOT FOUND                  
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',(L'OPMSG3,OPMSG3)                     
GDSNX    XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*PRINT THE TABLE                                                                
*                                                                               
PRTTAB   NTR1                                                                   
         GOTO1 =V(PRINTER)                                                      
         SR    R2,R2                                                            
         LA    R8,TABLE                                                         
TABNXT   CH    R2,TABCNT                                                        
         BNL   TABEND                                                           
*                                                                               
         MVC   P+1(L'TABIVS),TABIVS                                             
         MVC   P+8(L'TABIDN),TABIDN                                             
         MVC   P+13(L'TABOVS),TABOVS                                            
         MVC   P+20(L'TABODN),TABODN                                            
         MVC   P+31(L'TABFIL),TABFIL                                            
*                                                                               
PRTTPRT  EQU   *                                                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R2,1                                                             
         AHI   R8,L'TABLE                                                       
         B     TABNXT                                                           
TABEND   GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
*                                                                               
*PRINT TO THE DATASET VOLSDEV                                                   
*                                                                               
PRTTDS   NTR1                                                                   
         OPEN (VOLSDEV,OUTPUT)                                                  
         MVI   P,C'*'                                                           
         MVC   P+1(14),HEADDATE                                                 
         MVC   P+20(10),HEADTIME                                                
         PUT   VOLSDEV,P                                                        
         GOTO1 =V(PRINTER)                                                      
         SR    R2,R2                                                            
         LA    R8,TABLE                                                         
TDSNXT   CH    R2,TABCNT                                                        
         BNL   TDSEND                                                           
         CLI   0(R8),X'FF'                                                      
         BE    TDSKIP                                                           
*                                                                               
         MVC   P+1(L'TABIVS),TABIVS                                             
         MVC   P+8(L'TABIDN),TABIDN                                             
         MVC   P+13(L'TABOVS),TABOVS                                            
         MVC   P+20(L'TABODN),TABODN                                            
*                                                                               
PRTPUT   EQU   *                                                                
         PUT   VOLSDEV,P                                                        
         GOTO1 =V(PRINTER)                                                      
TDSKIP   AHI   R2,1                                                             
         AHI   R8,L'TABLE                                                       
         B     TDSNXT                                                           
TDSEND   GOTO1 =V(PRINTER)                                                      
         CLOSE VOLSDEV                                                          
         XIT1                                                                   
*                                                                               
*                                                                               
*PRINT USERCATALOG DEFINATION STATEMENT TO DATASET CATDEF                       
*                                                                               
PRTTCD   NTR1                                                                   
         OPEN (CATDEF,OUTPUT)                                                   
*                                                                               
         MVC   DSN,DIRDSN                                                       
         MVI   FILETYPE,C'D'                                                    
         MVC   CDLN1,CDLN1C                                                     
         MVC   CDLN2,CDLN2C                                                     
         MVC   CDLN3,CDLN3C                                                     
         B     PCD15                                                            
*                                                                               
PCD10    MVC   DSN,MSTDSN                                                       
         MVI   FILETYPE,C'M'                                                    
         MVC   CDLN1,CDLN1C                                                     
         MVC   CDLN2,CDLN2C                                                     
         MVC   CDLN3,CDLN3C                                                     
*                                                                               
PCD15    MVC   CDLN1+17(44),DSN                                                 
         LA    RE,CDLN1+17+44-1                                                 
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVC   1(3,RE),=C') -'                                                  
         MVC   P(L'CDLN1),CDLN1                                                 
         PUT   CATDEF,P                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R5,CDLN2+19                                                      
         LR    R6,R5                                                            
         SR    R2,R2                                                            
         LA    R8,TABLE                                                         
PCD20    CH    R2,TABCNT                                                        
         BNL   PCD60                                                            
         CLC   TABFIL,FILETYPE                                                  
         BNE   PCD50                                                            
         MVC   0(L'TABOVS,R5),TABOVS                                            
         AHI   R5,7                                                             
PCD50    AHI   R2,1                                                             
         AHI   R8,L'TABLE                                                       
         B     PCD20                                                            
PCD60    DS    0H                                                               
*                                                                               
         CR    R6,R5                                                            
         BNE   PCD80                                                            
*                                  IMPOSSIBLE, UNLESS MSTFILE=DIRFILE           
         OI    ERROR,X'20'                                                      
         MVC   OPMSG4+25(44),DSN                                                
         MVC   P(L'OPMSG4),OPMSG4                                               
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',(L'OPMSG4,OPMSG4)                     
         AHI   R5,1                                                             
*                                                                               
PCD80    BCTR  R5,0                                                             
         MVC   0(4,R5),=C')) -'                                                 
         MVC   P(L'CDLN2),CDLN2                                                 
         PUT   CATDEF,P                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(L'CDLN3),CDLN3                                                 
         PUT   CATDEF,P                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   FILETYPE,C'M'                                                    
         BNE   PCD10                                                            
*                                                                               
         CLOSE CATDEF                                                           
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
DSN      DS    CL44                                                             
FILETYPE DS    C                                                                
CDLN1    DC    CL80' DEF NVSAM (NAME('                                          
CDLN2    DC    CL80'    DEVT(3390) VOL('                                        
CDLN3    DC    CL80'    CATALOG(USERCAT.ICF.FACIL)'                             
CDLN1C   DC    CL80' DEF NVSAM (NAME('                                          
CDLN2C   DC    CL80'    DEVT(3390) VOL('                                        
CDLN3C   DC    CL80'    CATALOG(USERCAT.ICF.FACIL)'                             
*                                                                               
*ELIMITIATE THE DUPLICATE VOLSER/DEVICE WITHIN THE SAME DATASET                 
*                                                                               
DUPOFF   NTR1                                                                   
         LH    R2,TABCNT                                                        
         LTR   R2,R2                                                            
         BZ    DUPEND                                                           
         LA    R8,TABLE                                                         
*                                                                               
DUP10    LR    R3,R2                                                            
         BCT   R3,*+8              COUNTER FOR COMPARSION                       
         B     DUPEND                                                           
*                                                                               
         LA    R7,L'TABLE(R8)                                                   
DUP20    CLC   0(L'TABLE,R8),0(R7)                                              
         BNE   *+8                                                              
         MVI   0(R7),X'FF'                                                      
         AHI   R7,L'TABLE                                                       
         BCT   R3,DUP20                                                         
*                                                                               
         AHI   R8,L'TABLE                                                       
         BCT   R2,DUP10                                                         
DUPEND   XIT1                                                                   
*                                                                               
*                                                                               
*ELIMITIATE THE DUPLICATE VOLSER/DEVICE                                         
*                                                                               
DUPOFF2  NTR1                                                                   
         LH    R2,TABCNT                                                        
         LTR   R2,R2                                                            
         BZ    DUPEND                                                           
         LA    R8,TABLE                                                         
*                                                                               
DUP210   LR    R3,R2                                                            
         BCT   R3,*+8              COUNTER FOR COMPARSION                       
         B     DUP2END                                                          
*                                                                               
         LA    R7,L'TABLE(R8)                                                   
DUP220   CLC   1(L'TABLE-1,R8),1(R7)                                            
         BNE   *+8                                                              
         MVI   0(R7),X'FF'                                                      
         AHI   R7,L'TABLE                                                       
         BCT   R3,DUP220                                                        
*                                                                               
         AHI   R8,L'TABLE                                                       
         BCT   R2,DUP210                                                        
DUP2END  XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
UCBPTR   DS   F                                                                 
VOL      DS   CL6                            VOLSER SEARCHING FOR               
TEXTLOOK DC   CL58'PIN TEXT FOR UCBLOOK'     PIN TEXT                           
PTOKLOOK DS   CL8                            PIN TOKEN                          
*                                                                               
OPMSG1   DC   C'FLASH COPY VOLSER FCXXXX FOR VOLUME VVVVVV ASSOCIATED W+        
               ITH DSN=XXXXXXXXXX IS NOT ONLINE, RC=8'                          
OPMSG2   DC   CL80'DSN='                                                        
OPMSG3   DC   C'EITHER MSTFILE OR DIRFILE DD STATMENT IS NOT FOUND.'            
OPMSG4   DC   CL80'NO VOLSEQS FOUND FOR DSN='                                   
*                                                                               
SUPSTATE MODESET KEY=ZERO,MODE=SUP,MF=L                                         
PROBSTAT MODESET KEY=NZERO,MODE=PROB,MF=L                                       
*                                                                               
CHAN     DS    F                                                                
         DS    0D                                                               
         DC    C'*VOLSDEV'                                                      
VOLSDEV  DCB   DDNAME=VOLSDEV,DSORG=PS,MACRF=(PM)                               
*                                                                               
         DC    C'*CATDEF*'                                                      
CATDEF   DCB   DDNAME=CATDEF,DSORG=PS,MACRF=(PM)                                
*                                                                               
         DC    C'*DIRFIL*'                                                      
DIRFILE  DCB   DDNAME=DIRFILE,DSORG=PS,MACRF=(GM),EXLST=JFCBPTR                 
*                                                                               
         DC    C'*MSTFIL*'                                                      
MSTFILE  DCB   DDNAME=MSTFILE,DSORG=PS,MACRF=(GM),EXLST=JFCBPTR                 
*                                                                               
DEBFILE  DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
DADDS    DC    C'DADDS'                                                         
         EJECT                                                                  
*                                                                               
BYTE     DC    X'00'                                                            
ERROR    DC    X'00'                                                            
MSTDSN   DS    CL44                                                             
DIRDSN   DS    CL44                                                             
         DS    0H                                                               
         DC    CL8'*TABLE*'                                                     
TABCNT   DS    H                                                                
TABLE    DS    (TABNUM)CL(TABLEQ)                                               
TABNUM   EQU   40                                                               
*                                                                               
JFCB     DS    44F                                                              
JFCBPTR  DC    X'87'                                                            
         DC    AL3(JFCB)                                                        
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
TABLED   DSECT                                                                  
TABFIL   DS    C                   FILE TYPE: M-MSTFILE, D-DIRFILE              
TABOVS   DS    CL6                 OUTPUT VOLSER                                
TABODN   DS    CL4                 OUTPUT DEVICE#                               
TABIVS   DS    CL6                 INPUT VOLSER                                 
TABIDN   DS    CL4                 INPUT DEVICE#                                
TABLEQ   EQU   *-TABLED                                                         
*                                                                               
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         SPACE 2                                                                
       ++INCLUDE DDDPRINT                                                       
         SPACE 1                                                                
         PRINT   GEN                                                            
         IEFJFCBN   LIST=YES                                                    
         IEZDEB     LIST=NO                                                     
         DSECT                                                                  
         IEFUCBOB   DEVCLAS=DA                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004YYUNVSQ   08/13/03'                                      
         END                                                                    

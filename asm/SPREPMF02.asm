*          DATA SET SPREPMF02  AT LEVEL 023 AS OF 05/01/02                      
*PHASE SPMF02A                                                                  
         TITLE 'SPMF02 - COMMERCIAL USAGE REPORT'                               
SPMF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPMF**,RR=R2                                                 
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R2,RELO                                                          
         LA    R8,SPACEND                                                       
         USING MYD,R8                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         EJECT                                                                  
*              HANDLE MODE SETTINGS                                             
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MF2                                                              
         MVI   REQSW,C'Y'                                                       
         L     R3,=A(FILMBUFF)                                                  
         A     R3,RELO                                                          
         XC    4(4,R3),4(R3)                                                    
         MVC   BQSTARTP(4),=X'0000FFFF'                                         
         CLC   QSTART(12),SPACES                                                
         BE    XIT                                                              
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
         B     XIT                                                              
         SPACE 2                                                                
MF2      CLI   MODE,ESTFRST                                                     
         BNE   MF4                                                              
         CLI   REQSW,C'Y'                                                       
         BNE   XIT                                                              
         MVC   PAGE,=H'1'                                                       
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         BAS   RE,FILMBILD                                                      
         B     XIT                                                              
         SPACE 2                                                                
MF4      CLI   MODE,PROCBUY                                                     
         BNE   MF6                                                              
         BAS   RE,BUY                                                           
         B     XIT                                                              
         SPACE 2                                                                
MF6      CLI   MODE,MKTLAST                                                     
         BNE   XIT                                                              
         BAS   RE,MKTREP                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD FILM BUFFER                                     
         SPACE 3                                                                
FILMBILD NTR1                                                                   
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   CMLKID(2),=X'0A21'  FIND FILMS FOR THIS CLIENT                   
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   MYKEY,KEY                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         B     FB4                                                              
         SPACE 2                                                                
FB2      GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',KEY,KEY,0                     
         SPACE 2                                                                
FB4      CLC   KEY(5),MYKEY                                                     
         BNE   FB10                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,MYIO,        X        
               (0,DMWORK)                                                       
         LA    R2,MYIO                                                          
         MVI   ELCODE,X'10'        LOOK FOR A DATA ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   FB2                                                              
         USING CMLDTAEL,R2                                                      
         L     R3,=A(FILMBUFF)                                                  
         A     R3,RELO                                                          
         LM    R0,R1,0(R3)                                                      
         CR    R0,R1               ANY ROOM                                     
         BNE   *+6                                                              
         DC    H'0'                OOPS                                         
         LA    R1,1(R1)                                                         
         ST    R1,4(R3)            UPDATE COUNT                                 
         BCTR  R1,0                                                             
         MH    R1,=Y(FILMDL)                                                    
         LA    R3,8(R3,R1)         POSITION TO FILM ENTRY                       
         USING FILMD,R3                                                         
         XC    0(FILMDL,R3),0(R3)                                               
         MVC   FILMCAN,CMLKCML                                                  
         MVC   FILMPP,CMLKCML+2                                                 
         MVC   FILMNO,CMLSEQ+1                                                  
         B     FB2                                                              
         SPACE 2                                                                
FB10     L     R3,=A(FILMBUFF)     SORT BUFFER INTO CANISTER SEQUENCE           
         A     R3,RELO                                                          
         L     R4,4(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R5,FILMDL                                                        
         GOTO1 XSORT,DMCB,8(R3),(R4),(R5),8,0                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST BUYS INTO FILM BUFFER                            
         SPACE 3                                                                
BUY      NTR1                                                                   
         L     R2,ADBUY            BROWSE ROUND THE BUY                         
         USING BUYKEY,R2                                                        
         CLC   BUYMSTA(2),KEY+4    REJECT SPILL                                 
         BNE   XIT                                                              
         MVC   MYSTA,BUYMSTA+2                                                  
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     BUY4                                                             
         SPACE 2                                                                
BUY2     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
BUY4     CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'12'                                                      
         BE    BUY6                                                             
         CLI   0(R2),X'0B'         FOR POOL ELEMENTS                            
         BL    BUY2                                                             
         CLI   0(R2),X'0C'                                                      
         BH    BUY2                                                             
         USING REGELEM,R2                                                       
         MVI   SAVPROD,0                                                        
         TM    RSTATUS,X'80'       IGNORE MINUS                                 
         BO    BUY2                                                             
         TM    RSTATUS,X'40'          AND MINUSED SPOTS                         
         BO    BUY2                                                             
         CLC   RDATE,BQSTARTP      FILTER DATES                                 
         BL    BUY2                                                             
         CLC   RDATE,BQENDP                                                     
         BH    BUY2                                                             
         MVC   SAVPROD,RPPRD                                                    
         B     BUY2                                                             
         SPACE 2                                                                
BUY6     DS    0H                                                               
         USING FLMELEM,R2                                                       
         CLI   SAVPROD,0                                                        
         BE    BUY2                                                             
         L     R1,PRDBUFF          LOOK UP PRODUCT CODE IN BUFFER               
         ZIC   R0,SAVPROD                                                       
         BCTR  R0,0                                                             
         MH    R0,PRDBUFLN                                                      
         AR    R1,R0                                                            
         MVC   WORK+2(3),1(R1)                                                  
         MVC   WORK(2),WORK+2                                                   
         CLI   WORK+4,X'41'        COMPRESS IF NECESSARY                        
         BL    BUY8                                                             
         GOTO1 CLPACK,DMCB,WORK+2,WORK                                          
         SPACE 2                                                                
BUY8     MVC   WORK+2(2),FLMNUM                                                 
         L     R3,=A(FILMBUFF)                                                  
         A     R3,RELO                                                          
         L     R4,4(R3)                                                         
         LA    R3,8(R3)                                                         
         USING FILMD,R3                                                         
         SPACE 2                                                                
BUY10    CLC   WORK(4),FILMPP      FIND A MATCH ON PRODUCT/FILM                 
         BE    BUY12                                                            
         LA    R3,FILMDL(R3)                                                    
         BCT   R4,BUY10                                                         
         MVC   FILMCAN,=C'*UNKNOWN'                                             
         SPACE 2                                                                
BUY12    LA    R4,STALIST          LOOK FOR MATCH ON STATION                    
         LA    R5,FILMUSGE                                                      
         LA    R1,NSTA                                                          
         SPACE 2                                                                
BUY14    OC    0(3,R4),0(R4)                                                    
         BE    BUY16                                                            
         CLC   0(3,R4),MYSTA                                                    
         BE    BUY16                                                            
         LA    R4,3(R4)                                                         
         LA    R5,2(R5)                                                         
         BCT   R1,BUY14                                                         
         DC    H'0'                                                             
         SPACE 2                                                                
BUY16    MVC   0(3,R4),MYSTA                                                    
         LH    R1,0(R5)            ADD TO STATION USAGE                         
         LA    R1,1(R1)                                                         
         STH   R1,0(R5)                                                         
         LH    R1,FILMUMKT         AND TO MARKET                                
         LA    R1,1(R1)                                                         
         STH   R1,FILMUMKT                                                      
         B     BUY2                                                             
         EJECT                                                                  
*              ROUTINES TO PRINT A MARKET REPORT                                
         SPACE 3                                                                
MKTREP   NTR1                                                                   
         USING FILMD,R3                                                         
         MVI   FORCEHED,C'Y'                                                    
         XC    MKLINE,MKLINE                                                    
         L     R3,=A(FILMBUFF)                                                  
         A     R3,RELO                                                          
         L     R4,4(R3)                                                         
         LA    R3,8(R3)                                                         
         SPACE 3                                                                
MR2      OC    FILMUSGE,FILMUSGE                                                
         BZ    MR10                                                             
         MVC   P+1(8),FILMCAN                                                   
         CLI   FILMCAN,C'*'                                                     
         BE    MR8                                                              
         L     R2,PRDBUFF          LOOK FOR PRODUCT CODE                        
         LA    R1,220                                                           
         SPACE 2                                                                
MR4      CLC   FILMCAN+2(2),1(R2)                                               
         BE    MR6                                                              
         AH    R2,PRDBUFLN                                                      
         BCT   R1,MR4                                                           
         MVC   P+12(7),=C'UNKNOWN'                                              
         BAS   RE,PRDRD                                                         
         B     MR8                                                              
         SPACE 2                                                                
MR6      MVC   P+12(20),4(R2)                                                   
         SPACE 2                                                                
MR8      BAS   RE,FORMAT           FILL A LINE                                  
         BAS   RE,MKADD            ADD TO MARKET                                
         XC    FILMUSGE,FILMUSGE                                                
         MVI   SPACING,2           PRINT                                        
         GOTO1 REPORT                                                           
         SPACE 2                                                                
MR10     LA    R3,FILMDL(R3)                                                    
         BCT   R4,MR2                                                           
         OC    MKLINE,MKLINE                                                    
         BZ    MR12                                                             
         LA    R3,MKLINE                                                        
         MVC   P+12(17),=C'TOTALS FOR MARKET'                                   
         BAS   RE,FORMAT                                                        
         GOTO1 REPORT                                                           
         SPACE 2                                                                
MR12     XC    STALIST,STALIST                                                  
         B     XIT                                                              
         EJECT                                                                  
*              MARKET REPORT - SUBSIDIARY                                       
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         LA    R3,12(R3)                                                        
         LA    R4,P+34                                                          
         LA    R5,NSTA+1                                                        
         SPACE 2                                                                
FM2      EDIT  (2,(R3)),(5,0(R4))                                               
         LA    R3,2(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R5,FM2                                                           
         B     XIT                                                              
         SPACE 2                                                                
MKADD    NTR1                                                                   
         LA    R3,12(R3)                                                        
         LA    R4,MKLINE+12                                                     
         LA    R5,NSTA+1                                                        
         SPACE 2                                                                
MKADD2   LH    R1,0(R4)                                                         
         AH    R1,0(R3)                                                         
         STH   R1,0(R4)                                                         
         LA    R3,2(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R5,MKADD2                                                        
         B     XIT                                                              
         SPACE 2                                                                
PRDRD    NTR1                                                                   
         LA    R4,KEY                                                           
         USING PKEY,R4                                                          
         XC    KEY,KEY                                                          
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,FILMCAN+2                                                
         MVI   PKEYPRD+2,C' '                                                   
         MVC   MYKEY,KEY                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),MYKEY                                                    
         BNE   XIT                                                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,MYIO,        X        
               (0,DMWORK)                                                       
         LA    R4,MYIO                                                          
         MVC   P+12(20),PNAME                                                   
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
         DS    0H                                                               
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'REPT'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
         DROP  RF                                                               
         LA    R2,STALIST                                                       
         LA    R3,H9+35                                                         
         SPACE 2                                                                
HOOK2    OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         MVC   WORK(2),=X'0001'                                                 
         MVC   WORK+2(3),0(R2)                                                  
         GOTO1 MSUNPK,DMCB,WORK,WORK+10,0(R3)                                   
         MVI   4(R3),C' '                                                       
         MVC   132(4,R3),=C'----'                                               
         LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
         B     HOOK2                                                            
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R2),DATADISP,ELCODE                                             
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
NSTA     EQU   15                  MAX NUMBER OF STATIONS                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER FILM ENTRY                                        
         SPACE 2                                                                
FILMD    DSECT                                                                  
FILMCAN  DS    CL8                                                              
FILMPP   DS    CL2                                                              
FILMNO   DS    CL2                                                              
FILMUSGE DS    CL(2*(NSTA+1))    PLUS 1 FOR MARKET                              
FILMUMKT EQU   FILMUSGE+2*NSTA                                                  
FILMDL   EQU   *-FILMD                                                          
         SPACE 3                                                                
*              PROGRAM STORAGE                                                  
         SPACE 3                                                                
MYD      DSECT                                                                  
REQSW    DS    CL1                                                              
STALIST  DS    CL(3*(NSTA+1))                                                   
MYKEY    DS    CL32                                                             
MYIO     DS    2000C                                                            
         DS    0F                                                               
MKLINE   DS    CL(FILMDL)                                                       
SAVPROD  DS    CL1                                                              
ELCODE   DS    CL1                                                              
MYSTA    DS    CL3                                                              
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         PRINT ON                                                               
         SPACE 3                                                                
FILMBUFF CSECT                                                                  
         DC    F'2000'             MAX 2000 COMMERCIALS                         
         DC    F'0'                                                             
         DS    (2000*FILMDL)X                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPREPMF02 05/01/02'                                      
         END                                                                    

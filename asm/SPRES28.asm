*          DATA SET SPRES28    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T20F28,+0                                                                
         TITLE 'T20F28 - SIDLIST EDIT/REPORT'                                   
T20F28   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F28,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T20FFFD,RA                                                       
         ST    R2,RELO                                                          
         SPACE 1                                                                
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         CLI   MODE,VALKEY                                                      
         BE    EDIT                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         GOTO1 =A(SIDRPT),DMCB,(RC),RR=RELO                                     
XIT      XIT1                                                                   
         EJECT                                                                  
EDIT     DS    0H                                                               
         MVI   SPACOPT,1                                                        
         MVI   DPFILT,0                                                         
         MVI   PROGFILT,0                                                       
         MVI   PERFILT,0                                                        
         MVI   DAYFILT,0                                                        
         XC    TIMEFILT,TIMEFILT                                                
         XC    QSTAT,QSTAT                                                      
         XC    QMKT,QMKT                                                        
         SPACE 1                                                                
* NEED TO EDIT DUMMY SOURCE TO BUILD BAGYMD *                                   
         SPACE 1                                                                
         LA    R2,=X'0E00000000030000D5E2C9000000'                              
         GOTO1 VVALSRC                                                          
         EJECT                                                                  
* EDIT MARKET                                                                   
         SPACE 1                                                                
         LA    R2,RESMKTH          MARKET (OPTIONAL)                            
         CLI   5(R2),0                                                          
         BE    EDIT4                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT4                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BO    EDIT2                                                            
         MVI   ERROR,NOTNUM                                                     
         B     EDTERR                                                           
         SPACE 1                                                                
EDIT2    ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   QMKT,DUB                                                         
         EJECT                                                                  
* EDIT DAY TIME                                                                 
         SPACE 1                                                                
EDIT4    LA    R2,RESDAYH          DAY                                          
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT34                                                           
         GOTO1 VVALDAY                                                          
         MVC   DAYFILT,ACTUAL                                                   
         SPACE 1                                                                
EDIT34   LA    R2,RESTIMEH         TIME                                         
         CLI   5(R2),0                                                          
         BE    EDIT36                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT36                                                           
         GOTO1 VVALTIM                                                          
         MVC   TIMEFILT,WORK                                                    
         SPACE 1                                                                
         EJECT                                                                  
* EDIT PERIOD DAYPART AND PROGTYP                                               
         SPACE 1                                                                
EDIT36   LA    R2,RESPERH          PERIOD                                       
         CLI   5(R2),0                                                          
         BE    EDIT37                                                           
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    EDTERR                                                           
         MVC   PERFILT,8(R2)                                                    
         SPACE 1                                                                
EDIT37   LA    R2,RESDPTH          DAYPART                                      
         CLI   5(R2),0                                                          
         BE    EDIT38                                                           
         MVC   DPFILT,8(R2)                                                     
         SPACE 1                                                                
EDIT38   LA    R2,RESPRGH          PROGRAM TYPE                                 
         CLI   5(R2),0                                                          
         BE    EDITOPT                                                          
         MVC   PROGFILT,8(R2)                                                   
         EJECT                                                                  
* EDIT OPTIONS                                                                  
         SPACE 1                                                                
EDITOPT  LA    R2,RESOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    EDITFILT                                                         
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK),0                               
         ZIC   R0,4(R1)                                                         
         LA    R3,BLOCK                                                         
         LTR   R0,R0                                                            
         BNZ   EDIT46                                                           
         MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
         SPACE 1                                                                
EDIT46   CLC   12(2,R3),=C'S '     SPACING                                      
         BNE   EDIT48                                                           
         CLI   11(R3),0                                                         
         BE    BADOPT                                                           
         MVC   SPACOPT,11(R3)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDIT48   DS    0H                                                               
BADOPT   MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
         SPACE 1                                                                
EDITEND  LA    R3,42(R3)                                                        
         BCT   R0,EDIT46                                                        
         EJECT                                                                  
* EDIT FILTERS                                                                  
         SPACE 1                                                                
EDITFILT LA    R2,RESFILTH         FILTERS                                      
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LA    R3,BLOCK                                                         
         LTR   R0,R0                                                            
         BNZ   FILT6                                                            
         SPACE 1                                                                
BADFILT  MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
         SPACE 1                                                                
FILT6    CLC   12(4,R3),=C'PROGTYP'                                             
         BNE   FILT8                                                            
         MVC   PROGFILT,22(R3)                                                  
         B     FILTEND                                                          
         SPACE 1                                                                
FILT8    CLC   12(7,R3),=C'DAYPART'                                             
         BNE   FILT10                                                           
         MVC   DPFILT,22(R3)                                                    
         B     FILTEND                                                          
         SPACE 1                                                                
FILT10   DS    0H                                                               
         B     BADFILT                                                          
         SPACE 1                                                                
FILTEND  LA    R3,32(R3)                                                        
         BCT   R0,FILT6                                                         
         B     EXIT                                                             
         SPACE 1                                                                
VALCASH  NTR1                                                                   
         ZIC   R2,1(R3)                                                         
         GOTO1 CASHVAL,DMCB,22(R3),(R2)                                         
         CLI   DMCB,X'FF'                                                       
         BE    NO                                                               
         MVC   WORK(4),DMCB+4                                                   
         B     YES                                                              
         SPACE 1                                                                
EXIT     LA    R2,RESMKTH                                                       
         XIT1                                                                   
         SPACE 1                                                                
EDTERR   GOTO1 VGETERR                                                          
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         SPACE 1                                                                
NO       MVI   ERROR,INVALID                                                    
         LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         EJECT                                                                  
* PRINT THE REPORT *                                                            
         SPACE 1                                                                
SIDRPT   NMOD1 0,**SIDRPT                                                       
         L     RC,0(R1)            RESTORE GEND ADDRESS                         
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         XC    OLDVALS,OLDVALS                                                  
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         SPACE 1                                                                
         L     R7,=A(SIDBLOCK)     INTIALIZE SIDIOD                             
         A     R7,RELO                                                          
         USING SIDIOD,R7                                                        
         L     R1,=A(SIDIOAR)                                                   
         A     R1,RELO                                                          
         ST    R1,SIASID                                                        
         L     R1,=A(EBDIOAR)                                                   
         A     R1,RELO                                                          
         ST    R1,SIAEBD                                                        
         MVC   SIACOM,ACOMFACS                                                  
         MVC   SISELAGY,AGENCY                                                  
         MVC   SISELAM,BAGYMD                                                   
         MVC   SISELMKT,QMKT                                                    
         MVC   SISELSTA,QSTAT                                                   
         MVC   SIFLTPER,PERFILT                                                 
         MVC   SIFLTDPT,DPFILT                                                  
         MVC   SIFLTPRG,PROGFILT                                                
         MVC   SIFLTDAY,DAYFILT                                                 
         MVC   SIFLTTIM,TIMEFILT                                                
         SPACE 1                                                                
*                                  GET A(SIDIO) FROM CALLOV                     
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A34'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         SPACE 1                                                                
SILOOP   GOTO1 (RF),DMCB,(R7)                                                   
         CLI   SIMODE,SIONEREC                                                  
         BNE   RPTX                                                             
         BAS   RE,RPT2                                                          
         B     SILOOP                                                           
         EJECT                                                                  
* REPORT ON SID RECORD                                                          
         SPACE 1                                                                
RPT2     NTR1                                                                   
         CLC   OLDMKT,SIACTMKT     CONTROL BREAKS                               
         BNE   RPT4                                                             
         CLC   OLDSTA,SIACTSTA                                                  
         BNE   RPT4                                                             
         CLC   OLDPER,SIACTPER                                                  
         BNE   RPT6                                                             
         B     RPT12                                                            
         SPACE 1                                                                
RPT4     MVI   FORCEHED,C'Y'       MARKET/STATION                               
         B     RPT8                                                             
         SPACE 1                                                                
RPT6     OC    OLDPER,OLDPER       PERIOD                                       
         BZ    RPT8                                                             
         CLI   LINE,50                                                          
         BL    RPT7                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     RPT8                                                             
         SPACE 1                                                                
RPT7     L     R5,ABOX             CLOSE THE BOX                                
         USING BOXD,R5                                                          
         MVI   BOXINIT,0                                                        
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS(R1)                                                   
         MVI   0(R1),C'M'                                                       
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
RPT8     MVC   P+02(1),SIACTPER    BUYING PERIOD                                
         OI    P+02,X'F0'                                                       
         GOTO1 DATCON,DMCB,(3,SIDSTART),(8,P+04)                                
         GOTO1 DATCON,DMCB,(3,SIDEND),(8,P+13)                                  
         MVI   P+12,C'-'                                                        
         SPACE 1                                                                
RPT12    MVC   P+24(1),SIACTDPT    DAYPART                                      
         MVC   P+26(7),SIDDPTNM                                                 
         SPACE 1                                                                
RPT14    MVC   P+36(1),SIACTPRG    PROGRAM TYPE                                 
         MVC   P+38(7),SIDPRGNM                                                 
         SPACE 1                                                                
RPT16    GOTO1 UNDAY,DMCB,SIACTDAY,P+48      DAY                                
         SPACE 1                                                                
RPT18    MVC   P+59(11),SIDERTIM   TIME                                         
         LA    R2,SIACTEF1         EFFECTIVE DATE/COST                          
         LA    R3,P+73                                                          
         LA    R5,4                                                             
         SPACE 1                                                                
RPT20    OC    0(7,R2),0(R2)                                                    
         BZ    RPT22                                                            
         CLI   0(R2),X'40'                                                      
         BE    RPT22                                                            
         OC    0(3,R2),0(R2)       DATE                                         
         BZ    RPT21                                                            
         GOTO1 DATCON,DMCB,(3,0(R2)),(4,0(R3))                                  
         SPACE 1                                                                
*                                  AND COST                                     
RPT21    EDIT  (4,3(R2)),(10,9(R3)),2,ZERO=BLANK                                
         LA    R2,7(R2)                                                         
         LA    R3,132(R3)                                                       
         BCT   R5,RPT20                                                         
         SPACE 1                                                                
RPT22    BAS   RE,UP                                                            
         SPACE 1                                                                
RPT24    MVC   P+114(16),SIACTCOM  COMMENT/PROGRAM                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   OLDVALS,SIACTMKT                                                 
         B     RPTX                                                             
         EJECT                                                                  
* FORMAT THE UPGRADE EXPRESSION                                                 
         SPACE 1                                                                
UP       NTR1                                                                   
         MVC   BFORM(132),SPACES                                                
         LA    R2,BFORM                                                         
         CLI   SIUPWHER,C'S'                                                    
         BNE   UP2                                                              
         MVI   0(R2),C'*'          SHOW SID OVERRRIDE=*                         
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
UP2      MVC   0(16,R2),SIUPDATA   UPDATE DATA                                  
         LA    R2,17(R2)                                                        
         CLI   SIUPBOOK,0          FROM BOOK                                    
         BE    UP4                                                              
         MVC   0(3,R2),=C'BK='                                                  
         ZIC   R1,SIUPBOOK+1       YES - SO SHOW THAT AS WELL                   
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   3(3,R2),0(R1)                                                    
         EDIT  (1,SIUPBOOK),(2,6(R2))                                           
         LA    R2,10(R2)                                                        
         SPACE 1                                                                
UP4      L     R6,SIASID           NOW LOOK FOR DEMO OVERRIDES                  
         MVI   ELCODE,X'DE'                                                     
         LA    R6,24(R6)                                                        
         SPACE 1                                                                
UP6      BAS   RE,NEXTEL                                                        
         BNE   UP8                                                              
         BAS   RE,CNVRT                                                         
         LA    R2,14(R2)                                                        
         B     UP6                                                              
         SPACE 1                                                                
UP8      GOTO1 SQUASHER,DMCB,BFORM,132                                          
         GOTO1 CHOPPER,DMCB,(132,BFORM),(16,P+95),(C'P',4)                      
         B     RPTX                                                             
         EJECT                                                                  
* CONVERT DEMO=VALUE EXPRESSION                                                 
         SPACE 1                                                                
CNVRT    NTR1                                                                   
         USING DOVELEM,R6                                                       
         SPACE 1                                                                
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         XC    BLOCK(256),BLOCK                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'A'                                                    
         MVI   DBSELMED,C'T'                                                    
         DROP  RE                                                               
         SPACE 1                                                                
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         SPACE 1                                                                
         MVC   DMCB+4(4),=X'D9000AE0'         DEMOCON OVERLAY                   
         GOTO1 CALLOV,DMCB,0                                                    
         DROP  R4                                                               
         L     RF,DMCB                                                          
         LA    R3,DUB                                                           
         XC    0(3,R3),0(R3)                  DOVDEMO=2 BYTES/BUT               
         MVC   1(2,R3),DOVDEMO                DEMOCON NEEDS 3                   
         SPACE 1                                                                
         GOTO1 (RF),(R1),(0,(R3)),(2,(R2)),(C'S',BLOCK)     7 CHAR              
         SPACE 1                                                                
CNV05    CLI   0(R2),C' '          LOOK FOR END OF DEMNAME                      
         BNH   CNV10                                                            
         LA    R2,1(R2)                                                         
         B     CNV05                                                            
CNV10    MVI   0(R2),C'='          GOT IT/MOVE IN =                             
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         EDIT  (2,DOVVALUE),(6,(R2)),1,ALIGN=LEFT                               
         B     RPTX                                                             
         DROP  R6                                                               
         EJECT                                                                  
*              HEADINGS                                                         
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES           INITIALIZE BOXES                        
         LA    R4,BOXCOLS                                                       
         MVI   0(R4),C'L'                                                       
         MVI   22(R4),C'C'                                                      
         MVI   34(R4),C'C'                                                      
         MVI   46(R4),C'C'                                                      
         MVI   57(R4),C'C'                                                      
         MVI   71(R4),C'C'                                                      
         MVI   80(R4),C'C'                                                      
         MVI   93(R4),C'C'                                                      
         MVI   112(R4),C'C'                                                     
         MVI   131(R4),C'R'                                                     
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         MVC   H4(6),=C'MARKET'                                                 
         EDIT  (2,SIACTMKT),(4,H4+10),ALIGN=LEFT                                
         MVC   H4+16(24),SIDERMNM                                               
         MVC   H5(7),=C'STATION'                                                
         MVC   H5+10(4),SIDERSTA                                                
         MVC   H5+16(3),SIDERAFF                                                
         SPACE 1                                                                
HOOK2    MVC   H10+5(13),=C'BUYING PERIOD'                                      
         MVC   H10+25(28),=C'DAYPART     PROGRAM      DAY'                      
         MVC   H10+62(4),=C'TIME'                                               
         MVC   H10+73(6),=C'EFFECT'                                             
         MVC   H11+73(6),=C' DATE '                                             
         MVC   H10+85(4),=C'COST'                                               
         MVC   H10+95(14),=C'UPGRADE VALUES'                                    
         MVC   H10+114(10),=C'PROGRAM(S)  '                                     
         MVC   H11+38(4),=C'TYPE'                                               
         SPACE 1                                                                
RPTX     XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
RELO     DS    A                                                                
OLDVALS  DS    0CL14                                                            
OLDMKT   DS    CL2                                                              
OLDSTA   DS    CL3                                                              
OLDPER   DS    CL1                                                              
OLDSEQ   DS    CL1                                                              
OLDDPT   DS    CL1                                                              
OLDPRG   DS    CL1                                                              
OLDDAY   DS    CL1                                                              
OLDTIM   DS    CL4                                                              
BFORM    DS    CL132                                                            
QMKT     DS    CL2                                                              
QSTAT    DS    CL3                                                              
DTLIST   DS    CL30                                                             
SPACOPT  DS    CL1                                                              
SAVBMKT  DS    CL7                                                              
PROGFILT DS    CL1                                                              
PERFILT  DS    CL1                                                              
DPFILT   DS    CL1                                                              
DAYFILT  DS    CL1                                                              
TIMEFILT DS    CL4                                                              
         LTORG                                                                  
         EJECT                                                                  
HEDSPECS SPROG 0,1,2,3,4,5                                                      
         PSPEC H1,1,C'MEDIA     SPOT T.V'                                       
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H1,50,C'SID LISTING'                                             
         PSPEC H2,50,C'-----------'                                             
         PSPEC H1,97,AGYNAME                                                    
         PSPEC H2,97,AGYADD                                                     
         PSPEC H4,97,RUN                                                        
         PSPEC H5,97,REPORT                                                     
         PSPEC H5,114,PAGE                                                      
*                                                                               
BUFFC    DS    0D                                                               
         DC    C'*BLOCK**'                                                      
SIDBLOCK DC    300X'00'                                                         
         SPACE 1                                                                
         DC    C'**SID***'                                                      
SIDIOAR  DC    2000X'00'                                                        
         SPACE 1                                                                
         DC    C'**EBD***'                                                      
EBDIOAR  DC    2000X'00'                                                        
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD8D                                                       
         EJECT                                                                  
       ++INCLUDE SPSIDIOD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENESD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPRES28   05/01/02'                                      
         END                                                                    

*          DATA SET DENTIFIX   AT LEVEL 106 AS OF 03/25/05                      
*PHASE DENTIFIA DENTIFIX                                                        
*INCLUDE NETWEEK                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE KHDUMMY                                                                
DENTIFIX CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE 500,DENTIFIX,=V(REGSAVE),R6,R7                                   
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*************                                                                   
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(DENTIFIX),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         L     RF,=V(ADDAY)                                                     
         ST    RF,NADDAY                                                        
         L     RF,=V(GETDAY)                                                    
         ST    RF,NGETDAY                                                       
         L     RF,=V(NETWEEK)                                                   
         ST    RF,NNETWEEK                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,DMSYS,DMFLIST                            
         OPEN  (OUT,(OUTPUT))                                                   
GETNXTN  XC    NTIKEY(24),NTIKEY                                                
         LA    RF,NTIKEY                                                        
         USING PNKEY,RF                                                         
         MVC   NTIKEY(3),=C'NNN'                                                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'NTIDIR',NTIKEY,OUTKEY             
         B     CHKN                                                             
GETSEQN  GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'NTIDIR',NTIKEY,OUTKEY             
CHKN     CLC   OUTKEY(3),=C'NNN'                                                
         BNE   READJ                                                            
         LA    RF,OUTKEY                                                        
         MVC   NTIKEY,OUTKEY                                                    
         CLC   PNBOOK,=X'660F'                                                  
         BL    GETSEQN                                                          
         CLI   PNSTAT+4,C'T'                                                    
         BE    *+12                                                             
         CLI   PNSTAT+4,C'D'                                                    
         BNE   GETSEQN                                                          
* TRANSFER OLD N POINTERS SO WE CAN READ FILE                                   
*        MVC   OUTREC(30),OUTKEY                                                
*        MVC   OUTLEN(4),=X'001B0000'                                           
*        MVC   OUTREC+20(2),=X'FFFF'                                            
*        MVI   OUTREC+22,0                                                      
*        MVC   OUTREC+18(2),=X'0000'                                            
*        GOTO1 =V(HEXOUT),DMCB,OUTREC,P+24,30                                   
*        GOTO1 =V(PRINTER)                                                      
*        LA    RE,OUTLEN                                                        
*        PUT   OUT,(RE)                                                         
*                                                                               
* CREATE NEW N POINTERS FOR NEW PROGRAM NUMBERS                                 
         LA    R5,NTINUMT                                                       
GENN1    CLI   0(R5),X'FF'                                                      
         BE    GETSEQN                                                          
         LA    RF,OUTKEY                                                        
         CLC   PNPNUM(2),0(R5)                                                  
         BE    *+12                                                             
GENN2    LA    R5,9(R5)                                                         
         B     GENN1                                                            
*                                                                               
         MVC   P(18),OUTKEY                                                     
         GOTO1 =V(HEXOUT),DMCB,OUTKEY,P+24,30                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   OUTREC(30),OUTKEY                                                
         LA    RF,OUTREC                                                        
         MVC   PNPNUM(2),7(R5)                                                  
         MVC   OUTLEN(4),=X'001B0000'                                           
         MVC   OUTREC+20(2),=X'FFFF'                                            
         MVI   OUTREC+22,0                                                      
         MVC   OUTREC+18(2),=X'0000'                                            
         GOTO1 =V(HEXOUT),DMCB,OUTREC,P+24,30                                   
         GOTO1 =V(PRINTER)                                                      
         LA    RE,OUTLEN                                                        
         PUT   OUT,(RE)                                                         
         B     GENN2                                                            
         DROP  RF                                                               
*                                                                               
READJ    LA    R5,NTINUMT                                                       
GETNXTJ  XC    NTIKEY(24),NTIKEY                                                
         LA    RF,NTIKEY                                                        
         USING PJKEY,RF                                                         
         MVC   NTIKEY(8),=C'JNNPPPPN'                                           
         MVC   PJEXTNUM,2(R5)                                                   
         MVC   INTPNTI,PJEXTNUM                                                 
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'NTIDIR',NTIKEY,OUTKEY             
         LA    RF,OUTKEY                                                        
         MVC   PJINTNUM(2),7(R5)                                                
         MVC   OUTREC(30),OUTKEY                                                
         MVC   OUTLEN(4),=X'001B0000'                                           
         MVC   OUTREC+20(2),=X'FFFF'                                            
         MVI   OUTREC+22,0                                                      
         MVC   OUTREC+18(2),=X'0000'                                            
         LA    RE,OUTLEN                                                        
         PUT   OUT,(RE)                                                         
         LA    RF,OUTKEY                                                        
         CLC   PJEXTNUM,INTPNTI                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         EDIT  (B2,PJINTNUM),(5,P+9)                                            
         GOTO1 =V(HEXOUT),DMCB,OUTKEY,P+19,30                                   
         GOTO1 =V(PRINTER)                                                      
         LA    R5,9(R5)                                                         
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    *+8                                                              
         B     GETNXTJ                                                          
*                                                                               
         LA    R5,NTINUMT                                                       
         XC    NTIKEY(24),NTIKEY                                                
         LA    RF,NTIKEY                                                        
         USING PMKEY,RF                                                         
         MVC   NTIKEY(3),=C'QNN'                                                
         MVC   PMBOOK,=X'660F'                                                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'NTIDIR',NTIKEY,OUTDIR             
         B     GETDDSH                                                          
GETDDS1  GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'NTIDIR',NTIKEY,OUTDIR             
GETDDSH  CLC   OUTDIR(3),=C'QNN'   FIX PROGRAM RECORDS                          
         BNE   EXIT                                                             
         LA    RE,OUTDIR                                                        
         MVC   KEY,0(RE)                                                        
         MVC   SVDA,PMNDXDA-PMKEY(RE)                                           
         MVC   SVSTATUS,PMKSTAT-PMKEY(RE)                                       
GETDDS   DS    0C                                                               
         XC    NTIKEY(24),NTIKEY                                                
         MVC   NTIKEY(18),OUTDIR                                                
         LA    RE,NTIKEY                                                        
         MVC   PMRSTAT-PMKEY(1,RE),SVSTATUS                                     
         MVC   PMPNUM-PMKEY(L'PMPNUM,RE),=X'0001'                               
         MVC   OUTREC(24),NTIKEY                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'NTIFIL',SVDA,OUTREC               
         B     GETDDS3A                                                         
GETDDS3  GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'NTIFIL',SVDA,OUTREC               
*        MVC   P(120),OUTREC                                                    
*        GOTO1 =V(PRINTER)                                                      
*        XC    P(132),P                                                         
*        GOTO1 =V(HEXOUT),DMCB,OUTREC,P+19,46                                   
*        GOTO1 =V(PRINTER)                                                      
GETDDS3A LA    RF,OUTREC                                                        
         SR    R1,R1               SET OUT LEN FOR TAPE                         
         ICM   R1,3,PMRLEN                                                      
         AHI   R1,4                                                             
         XC    OUTLEN(4),OUTLEN                                                 
         STCM  R1,3,OUTLEN                                                      
         CLC   OUTREC(3),=C'QNN'   FIX PROGRAM RECORDS                          
         BNE   GETDDS1                                                          
         CLI   PMBTYP,C'U'                                                      
         BE    GETDDS1                                                          
         CLI   PMSTAT+4,C'T'       FOR NETWORK POCKETPIECE                      
         BE    *+12                                                             
         CLI   PMSTAT+4,C'D'       AND DAILIES                                  
         BNE   GETDDS1                                                          
         LA    R5,NTINUMT                                                       
CHKDDS   CLC   PMPNUM,0(R5)                                                     
         BE    GETDDS2                                                          
         LA    R5,9(R5)                                                         
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    PUTDDS                                                           
         B     CHKDDS                                                           
GETDDS2  MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         EDIT  (B2,PMPNUM),(5,P+9)                                              
         GOTO1 =V(HEXOUT),DMCB,OUTREC,P+28,50                                   
         LA    RF,OUTREC                                                        
         LA    RE,PMDATA                                                        
         USING PHTELEM,RE                                                       
GETEL    CLI   0(RE),X'10'                                                      
         BE    HAVEL                                                            
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     GETEL                                                            
HAVEL    DS    0C                                                               
         LA    R5,NTINUMT                                                       
CHKNTI   CLC   PHTNTI(5),2(R5)                                                  
         BE    CHKNTI2                                                          
         LA    R5,9(R5)                                                         
         CLC   0(2,R5),=X'FFFF'                                                 
         BNE   CHKNTI                                                           
         MVC   P(2),=C'OK'                                                      
         B     PUTDDS                                                           
CHKNTI2  MVC   DUB(5),PHTNTI                                                    
         MVC   PHTDDS(2),7(R5)                                                  
         LA    RF,OUTREC                                                        
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    *+16                                                             
         MVC   PMPNUM,7(R5)                                                     
         MVC   PHTDDS(2),7(R5)                                                  
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(2,RE),7(R5)                                                    
         GOTO1 =V(HEXOUT),DMCB,DUB,P+15,5                                       
         GOTO1 =V(HEXOUT),DMCB,OUTREC,P+28,50                                   
CHKNTI3  GOTO1 =V(PRINTER)                                                      
         LA    RF,OUTREC                                                        
         USING PMKEY,RF                                                         
         MVC   PMPNUM,7(R5)                                                     
PUTDDS   LA    RE,OUTLEN                                                        
         PUT   OUT,(RE)                                                         
         B     GETDDS3                                                          
*                                                                               
*                                                                               
EXIT     CLOSE (OUT)                                                            
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*                 CURR#      NTILONG#       NEW#                                
NTINUMT  DC    AL2(10769),X'0000127728',AL2(11100)                              
         DC    AL2(10769),X'0000127785',AL2(11101)                              
         DC    AL2(10769),X'0000127806',AL2(11102)                              
         DC    AL2(10769),X'0000127828',AL2(11103)                              
         DC    AL2(10769),X'0000127847',AL2(11104)                              
         DC    AL2(10770),X'0000127729',AL2(11105)                              
         DC    AL2(10770),X'0000127827',AL2(11106)                              
         DC    AL2(10770),X'0000127862',AL2(11107)                              
         DC    AL2(10771),X'0000127808',AL2(11108)                              
         DC    AL2(10771),X'0000127890',AL2(11109)                              
         DC    AL2(10772),X'0000127829',AL2(11110)                              
         DC    AL2(10772),X'0000127841',AL2(11111)                              
         DC    AL2(10772),X'0000127842',AL2(11112)                              
         DC    AL2(10772),X'0000127844',AL2(11113)                              
         DC    AL2(10772),X'0000127911',AL2(11114)                              
         DC    AL2(10773),X'0000127843',AL2(11115)                              
         DC    AL2(10773),X'0000127917',AL2(11116)                              
         DC    AL2(10774),X'0000127872',AL2(11117)                              
         DC    AL2(10774),X'0000127967',AL2(11118)                              
         DC    AL2(10775),X'0000124363',AL2(11119)                              
         DC    AL2(10775),X'0000127819',AL2(11120)                              
         DC    AL2(10775),X'0000127912',AL2(11121)                              
         DC    AL2(10775),X'0000127915',AL2(11122)                              
         DC    AL2(10775),X'0000127918',AL2(11123)                              
         DC    AL2(10775),X'0000127968',AL2(11124)                              
         DC    AL2(10775),X'0000127991',AL2(11125)                              
         DC    AL2(10775),X'0000128141',AL2(11126)                              
         DC    AL2(10776),X'0000127820',AL2(11127)                              
         DC    AL2(10776),X'0000127913',AL2(11128)                              
         DC    AL2(10776),X'0000128083',AL2(11129)                              
         DC    AL2(10776),X'0000128121',AL2(11130)                              
         DC    AL2(10776),X'0000128150',AL2(11131)                              
         DC    AL2(10776),X'0000128205',AL2(11132)                              
         DC    AL2(10778),X'0000127616',AL2(11133)                              
         DC    AL2(10778),X'0000128078',AL2(11134)                              
         DC    AL2(10778),X'0000128079',AL2(11135)                              
         DC    AL2(10778),X'0000128196',AL2(11136)                              
         DC    AL2(10778),X'0000128206',AL2(11137)                              
         DC    AL2(10779),X'0000127907',AL2(11138)                              
         DC    AL2(10779),X'0000127904',AL2(11139)                              
         DC    AL2(10779),X'0000128180',AL2(11140)                              
         DC    AL2(10779),X'0000128197',AL2(11141)                              
         DC    AL2(10780),X'0000127901',AL2(11142)                              
         DC    AL2(10780),X'0000128204',AL2(11143)                              
         DC    AL2(10781),X'0000128143',AL2(11144)                              
         DC    AL2(10781),X'0000128151',AL2(11145)                              
         DC    AL2(10782),X'0000128152',AL2(11146)                              
         DC    AL2(10782),X'0000128291',AL2(11147)                              
         DC    AL2(10783),X'0000128155',AL2(11148)                              
         DC    X'FFFF'                                                          
NUMREC   DC    A(0)                                                             
SVNTDAY  DC    X'00'                                                            
SVCOST   DS    F                                                                
DAYPOSTN DS    X                                                                
NTIKEY   DC    24X'00'                                                          
OUTKEY   DC    24X'00'                                                          
OUTDIR   DC    24X'00'                                                          
PACK16   DS    XL16                                                             
INTPNTI  DS    XL5                                                              
KEY      DS    XL23                                                             
KEYSAVE  DS    XL23                                                             
DMRDHI   DC    C'DMRDHI'                                                        
DMRSEQ   DC    C'DMRSEQ'                                                        
IOAREA   DS    XL1024                                                           
SVSTATUS DS    XL1                                                              
SVDA     DS    XL4                                                              
MERGEDBK DS    XL(L'PMBOOK)                                                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=241'                                   
SORTCARD DC    CL80'SORT FIELDS=(5,41,A,49,16,A,46,3,A),FORMAT=BI,WORK=+        
               1'                                                               
*                                                                               
UTL      DS    0D                                                               
         DC    4X'00',X'02'        UTL FOR SPOT1                                
* DATA MANAGER LITERALS                                                         
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYS    DC    CL8'SPOT'                                                        
DMFLIST  DC    C'NDEMDIRNNDEMFILNNDEMDIRANDEMFILA'                              
         DC    C'NDEMDIRONDEMFILO'                                              
         DC    C'NDEMFILP'                                                      
         DC    C'NDEMFILQ'                                                      
         DC    C'NDEMFILU'                                                      
         DC    C'NDEMFILV'                                                      
         DC    C'NDEMDIRRNDEMFILR'                                              
         DC    C'NNTIDIR NNTIFIL '                                              
         DC    C'NNTIFILN'                                                      
         DC    C'NNTIFILO'                                                      
         DC    C'NNTIFILP'                                                      
         DC    C'NPAVDIR NPAVFIL NCTFILE X'                                     
         EJECT                                                                  
*                                                                               
LNBUFF   EQU   L'PNAME+L'HN+L'GS+L'ONUMS                                        
LNBUF2   EQU   CLEARE-CLEARS                                                    
         DC    CL8'**WORK**'                                                    
FIRSTPUT DC    X'01'                                                            
TVFBOOK  DS    XL6                 YYMMDD                                       
FRSTBOOK DS    XL2                 FIRST WEEK OF THE MONTH                      
SAVER8   DC    A(0)                                                             
ARDEF    DC    A(0)                                                             
NOMATCH  DC    X'0'                                                             
DDSNTI   DS    CL2                 DDS PROGRAM NUMBER                           
GETSW    DS    C                                                                
MATCH    DS    C                                                                
ALPHA    DS    C                                                                
NUMSW    DS    C                                                                
RFIELD   DS    C                                                                
HX       DS    C                                                                
HA       DS    C                                                                
HN       DS    C                                                                
GS       DS    C                                                                
MULTISW  DS    C                                                                
DPT3     DS    CL3                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
PNUM     DS    H                                                                
ADPTCUME DS    F                                                                
NADDAY   DS    F                                                                
NGETDAY  DS    F                                                                
NNETWEEK DS    F                                                                
WORK     DS    CL20                                                             
WORK2    DS    CL80                                                             
PNAME    DS    CL50                                                             
         DC    CL8'*CLEARS*'                                                    
CLEARS   DS    0C                                                               
TVQNET   DS    CL4                                                              
TVQNTI   DS    XL7                                                              
TVQNAME  DS    CL25                                                             
TVQBOOK  DS    CL6                  BOOK YYMMDD                                 
TVQMNTH  DS    CL2                                                              
TVQPAIR  DS    CL6                  PROGRAM DATE YYMMDD                         
TVQTIME  DS    CL8                  PROGRAM TIME HH:MM:SS                       
TVQCOST  DS    CL9                                                              
TVQLNTH  DS    CL4                                                              
NTINET   DS    CL6                                                              
NTINTI   DS    XL5                                                              
NTIFILT  DS    XL2                                                              
NTILP    DS    CL24                                                             
CLEARE   DS    0C                                                               
TBSTART  DS    0C                                                               
TBPNAME  DS    CL50                                                             
TBNET    DS    CL1                                                              
TBDPT    DS    CL1                                                              
TBIQ     DS    XL50                                                             
TBTVQ    DS    XL50                                                             
TBFT     DS    XL50                                                             
TBEND    DS    0C                                                               
P2       DS    CL132                                                            
         DS    200C                                                             
         DC    CL8'**IREC**'                                                    
INREC    DS    CL800                                                            
         DC    CL8'**ONUM**'                                                    
ONUMS    DS    XL150                                                            
         DC    CL8'**DNUM**'                                                    
DNUMS    DS    XL150                                                            
DPIQ     EQU   DNUMS                                                            
DPTVQ    EQU   DNUMS+50                                                         
DPFT     EQU   DNUMS+100                                                        
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02000,                                            X        
               BLKSIZE=08200,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
         DC    CL8'*DMCB***'                                                    
DMCB     DS    6F                                                               
*                                                                               
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
OUTLEN   DS    XL4                                                              
OUTREC   DS    XL2000                                                           
         DC    CL8'*OUTREC*'                                                    
OUTRSV   DS    XL4                                                              
OUTDSV   DS    XL2000                                                           
         DC    CL8'**TOTS**'                                                    
*TABLE OF MERGED WEEKS                                                          
WKMERGET DS    5XL(L'PMBOOK)                                                    
         ORG   WKMERGET                                                         
         DC    X'FF'                                                            
         ORG                                                                    
         EJECT                                                                  
                                                                                
         DS    0L                                                               
         DC CL16'**DEMCNVSSB**'                                                 
                                                                                
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'00' NO RECOVERY                                  
         DC    5XL4'00000000'                                                   
         DC    A(FACINDX)                                                       
         DC    A(FACBUFF)                                                       
         ORG                                                                    
         EJECT                                                                  
FACINDX  CSECT                                                                  
         DC    16X'00'             FACWRK INDEX AREA                            
*                                                                               
FACBUFF  DC    (6144)X'00'         FACWRK 6K BUFFER                             
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106DENTIFIX  03/25/05'                                      
         END                                                                    

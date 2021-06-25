*          DATA SET DDPANMRRPT AT LEVEL 056 AS OF 08/09/06                      
*PHASE PANMRRPA                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE PANIC                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'ANALYZE PANAPT APTDUMP FILE DATA'                               
PANMRRPT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PANMRRPT,=V(REGSAVE),RR=R7                                     
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(PANMRRPT),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
         L     R6,=A(REC+4)                                                     
         L     R8,=A(REC+4)                                                     
         USING APAMMDES,R6                                                      
         USING MMMBRD,R8                                                        
         MVC   TITLE(40),=CL40'PANAPT - MEMBERS MOVED TO PRODUCTION'            
         GOTO1 =V(PRINTER)                                                      
         XC    EXDESCNT,EXDESCNT                                                
*                                                                               
         MVC   PDATE(8),=CL8'  DATE'                                            
         MVC   PTIME(8),=CL8'  TIME'                                            
         MVC   PUSERID(7),=CL7'CREATED'                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   PMR#(3),=CL3'MR#'                                                
         MVC   PDATE(8),=CL8'LAST UPD'                                          
         MVC   PTIME(8),=CL8'LAST UPD'                                          
*        MVC   PSTAT(4),=CL4'STAT'                                              
         MVC   PUSERID(7),=CL7'  BY'                                            
         MVC   PDES(11),=CL11'DESCRIPTION'                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   PMR#(3),=CL11'-----------'                                       
         MVC   PDATE(8),=CL11'-----------'                                      
         MVC   PTIME(8),=CL11'-----------'                                      
*        MVC   PSTAT(4),=CL11'-----------'                                      
         MVC   PUSERID(7),=CL11'-----------'                                    
         MVC   PDES(11),=CL11'-----------'                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         OPEN  FILE                                                             
*                                                                               
NEXT     L     R2,=A(REC)                                                       
         GET   FILE,(R2)                                                        
         CLC   DESTYPE,=C'01'                                                   
         BNE   TYPE02                                                           
*                                                                               
         OC    EXDESCNT,EXDESCNT                                                
         BNZ   PRTEXDES                                                         
*                                                                               
CONT     MVC   EXDESCNT,DESEXCNT                                                
         MVC   AEXDES,=A(EXDES)                                                 
         MOVE  (EXDES,840),DESXPDES                                             
         XC    SRTREC,SRTREC                                                    
         MVI   SRTRLN#,X'FF'                                                    
*                                                                               
         MVC   SRTRYR,DESUPDCC                                                  
         MVC   SRTRMON,DESUPDMM                                                 
         MVC   SRTRDAY,DESUPDDD                                                 
         MVC   SRTRHH,DESUPDHH                                                  
         MVC   SRTRMM,DESUPDMI                                                  
         MVC   SRTRSS,DESUPDSS                                                  
         MVC   SRTRMR#,DESNUMB                                                  
         BRAS  RE,SRTPUT                                                        
*                                                                               
         MVC   PMR#,DESNUMB                                                     
*                                                                               
         MVC   PDATEMON,DESUPDMM                                                
         MVI   PDATEMON+L'PDATEMON,C'/'                                         
         MVC   PDATEDAY,DESUPDDD                                                
         MVI   PDATEDAY+L'PDATEDAY,C'/'                                         
         MVC   PDATEYR,DESUPDCC                                                 
*                                                                               
         MVC   PTIMEHR,DESUPDHH                                                 
         MVI   PTIMEHR+L'PTIMEHR,C':'                                           
         MVC   PTIMEMIN,DESUPDMI                                                
         MVI   PTIMEMIN+L'PTIMEMIN,C':'                                         
         MVC   PTIMESEC,DESUPDSS                                                
*                                                                               
         MVC   PUSERID,DESADDID                                                 
*                                                                               
*        MVC   PSTAT,DESSTAT                                                    
*                                                                               
         MVC   PDES,DESDESCR                                                    
*                                                                               
         BRAS  RE,SRTPUT                                                        
         B     NEXT                                                             
*                                                                               
TYPE02   DS    0H                                                               
         CLC   MRTYPE,=C'02'                                                    
         BNE   NEXT                                                             
*                                                                               
         MVC   PPBNAME,MRFRMEM                                                  
         MVI   PPBTYPE-1,C'('                                                   
         MVC   PPBTYPE(L'MRLIB),MRLIB                                           
         MVI   PPBTYPE+L'MRLIB,C' '                                             
         MVC   PPBTYPE+L'MRLIB+1(L'MRSUB),MRSUB                                 
         CLC   MRSUB,SPACES                                                     
         BE    *+8                                                              
         MVI   PPBTYPE+L'MRLIB,C'/'                                             
         LA    RE,PPBTYPE+L'PPBTYPE-1                                           
         CLI   0(RE),C' '          PLACE CLOSE PAREN AT END OF STRING           
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C')'                                                       
         BRAS  RE,GETPHASE         GET THE PHASE NAME OF THIS BOOK              
*                                                                               
         LH    RF,EXDESCNT                                                      
         LTR   RF,RF                                                            
         BZ    PRT                                                              
         L     RE,AEXDES                                                        
         MVC   PEXDES,0(RE)                                                     
         AHI   RE,L'EXDES                                                       
         ST    RE,AEXDES                                                        
         BCTR  RF,0                                                             
         STH   RF,EXDESCNT                                                      
*                                                                               
PRT      BRAS  RE,SRTPUT                                                        
         B     NEXT                                                             
*                                                                               
PRTEXDES DS    0H                                                               
         LH    RF,EXDESCNT                                                      
         LTR   RF,RF                                                            
         BZ    CONT                                                             
*                                                                               
         L     RE,AEXDES                                                        
         MVC   PEXDES,0(RE)                                                     
         AHI   RE,L'EXDES                                                       
         ST    RE,AEXDES                                                        
         BCTR  RF,0                                                             
         STH   RF,EXDESCNT                                                      
         BRAS  RE,SRTPUT                                                        
         B     PRTEXDES                                                         
*                                                                               
PRTLAST  DS    0H                                                               
         LH    RF,EXDESCNT                                                      
         LTR   RF,RF                                                            
         BZ    CLOSE                                                            
         L     RE,AEXDES                                                        
         MVC   PEXDES,0(RE)                                                     
         AHI   RE,L'EXDES                                                       
         ST    RE,AEXDES                                                        
         BCTR  RF,0                                                             
         STH   RF,EXDESCNT                                                      
         BRAS  RE,SRTPUT                                                        
         B     PRTLAST                                                          
*                                                                               
CLOSE    CLOSE FILE                                                             
*                                                                               
*                                                                               
SRTGET   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)                                                      
         BZ    SRTEND                                                           
         MVC   P,SRTRP-SRTREC(RF)                                               
         GOTO1 =V(PRINTER)                                                      
         B     SRTGET                                                           
*                                                                               
SRTEND   GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         XBASE                                                                  
*                                                                               
*                                                                               
SRTPUT   NTR1                                                                   
         OC    P,SPACES                                                         
         MVC   SRTRP,P                                                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         XC    P,P                                                              
         IC    RF,SRTRLN#                                                       
         BCTR  RF,0                                                             
         STC   RF,SRTRLN#                                                       
         XIT1                                                                   
*                                                                               
*                                                                               
GETPHASE NTR1                                                                   
*                                                                               
         MVC   OUTNAME,SPACES                                                   
*                                                                               
         CLC   =C'ACAT',MRLIB                                                   
         BE    GPACAT                                                           
         CLC   =C'ALNK',MRLIB                                                   
         BE    GPALNK                                                           
         CLC   =C'LNK ',MRLIB                                                   
         BE    GPLNK                                                            
         CLC   =C'INCL',MRLIB                                                   
         BE    GPX                                                              
         CLC   =C'SRCE',MRLIB                                                   
         BE    GPX                                                              
         CLC   =C'SEC ',MRLIB                                                   
         BE    GPX                                                              
         CLC   =C'MACR',MRLIB       *** NEEDS WORK ***                          
         BE    GPX                                                              
         CLC   =C'DICT',MRLIB       *** NEEDS WORK ***                          
         BE    GPX                                                              
         CLC   =C'GEN ',MRLIB                                                   
         BE    GPGEN                                                            
         CLC   =C'APG ',MRLIB                                                   
         BE    GPAPG                                                            
         CLC   =C'RRG ',MRLIB                                                   
         BE    GPRRG                                                            
         CLC   =C'DPG ',MRLIB                                                   
         BE    GPDPG                                                            
         DC    H'0'                UNKNOWN LIB CODE                             
*                                                                               
GPACAT   GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    GPXNF                                                            
         CLC   CATALP,RECORD                                                    
         BE    GPACAT10                                                         
         CLC   =C'/*',RECORD       END OF BOOK                                  
         BNE   GPACAT                                                           
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPACAT10 LA    R5,RECORD+8                                                      
*                                                                               
GPACAT20 CLI   0(R5),X'40'               START OF NAME?                         
         BNE   GPACAT30                  YES, EXTRACT IT                        
         LA    R5,1(R5)                  NO, ADVANCE POINTER                    
         B     GPACAT20                                                         
*                                                                               
GPACAT30 TRT   0(9,R5),TRTBLANK          FIND BLANK OR COMMA                    
         SR    R1,R5                     LENGTH OF NAME                         
         MVC   OUTNAME(2),=CL2'RM'       RM PREFIX FOR OBJECT                   
         BCTR  R1,0                      DECREMENT FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTNAME+2(0),0(R5)                                               
         B     GPX                                                              
*                                                                               
GPALNK   DS    0H                                                               
GPLNK    DS    0H                                                               
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    GPXNF                                                            
         CLC   PHASE,RECORD                                                     
         BE    GPLNK10                                                          
         CLC   =C'/*',RECORD       END OF BOOK                                  
         BNE   GPLNK                                                            
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPLNK10  LA    R5,RECORD+7               ADVANCE PAST *PHASE                    
*                                                                               
GPLNK15  CLI   0(R5),X'40'                                                      
         BNE   GPLNK20                   FOUND FIRST NONBLANK                   
         LA    R5,1(R5)                  ADVANCE POINTER                        
         B     GPLNK15                   LOOP                                   
*                                                                               
GPLNK20  TRT   0(10,R5),TRTBLANK         FIND BLANK OR COMMA                    
         SR    R1,R5                     LENGTH OF NAME                         
         BCTR  R1,0                                                             
         BCTR  R1,0                      CUT OFF TEST LEVEL LETTER              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTNAME(0),0(R5)                                                 
*                                                                               
         CHI   R1,6                      WAS THE NAME 8 CHARS?                  
         BL    GPX                       NO, DONE, EXIT                         
*                                                                               
*CHECK FOR PHASE NAME OVERRIDE WHEN PHASE IS 8 CHARS                            
         LA    R5,3(R5,R1)               1ST CHAR PAST DELIM                    
         LA    R4,20                     MAX ITERATIONS                         
*                                        CAPTURE NAME                           
GPLNK30  CLI   0(R5),X'40'                                                      
         BNE   GPLNK40                   FOUND FIRST NONBLANK                   
         LA    R5,1(R5)                  ADVANCE POINTER                        
         BCT   R4,GPLNK30                LOOP                                   
         B     GPX                       NO PHASE NAME OVERRIDE, DONE           
*                                                                               
*                                        FIND END OF BASE                       
GPLNK40  CLI   0(R5),C'*'                IS IT *?                               
         BE    GPX                       YES, DONE                              
         CLI   0(R5),C'+'                IS IT +?                               
         BE    GPX                       YES, DONE                              
*                                                                               
         SR    R1,R1                     CLEAR FOR TRT                          
         TRT   0(10,R5),TRTBLANK         FIND BLANK OR COMMA                    
         SR    R1,R5                     LENGTH OF NAME                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTNAME(0),0(R5)                                                 
         B     GPX                       USE PHASE NAME OVERRIDE, DONE          
*                                                                               
*                                                                               
GPGEN    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    GPXNF                                                            
         CLI   RECORD,C'S'               POSSIBLE SCREEN CARD?                  
         BNE   *+12                      NO                                     
         CLI   RECORD+4,C'T'             IS IT THE SCREEN CARD?                 
         BE    GPGEN10                                                          
         CLC   =C'/*',RECORD       END OF BOOK                                  
         BNE   GPGEN                                                            
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPGEN10  MVC   OUTNAME(4),RECORD+4       ROOT NAME PPPP                         
         MVC   OUTNAME+4(2),RECORD+1     SUFFIX SS                              
         B     GPX                                                              
*                                                                               
*                                                                               
GPAPG    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    GPXNF                                                            
         CLC   APGPHASE,RECORD           PHASE RECORD?                          
         BE    GPAPG10                                                          
         CLC   =C'/*',RECORD       END OF BOOK                                  
         BNE   GPAPG                                                            
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPAPG10  MVC   OUTNAME(2),=CL2'AC'                                              
         MVC   OUTNAME+2(5),RECORD+9     CONSTRUCT PHASE NAME                   
         B     GPX                                                              
*                                                                               
*                                                                               
GPDPG    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    GPXNF                                                            
         CLC   DPGPHASE,RECORD           PHASE RECORD?                          
         BE    GPDPG10                                                          
         CLC   =C'/*',RECORD       END OF BOOK                                  
         BNE   GPDPG                                                            
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPDPG10  MVC   OUTNAME(6),RECORD+15      RETRIEVE PHASE NAME                    
         B     GPX                                                              
*                                                                               
*                                                                               
GPRRG    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    GPXNF                                                            
         CLC   RRGPHASE,RECORD           PHASE RECORD?                          
         BE    GPRRG10                                                          
         CLC   =C'/*',RECORD       END OF BOOK                                  
         BNE   GPRRG                                                            
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPRRG10  MVC   OUTNAME(4),=CL4'RERG'     CONSTRUCT PHASE NAME                   
         MVC   OUTNAME+4(2),RECORD+9                                            
         B     GPX                                                              
*                                                                               
GPXNF    MVC   PPBPHASE,=CL10'*NOT FOUND'                                       
         B     GPX2                                                             
*                                                                               
GPX      MVC   PPBPHASE,OUTNAME                                                 
GPX2     GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,21,D),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=153'                                   
SRTREC   DS    CL153                                                            
         ORG   SRTREC                                                           
SRTRYR   DS    CL4                 YEAR                                         
SRTRMON  DS    CL2                 MONTH                                        
SRTRDAY  DS    CL2                 DAY                                          
SRTRHH   DS    CL2                 HOUR                                         
SRTRMM   DS    CL2                 MIN                                          
SRTRSS   DS    CL2                 SEC                                          
SRTRMR#  DS    CL2                 MOVE REQUEST #                               
SRTRLN#  DS    CL1                 LINE # FOR SORTER                            
SRTRP    DS    CL132               PRINT LINE                                   
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
DUB      DS    D                                                                
EXDESCNT DS    H                   EXPANDED DESCRIPTION COUNTER                 
AEXDES   DS    F                   A(NEXT EX DESC BLOCK TO BE PRINTED)          
FILE     DCB   DDNAME=INFILE,DSORG=PS,MACRF=GM,EODAD=PRTLAST                    
RECORD   DC    CL80' '                                                          
EXDES    DS    12CL70                                                           
*                                                                               
OUTNAME  DS    CL10                                                             
CATALP   DC    CL8'*CATALP '                                                    
PHASE    DC    CL7'*PHASE '                                                     
APGPHASE DC    CL9'PHASE    '                                                   
DPGPHASE DC    CL15'         PHASE '                                            
RRGPHASE DC    CL9'PHASE    '                                                   
*                                                                               
         DS    0D                                                               
TRTBLANK DC    256X'00'                                                         
         ORG   TRTBLANK+X'40'                                                   
         DC    X'FF'                                                            
         ORG   TRTBLANK+C','                                                    
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
*TEMP     DS    256C                                                            
*                                                                               
REC      DS    2000C                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT GEN                                                              
         APAMMDES                                                               
         EJECT                                                                  
MMMBRD   DSECT                                                                  
         APAMMMBR                                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
PMR#     DS    CL6                 MOVE REQUEST NUMBER                          
         DS    CL1                                                              
         DS    CL1                                                              
PDATE    DS    0CL10                                                            
PDATEMON DS    CL2                 MONTH                                        
         DC    C'/'                                                             
PDATEDAY DS    CL2                 DAY                                          
         DC    C'/'                                                             
PDATEYR  DS    CL4                 YEAR                                         
         DS    CL1                                                              
PTIME    DS    0CL8                TIME                                         
PTIMEHR  DS    CL2                 HOUR                                         
         DC    C':'                                                             
PTIMEMIN DS    CL2                 MINUTE                                       
         DC    C':'                                                             
PTIMESEC DS    CL2                 SECOND                                       
         DS    CL3                                                              
PUSERID  DS    CL8                 USER ID                                      
         DS    CL3                                                              
*PSTAT    DS    CL6                 STATUS OF MOVE REQUEST                      
*         DS    CL1                                                             
PDES     DS    CL55                DESCRIPTION                                  
         ORG   PDES+2                                                           
PEXDES   DS    CL70                EXPANDED DESCRIPTION                         
         ORG   PDATE                                                            
PPBNAME  DS    CL10                PAN BOOK NAME                                
         DS    CL1                                                              
PPBPHASE DS    CL10                PHASE NAME                                   
         DS    CL6                                                              
PPBTYPE  DS    CL8                 PAN LIBCODE (LIBC/SUB)                       
         DS    CL1                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056DDPANMRRPT08/09/06'                                      
         END                                                                    

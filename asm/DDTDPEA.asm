*          DATA SET DDTDPEA    AT LEVEL 040 AS OF 09/25/96                      
*PHASE DDTDPEA,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DATCON                                                                 
DPEA     TITLE 'TEST'                                                           
         PRINT NOGEN                                                            
         SPACE 1                                                                
DPEA     CSECT                                                                  
         NBASE 0,**DPEA**,RA,WORK=A(WORKCHN)                                    
         LA    RC,WORKAREA                                                      
         USING WORKD,RC                                                         
         USING TLSTD,TSARLST1                                                   
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         GOTO1 VLOADER,PARM,=CL8'T00A7D',0                                      
         MVC   ATSAR,4(R1)         A(TSAROFF)                                   
*                                                                               
         GOTO1 VALINP              VALIDATE INPUT CARDS                         
         BNE   XBASE                                                            
*                                                                               
         GOTO1 GETDIR                                                           
         GOTO1 GETPAN                                                           
         GOTO1 PRCLIB                                                           
         GOTO1 PRCPAN                                                           
*                                                                               
         GOTO1 PRTPHA                                                           
         GOTO1 PRTSCR                                                           
*                                                                               
         GOTO1 OUTPHA                                                           
*                                                                               
XBASE    XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT CARDS TO SET UP BOOKTABD                             *         
***********************************************************************         
         SPACE 1                                                                
VALINP   NTR1  ,                                                                
         MVC   P(L'ISTCARD),ISTCARD                                             
         GOTO1 VPRINTER                                                         
         LA    R4,BOOKTAB                                                       
         LA    R0,BOOKTABN                                                      
         USING BOOKTABD,R4                                                      
*                                                                               
VINP02   GOTO1 VCARDS,PARM,CARD,=C'RE00'                                        
         CLC   =CL2'/*',CARD       END OF JCL                                   
         BE    VINP10                                                           
         CLI   CARD,C'*'           IGNORE COMMENT CARD                          
         BE    VINP02                                                           
*                                                                               
         LA    R0,32               SET LENGTH OF INPUT                          
         LA    RF,CARD                                                          
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         LA    RE,32                                                            
         SR    RE,R0                                                            
         STC   RE,CARDLN                                                        
*                                                                               
         GOTO1 PARTINP,PARM,(CARDLN,CARD)                                       
         CLI   PARTALN,0           SET MIN/MAX BOOK VALUES                      
         BE    VALINPIN                                                         
         CLI   PARTALN,L'BOOKMIN                                                
         BH    VALINPIN                                                         
         MVC   BOOKMIN,PARTA                                                    
         CLI   PARTBLN,0                                                        
         BE    VALINPIN                                                         
         CLI   PARTBLN,L'BOOKMAX                                                
         BH    VALINPIN                                                         
         MVC   BOOKMAX,PARTB                                                    
         CLC   BOOKMIN,BOOKMAX                                                  
         BH    VALINPIN                                                         
         MVC   P(32),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R4,BOOKTABL(R4)                                                  
         BCT   R0,VINP02                                                        
*                                                                               
VINP10   MVI   BOOKTABD,EOT                                                     
         DROP  R4                                                               
*                                                                               
         LA    RE,BOOKTABN                                                      
         SR    RE,R0               TEST AT LEAST ONE ENTRY                      
         BZ    VALINPMI                                                         
*                                                                               
VALINPY  MVC   P(L'IENCARD),IENCARD                                             
         GOTO1 VPRINTER                                                         
         B     EXITY                                                            
*                                                                               
VALINPIN XR    RE,RE               INVALID CARD                                 
         ICM   RE,1,CARDLN                                                      
         BNZ   *+4                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   P(0),CARD                                                        
         LA    RF,P+2(RE)                                                       
         MVC   0(L'EINCARD,RF),EINCARD                                          
         GOTO1 VPRINTER                                                         
         B     EXITN                                                            
*                                                                               
VALINPMI MVC   P(L'EMICARD),EMICARD  MISSING CARD                               
         GOTO1 VPRINTER                                                         
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PARTITION INPUT                                          *         
*                                                                     *         
* NTRY: P1=(L'INPUT,A(INPUT))                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PARTINP  NTR1  ,                                                                
         MVI   PARTALN,0                                                        
         MVC   PARTA,SPACES                                                     
         MVI   PARTBLN,0                                                        
         MVC   PARTB,EFFS                                                       
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)          R2=A(INPUT)                                  
*                                                                               
         XR    RE,RE                                                            
         IC    RE,0(R1)            RE=L(INPUT)                                  
         LR    RF,RE                                                            
         LR    R1,R2               R1=A(INPUT)                                  
         CLI   0(R1),C'-'                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
*                                                                               
         SR    RF,RE               COPY PART A                                  
         BZ    PINP02                                                           
         STC   RF,PARTALN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PARTA(0),0(R2)                                                   
         LTR   RE,RE               TEST PARTITION CHARACTER FOUND               
         BNZ   PINP02                                                           
         MVC   PARTBLN,PARTALN     NO SO PARTB=PARTA                            
         EX    RF,*+4                                                           
         MVC   PARTB(0),0(R2)                                                   
         B     PARTINPX                                                         
*                                                                               
PINP02   BCT   RE,*+8                                                           
         B     PARTINPX                                                         
         STC   RE,PARTBLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   PARTB(0),1(R1)                                                   
*                                                                               
PARTINPX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET PAN DIRECTORY BOOKS                                             *         
***********************************************************************         
         SPACE 1                                                                
GETDIR   NTR1  ,                                                                
         LA    R4,BOOKTAB                                                       
         USING BOOKTABD,R4                                                      
*                                                                               
GDIR02   CLI   BOOKTABD,EOT                                                     
         BE    GDIR10                                                           
         MVC   PANBOOK,BOOKMIN                                                  
*                                                                               
GDIR04   GOTO1 VPANIC,PARM,(X'40',READ),DIR,PANBOOK,CARD                        
         CLC   =CL2'/*',CARD       TEST END OF DIRECTORY                        
         BE    GDIR08                                                           
         CLC   BOOKMAX,CARD        TEST HIGHER THAN MAX                         
         BL    GDIR08                                                           
         CLC   BOOKMIN,CARD                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    TLSTD(TLSTL),TLSTD                                               
                                                                                
         PUSH  USING                                                            
         USING DIRD,CARD                                                        
         MVI   TLKTYP,TLKDIRQ                                                   
         MVC   TLKBOOK,DIRNAME                                                  
         MVC   TLBNAME,DIRNAME                                                  
         MVC   TLBLEVEL,DIRLEVEL                                                
         GOTO1 VDATCON,PARM,(4,DIRDATM),(1,TLBDATE)                             
         LA    R0,L'TLBNAME-1                                                   
         LA    RF,TLBNAME+L'TLBNAME-1                                           
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,TLBNAMEL                                                      
         POP   USING                                                            
         GOTO1 TSARIO,TSAADD                                                    
*                                                                               
         B     GDIR04                                                           
*                                                                               
GDIR08   LA    R4,BOOKTABL(R4)                                                  
         B     GDIR02                                                           
         DROP  R4                                                               
*                                                                               
GDIR10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET PANBOOK DATA                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETPAN   NTR1  ,                                                                
O        USING TLSTD,TSARLST2                                                   
         XR    R3,R3                                                            
GPAN02   LA    R3,1(R3)                                                         
         STH   R3,O.TLNUM                                                       
         CLC   O.TLNUM,TSAR#                                                    
         BH    GPAN10                                                           
         GOTO1 TSARIO,TSAGET+IO2                                                
         CLI   O.TLKTYP,TLKDIRQ                                                 
         BNE   GPAN10                                                           
*                                                                               
GPAN04   GOTO1 VPANIC,PARM,READ,PAN,O.TLBNAME,CARD                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'* ',CARD         IGNORE LEVEL STAMP                           
         BE    GPAN04                                                           
         CLC   =C'TITLE',CARD+09   IGNORE TITLE                                 
         BE    GPAN04                                                           
*                                                                               
         XC    TLSTD(TLSTL),TLSTD  CLEAR TSAR RECORD ADDING                     
         MVC   TLDATA,O.TLDATA     COPY EXISTING DATA                           
*                                                                               
         GOTO1 TSTPHA              TEST PHASE BOOK                              
         BE    GPAN08                                                           
         GOTO1 TSTCAT              TEST CATALP BOOK                             
         BE    GPAN08                                                           
         GOTO1 TSTSCR              TEST SCREEN BOOK                             
         BE    GPAN08                                                           
         GOTO1 TSTOTH              SET OTHER                                    
*                                                                               
GPAN08   DS    0H                                                               
         GOTO1 TSARIO,TSAADD       ADD NEW TSAR RECORD                          
         B     GPAN02                                                           
*                                                                               
GPAN10   DS    0H                                                               
         GOTO1 VPANIC,PARM,CLOSE,PAN                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TEST AND SET PHASE BOOK INFO                                        *         
***********************************************************************         
         SPACE 1                                                                
TSTPHA   NTR1  ,                                                                
         CLC   =C'*PHASE ',CARD                                                 
         BNE   EXITN                                                            
         MVI   TLKTYP,TLKPHAQ                                                   
*                                                                               
         MVC   TLPNAME,SPACES      SET PHASE NAME                               
         LA    RF,CARD+7                                                        
         LA    RE,TLPNAME                                                       
TPHA02   MVC   0(1,RE),0(RF)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         CLI   0(RF),C','                                                       
         BNE   TPHA02                                                           
         GOTO1 NUMPHA,PARM,TLPNAME,TLPNUM                                       
         BNE   *+10                                                             
         MVC   TLPNAME+6(L'TLPNAME-6),SPACES                                    
*                                                                               
         MVC   TLKPHASE,TLPNAME                                                 
         TR    TLKPHASE,TRTTAB                                                  
         GOTO1 TSTSUF                                                           
         BE    TPHA04                                                           
         GOTO1 ADDLIB,PARM,TLPNAME                                              
         B     TSTPHAX                                                          
*                                                                               
TPHA04   GOTO1 ADDSUF,PARM,L'TLPNAME,TLPNAME,TLKSUFF,TLPNAME                    
*                                                                               
TSTPHAX  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* TEST AND SET CATALP BOOK INFO                                       *         
***********************************************************************         
         SPACE 1                                                                
TSTCAT   NTR1  ,                                                                
         CLC   =C'*CATALP ',CARD                                                
         BNE   EXITN                                                            
         MVI   TLKTYP,TLKCATQ                                                   
*                                                                               
         MVC   TLCNAME,SPACES      SET CATALP NAME                              
         LA    RF,CARD+8                                                        
         LA    RE,TLCNAME                                                       
TCAT02   MVC   0(1,RE),0(RF)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         CLI   0(RF),C','                                                       
         BNE   TCAT02                                                           
*                                                                               
         MVC   TLKPHASE,TLCNAME                                                 
         TR    TLKPHASE,TRTTAB                                                  
         GOTO1 TSTSUF                                                           
         BNE   TSTCATX                                                          
         GOTO1 ADDSUF,PARM,L'TLCNAME,TLCNAME,TLKSUFF,TLCNAME                    
*                                                                               
TSTCATX  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* TEST AND SET SCREEN BOOK INFO                                       *         
***********************************************************************         
         SPACE 1                                                                
TSTSCR   NTR1  ,                                                                
         CLI   CARD,C'S'                                                        
         BNE   EXITN                                                            
         MVI   TLKTYP,TLKSCRQ                                                   
*                                                                               
         MVC   TLSNAME,SPACES      SET SCREEN PHASE NAME                        
         MVC   TLSNAME(4),CARD+4                                                
         MVC   TLSNAME+4(2),CARD+1                                              
         GOTO1 NUMPHA,PARM,TLSNAME,TLSNUM                                       
         GOTO1 ADDLIB,PARM,TLSNAME                                              
*                                                                               
         MVC   TLKPHASE,TLSNAME                                                 
         TR    TLKPHASE,TRTTAB                                                  
         XC    TLKPHASE+4(L'TLKPHASE-4),EFFS                                    
         GOTO1 TSTSUF                                                           
         BNE   TSCR02                                                           
         GOTO1 ADDSUF,PARM,L'TLSNAME,TLSNAME,TLKSUFF,TLSNAME                    
*                                                                               
D        USING TLSTD,TSARLST3      TEST DSECT PAN BOOK ON FILE                  
TSCR02   XC    D.TLKEY,D.TLKEY                                                  
         MVI   D.TLKTYP,TLKDIRQ                                                 
         MVC   D.TLKBOOK,TLBNAME                                                
         XR    RE,RE                                                            
         IC    RE,TLBNAMEL                                                      
         LA    RE,D.TLKBOOK+1(RE)                                               
         MVI   0(RE),C'D'                                                       
         GOTO1 TSARIO,TSARDH+IO3                                                
         BNE   TSTSCRX                                                          
         MVC   TLSDNAME,D.TLBNAME  SCREEN DSECT BOOK NAME                       
         MVC   TLSDLEV,D.TLBLEVEL  SCREEN DSECT LEVEL                           
         MVC   TLSDDATE,D.TLBDATE  SCREEN DSECT DATE                            
         GOTO1 TSARIO,TSADEL+IO3   DELETE TSAR RECORD                           
         DROP  D                                                                
*                                                                               
TSTSCRX  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SET OTHER BOOK INFO                                                 *         
***********************************************************************         
         SPACE 1                                                                
TSTOTH   NTR1  ,                                                                
         MVI   TLKTYP,TLKOTHQ                                                   
         GOTO1 TSTSUF                                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* TEST SUFFIX ROUTINE - TESTS WHETHER PAN BOOK IS A PREFIX OF ANOTHER *         
*                                                                     *         
* NTRY: TLSTD = NEW RECORD                                            *         
* EXIT:  CC   = EQUAL IS A SUFFIX/PREFIX                              *         
***********************************************************************         
         SPACE 1                                                                
TSTSUF   NTR1  ,                                                                
*                                                                               
E        USING TLSTD,TSARLST3                                                   
         XC    E.TLKEY,E.TLKEY                                                  
         MVC   E.TLKEY(TLKBOOK-TLKEY),TLKEY                                     
*                                                                               
         LA    R1,TSARDH+IO3                                                    
         B     *+8                                                              
TSUF02   LA    R1,TSANXT+IO3       FIND TSAR RECORD FOR THIS PHASE              
         GOTO1 TSARIO                                                           
         BL    TSUF10                                                           
         CLC   E.TLKEY(TLKBOOK-TLKEY),TLKEY                                     
         BNE   TSUF10                                                           
*                                                                               
         CLC   TLBNAMEL,E.TLBNAMEL TEST BOOK IS A PREFIX OF THIS ONE            
         BNH   TSUF02                                                           
         XR    RE,RE                                                            
         IC    RE,E.TLBNAMEL                                                    
         EX    RE,*+8                                                           
         BNE   TSUF02                                                           
         CLC   TLBNAME(0),E.TLBNAME                                             
*                                                                               
         MVC   TLKBOOK,E.TLKBOOK   SET PAN BOOK IN NEW KEY                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'TLBNAME),TLBNAME                                          
         LA    RE,WORK+1(RE)                                                    
         MVC   TLKSUFF,0(RE)       SET SUFFIX IN NEW KEY                        
*                                                                               
         B     EXITY                                                            
         DROP  E                                                                
*                                                                               
TSUF10   MVC   TLKBOOK,TLBNAME                                                  
         MVC   TLKSUFF,SPACES                                                   
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  O                                                                
         EJECT                                                                  
***********************************************************************         
* ADD LOADLIB TSAR RECORD                                             *         
*                                                                     *         
* NTRY: P1 = A(PHASE NAME)                                            *         
***********************************************************************         
         SPACE 1                                                                
ADDLIB   NTR1  ,                                                                
         L     R2,0(R1)                                                         
*                                                                               
L        USING TLSTD,TSARLST3                                                   
         XC    L.TLSTD(TLSTL),L.TLSTD                                           
         MVI   L.TLKTYP,TLKLIBQ                                                 
         MVC   L.TLKPHASE,0(R2)                                                 
         GOTO1 TSARIO,TSARDH+IO3                                                
         BE    EXIT                                                             
         XC    L.TLSTD(TLSTL),L.TLSTD                                           
         MVI   L.TLKTYP,TLKLIBQ                                                 
         MVC   L.TLKPHASE,0(R2)                                                 
         GOTO1 TSARIO,TSAADD+IO3                                                
         B     EXIT                                                             
         DROP  L                                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS LOADLIB RECORDS - TEST WHICH EXIST                          *         
***********************************************************************         
         SPACE 1                                                                
PRCLIB   NTR1  ,                                                                
         XC    TLKEY,TLKEY                                                      
         MVI   TLKTYP,TLKLIBQ                                                   
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PLIB02   LA    R1,TSANXT                                                        
         GOTO1 TSARIO                                                           
         BL    PLIB10                                                           
         CLI   TLKTYP,TLKLIBQ                                                   
         BNE   PLIB10                                                           
*                                                                               
         LA    R4,TLDSUF           R4=A(SUFFUX LIST)                            
*                                                                               
         LA    R3,L'TLKPHASE-1     R3 = EXECUTABLE LENGTH OF PHASE NAME         
         LA    R2,TLKPHASE+L'TLKPHASE-1                                         
         CLI   0(R2),C' '                                                       
         BH    PLIB04                                                           
         BCTR  R2,0                                                             
         BCT   R3,*-10                                                          
         DC    H'0'                                                             
*                                                                               
PLIB04   EX    R3,PLIBCLC                                                       
         BH    PLIB08                                                           
         BE    PLIB06                                                           
PLIB05   GOTO1 NXTLIB                                                           
         BE    PLIB04                                                           
         B     PLIB08                                                           
PLIBCLC  CLC   LIBNAME(0),TLKPHASE                                              
*                                                                               
PLIB06   MVC   0(L'TLDSUF,R4),SPACES                                            
         LA    RE,L'TLKPHASE-2                                                  
         SR    RE,R3                                                            
         BM    PLIB07                                                           
         LA    RF,LIBNAME+1(R3)                                                 
         EX    RE,*+4                                                           
         MVC   0(0,R4),0(RF)                                                    
PLIB07   LA    R4,L'TLDSUF(R4)                                                  
         B     PLIB05                                                           
*                                                                               
PLIB08   MVI   0(R4),EOT                                                        
         GOTO1 TSARIO,TSAPUT                                                    
         B     PLIB02                                                           
*                                                                               
PLIB10   GOTO1 CLOLIB                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS PAN TSAR RECORDS                                            *         
***********************************************************************         
         SPACE 1                                                                
PRCPAN   NTR1  ,                                                                
         XC    TLKEY,TLKEY                                                      
         MVI   TLKTYP,TLKPHAQ                                                   
         GOTO1 TSARIO,TSARDH                                                    
         BL    EXIT                                                             
         LH    R3,TLNUM                                                         
         B     PPAN04                                                           
*                                                                               
PPAN02   LA    R3,1(R3)                                                         
         STH   R3,TLNUM                                                         
         CLC   TLNUM,TSAR#                                                      
         BH    EXIT                                                             
         GOTO1 TSARIO,TSAGET                                                    
PPAN04   CLC   TLKSUFF,SPACES      ONLY PROCESS 'LIVE' BOOKS                    
         BNE   PPAN02                                                           
*                                                                               
         CLI   TLKTYP,TLKPHAQ                                                   
         BNE   *+12                                                             
         BAS   RE,PRCPHA                                                        
         B     PPAN08                                                           
         CLI   TLKTYP,TLKSCRQ                                                   
         BNE   *+12                                                             
         BAS   RE,PRCSCR                                                        
         B     PPAN08                                                           
*                                                                               
PPAN08   B     PPAN02                                                           
*                                                                               
PPAN10   B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS PHASE                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRCPHA   NTR1  ,                                                                
T        USING TLSTD,TSARLST2                                                   
         XC    PHASESUF,PHASESUF                                                
*                                                                               
PRCPHA02 GOTO1 NXTPHA,PARM,TLPNAME                                              
         BNE   PRCPHA10                                                         
         LM    R2,R3,0(R1)         SAVE LENGTH AND ADDRESS OF PHASE             
         MVC   T.TLSTD(TLSTL),TLSTD                                             
         CLI   PHASESUF,C' '       TEST LIVE VERSION                            
         BE    PRCPHA04                                                         
*                                                                               
         MVC   T.TLKSUFF,PHASESUF                                               
         GOTO1 TSARIO,TSARDH+IO2                                                
         BE    PRCPHA04                                                         
         MVC   T.TLSTD(TLSTL),TLSTD                                             
         MVC   T.TLKSUFF,PHASESUF                                               
         MVC   T.TLPNAME,PHASE                                                  
         XC    T.TLBDATA,T.TLBDATA                                              
*                                                                               
PRCPHA04 GOTO1 GETSTMP,PARM,(R2),(R3),T.TLPLEVEL,T.TLPDATE                      
         LA    R1,TSAPUT+IO2                                                    
         OC    T.TLBDATA,T.TLBDATA                                              
         BNZ   *+8                                                              
         LA    R1,TSAADD+IO2                                                    
         GOTO1 TSARIO                                                           
*                                                                               
         B     PRCPHA02                                                         
*                                                                               
PRCPHA10 DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS SCREEN                                                      *         
***********************************************************************         
         SPACE 1                                                                
PRCSCR   NTR1  ,                                                                
T        USING TLSTD,TSARLST2                                                   
         XC    PHASESUF,PHASESUF                                                
*                                                                               
PRCSCR02 GOTO1 NXTPHA,PARM,TLSNAME                                              
         BNE   PRCSCR10                                                         
         LM    R2,R3,0(R1)         SAVE LENGTH AND ADDRESS OF PHASE             
         MVC   T.TLSTD(TLSTL),TLSTD                                             
         CLI   PHASESUF,C' '       TEST LIVE VERSION                            
         BE    PRCSCR04                                                         
*                                                                               
         MVC   T.TLKSUFF,PHASESUF                                               
         GOTO1 TSARIO,TSARDH+IO2                                                
         BE    PRCSCR04                                                         
         MVC   T.TLSTD(TLSTL),TLSTD                                             
         MVC   T.TLKSUFF,PHASESUF                                               
         MVC   T.TLSNAME,PHASE                                                  
         XC    T.TLBDATA,T.TLBDATA                                              
         XC    T.TLSDNAME,T.TLSDNAME                                            
         XC    T.TLSDLEV,T.TLSDLEV                                              
         XC    T.TLSDDATE,T.TLSDDATE                                            
*                                                                               
PRCSCR04 DS    0H                                                               
         LA    R1,TSAPUT+IO2                                                    
         OC    T.TLBDATA,T.TLBDATA                                              
         BNZ   *+8                                                              
         LA    R1,TSAADD+IO2                                                    
         GOTO1 TSARIO                                                           
*                                                                               
         B     PRCSCR02                                                         
*                                                                               
PRCSCR10 DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT PHASE RECORD                                    *         
*                                                                     *         
* NTRY: P1 = A(PHASE NAME)                                            *         
*       PHASESUF = LAST SUFFIX GOT (0 TO GET FIRST)                   *         
* EXIT: PHASE = TEST PHASE NAME                                       *         
*       PHASESUF = SUFFIX                                             *         
*       P1 = L(PHASE)                                                 *         
*       P2 = A(PHASE)                                                 *         
*       CC = LOW IF NO MORE TEST PHASES                               *         
***********************************************************************         
         SPACE 1                                                                
NXTPHA   NTR1  ,                                                                
         LR    R5,R1               R5 = A(PARAMETER LIST)                       
         L     RF,0(R1)                                                         
         MVC   PHASE,0(RF)                                                      
L        USING TLSTD,TSARLST3                                                   
*                                                                               
         LA    R2,L.TLDSUF                                                      
         OC    PHASESUF,PHASESUF   TEST FIRST TIME                              
         BNZ   NPHA02                                                           
         XC    L.TLKEY,L.TLKEY                                                  
         MVI   L.TLKTYP,TLKLIBQ                                                 
         MVC   L.TLKPHASE,PHASE                                                 
         GOTO1 TSARIO,TSARDH+IO3                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     NPHA04                                                           
*                                                                               
NPHA02   LA    R0,TLDSUFN                                                       
NPHA03   CLC   PHASESUF,0(R2)                                                   
         LA    R2,L'TLDSUF(R2)                                                  
         BE    NPHA04                                                           
         BCT   R0,NPHA03                                                        
         DC    H'0'                                                             
*                                                                               
NPHA04   CLI   0(R2),EOT                                                        
         BE    EXITL                                                            
*                                                                               
         MVC   PHASESUF,0(R2)                                                   
         GOTO1 ADDSUF,PARM,L'PHASE,PHASE,PHASESUF,PHASE                         
*                                                                               
         GOTO1 VLOADER,PARM,PHASE,0                                             
         ICM   RF,15,4(R1)         TEST PHASE EXISTS                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,R5),0(R1)       RETURN VLOADER'S RETURN                      
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  L                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT PHASE BOOKS                                                   *         
***********************************************************************         
         SPACE 1                                                                
PRTPHA   NTR1  ,                                                                
         XC    TLKEY,TLKEY                                                      
         MVI   TLKTYP,TLKPHAQ                                                   
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PPHA02   LA    R1,TSANXT                                                        
         GOTO1 TSARIO                                                           
         BL    PPHA10                                                           
         CLI   TLKTYP,TLKPHAQ                                                   
         BNE   PPHA10                                                           
*                                                                               
         CLI   TLKSUFF,C' '                                                     
         BH    PPHA04                                                           
         MVI   P,C'.'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 VPRINTER                                                         
*                                                                               
PPHA04   CLI   TLBNAME,C' '                                                     
         BNH   PPHA06                                                           
         MVC   PPBNAME,TLBNAME                                                  
         MVC   PPBLEVEL,TLBLEVEL                                                
         GOTO1 VDATCON,PARM,(X'41',TLBDATE),(17,PPBDATE)                        
*                                                                               
PPHA06   CLI   TLPLEVEL,C' '                                                    
         BNH   PPHA08                                                           
         MVC   PPPNAME,TLPNAME                                                  
         MVC   PPPLEVEL,TLPLEVEL                                                
         GOTO1 VDATCON,PARM,(X'41',TLPDATE),(17,PPPDATE)                        
*                                                                               
PPHA08   GOTO1 VPRINTER                                                         
         B     PPHA02                                                           
*                                                                               
PPHA10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT SCREEN BOOKS                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTSCR   NTR1  ,                                                                
         XC    TLKEY,TLKEY                                                      
         MVI   TLKTYP,TLKSCRQ                                                   
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PSCR02   LA    R1,TSANXT                                                        
         GOTO1 TSARIO                                                           
         BL    PSCR10                                                           
         CLI   TLKTYP,TLKSCRQ                                                   
         BNE   PSCR10                                                           
*                                                                               
         CLI   TLKSUFF,C' '                                                     
         BH    PSCR04                                                           
         MVI   P,C'.'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 VPRINTER                                                         
*                                                                               
PSCR04   CLI   TLBNAME,C' '                                                     
         BNH   PSCR06                                                           
         MVC   PSBNAME,TLBNAME                                                  
         MVC   PSBLEVEL,TLBLEVEL                                                
         GOTO1 VDATCON,PARM,(X'41',TLBDATE),(17,PSBDATE)                        
*                                                                               
PSCR06   CLI   TLSDNAME,C' '                                                    
         BNH   PSCR07                                                           
         MVC   PSDNAME,TLSDNAME                                                 
         MVC   PSDLEVEL,TLSDLEV                                                 
         GOTO1 VDATCON,PARM,(X'41',TLSDDATE),(17,PSDDATE)                       
*                                                                               
PSCR07   CLI   TLSNAME,C' '                                                     
         BNH   PSCR08                                                           
         MVC   PSPNAME,TLSNAME                                                  
*                                                                               
PSCR08   GOTO1 VPRINTER                                                         
         B     PSCR02                                                           
*                                                                               
PSCR10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TAPE PHASE BOOKS                                                    *         
***********************************************************************         
         SPACE 1                                                                
OUTPHA   NTR1  ,                                                                
         OPEN  (PHAFILE,OUTPUT)                                                 
         XC    TLKEY,TLKEY                                                      
         MVI   TLKTYP,TLKPHAQ                                                   
         LA    R1,TSARDH                                                        
         B     *+8                                                              
OPHA02   LA    R1,TSANXT                                                        
         GOTO1 TSARIO                                                           
         BL    OPHA20                                                           
         CLI   TLKTYP,TLKPHAQ                                                   
         BNE   OPHA20                                                           
*                                                                               
         GOTO1 CLRDATA                                                          
*                                  ADD COUNTRY                                  
*&&UK*&& MVC   CARD(2),=C'UK'                                                   
*&&US*&& MVC   CARD(2),=C'US'                                                   
         GOTO1 ADDDATA,PARM,2,CARD                                              
*                                                                               
         CLI   TLBNAME,C' '        TEST PAN BOOK EXISTS                         
         BH    OPHA04                                                           
         GOTO1 ADDBLANK,3          NO - ADD 4 BLANKS                            
         B     OPHA06                                                           
*                                                                               
OPHA04   GOTO1 ADDDATA,PARM,L'TLBNAME,TLBNAME   ADD PAN BOOK NAME               
         GOTO1 (RF),(R1),L'TLBLEVEL,TLBLEVEL    ADD PAN BOOK LEVEL              
         GOTO1 ADDDATE,(R1),TLBDATE             ADD PAN BOOK DATE               
*                                                                               
OPHA06   CLI   TLPLEVEL,C' '       TEST LOADLIB PHASE EXISTS                    
         BH    OPHA08                                                           
         GOTO1 ADDBLANK,3          NO - ADD 3 BLANKS                            
         B     OPHA10                                                           
*                                                                               
OPHA08   GOTO1 ADDDATA,PARM,L'TLPNAME,TLPNAME   ADD LOADLIB NAME                
         GOTO1 (RF),(R1),L'TLPLEVEL,TLPLEVEL    ADD LOADLIB LEVEL               
         GOTO1 ADDDATE,(R1),TLPDATE             ADD LOADLIB DATE                
*                                                                               
OPHA10   DS    0H                                                               
         GOTO1 ADDDATA,PARM,L'TLKBOOK,TLKBOOK   ADD PANBOOK KEY                 
*                                                                               
         MVC   CARD(L'TLKPHASE),TLKPHASE                                        
         TR    CARD(L'TLKPHASE),TRTTAB                                          
         GOTO1 ADDDATA,PARM,L'TLKPHASE,CARD     ADD LOADLIB KEY                 
*                                                                               
         GOTO1 ADDDATA,PARM,L'TLKSUFF,TLKSUFF   ADD SUFFIX                      
*                                                                               
         PUT   PHAFILE,DATALEN                                                  
         B     OPHA02                                                           
*                                                                               
OPHA20   CLOSE PHAFILE                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TSARIO ROUTINE                                                      *         
*                                                                     *         
* NTRY : R1 = ACTION + IO AREA                                        *         
* EXIT : CC = LOW FOR END-OF-FILE ERROR (TSARDH, TSANXT)              *         
*        CC = HIGH IF RECORD NOT FOUND (TSARDH)                       *         
***********************************************************************         
         SPACE 1                                                                
TSARIO   NTR1  ,                                                                
         PUSH  USING                                                            
         USING TSARD,TSARBLK                                                    
*                                                                               
         LR    R3,R1               R3 = TSAR ACTION                             
         SRL   R1,8                                                             
         SLL   R1,8                                                             
         LA    R2,TSARLST1(R1)                                                  
T        USING TLSTD,R2            R2 = A(TSAR RECORD AREA)                     
*                                                                               
         TM    TSINDS,TSIINIOK     TEST INITIALIZED                             
         BO    TSARIO02                                                         
         L     R0,TSARBUFL                                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         MVI   TSOFFACT,TSAINI                                                  
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
         MVI   TSKEYL,TLKEYL                                                    
         MVC   TSRECL,=AL2(TLRECL)                                              
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    TSAR#,TSAR#                                                      
*                                                                               
TSARIO02 STC   R3,TSOFFACT                                                      
         CLI   TSOFFACT,TSAINI     TEST INITIALIZATION                          
         BE    TSARIOX                                                          
         LA    RE,T.TLREC                                                       
         ST    RE,TSAREC                                                        
         MVC   TSRNUM,T.TLNUM                                                   
         GOTO1 ATSAR,TSARD                                                      
         MVC   T.TLNUM,TSRNUM                                                   
*                                                                               
         CLI   TSOFFACT,TSAADD     TEST ADDED                                   
         BNE   TSARIO04                                                         
         LH    RE,TSAR#            YES - INCREMENT NUMBER                       
         LA    RE,1(RE)                                                         
         STH   RE,TSAR#                                                         
         B     TSARIO10                                                         
TSARIO04 CLI   TSOFFACT,TSADEL     TEST DELETED                                 
         BNE   TSARIO10                                                         
         LH    RE,TSAR#            YES - DECREMENT NUMBER                       
         BCTR  RE,0                                                             
         STH   RE,TSAR#                                                         
*                                                                               
TSARIO10 CLI   TSERRS,0                                                         
         BE    TSARIOX                                                          
         CLI   TSOFFACT,TSARDH     TEST READ-HIGH/NEXT                          
         BE    *+14                                                             
         CLI   TSOFFACT,TSANXT                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSERRS,TSEEOF       RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    EXITL                                                            
         TM    TSERRS,TSERNF       RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    EXITH                                                            
         DC    H'0'                                                             
*                                                                               
TSARIOX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT PHASE NAME TO HEX VALUE                          *         
*   (EG T62131 IS CONVERTED TO X'062131' )                            *         
*                                                                     *         
* NTRY: P1 = A(PHASE NAME)                                            *         
*       P2 = A(HEX VALUE)                                             *         
***********************************************************************         
         SPACE 1                                                                
NUMPHA   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         XC    0(3,R3),0(R3)                                                    
         CLI   0(R2),C'T'                                                       
         BNE   EXITN                                                            
*                                                                               
         MVC   DUB(6),0(R2)                                                     
         MVI   DUB,C'0'                                                         
         GOTO1 VHEXIN,PARM,DUB,(R3),6                                           
         ICM   RF,15,12(R1)                                                     
         BZ    EXITN                                                            
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ADD SUFFIX TO NAME                                                  *         
*                                                                     *         
* NTRY: P1 = L(NAME)                                                  *         
*       P2 = A(NAME)                                                  *         
*       P3 = A(SUFFIX)                                                *         
*       P4 = A(NEW NAME)                                              *         
***********************************************************************         
         SPACE 1                                                                
ADDSUF   NTR1  ,                                                                
         LM    R1,R4,0(R1)                                                      
         MVC   WORK,SPACES                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),0(R2)                                                    
         LA    RF,WORK(R1)                                                      
         LR    R0,R1                                                            
ASUF02   CLI   0(RF),C' '                                                       
         BH    ASUF04                                                           
         BCTR  RF,0                                                             
         BCT   R0,ASUF02                                                        
         DC    H'0'                                                             
*                                                                               
ASUF04   MVC   1(L'TLKSUFF,RF),0(R3)                                            
         EX    R1,*+4                                                           
         MVC   0(0,R4),WORK        SET NEW NAME                                 
         LA    RF,WORK+1(R1)                                                    
         CLI   0(RF),C' '                                                       
         BNE   EXITL               ERROR - PHASE TOO LONG                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET LEVEL/DATE STAMP                                                *         
*                                                                     *         
* NTRY: P1 = L(SEARCH AREA)                                           *         
*       P2 = A(SEARCH AREA)                                           *         
*       P3 = A(CL3 LEVEL NUMBER)                                      *         
*       P4 = A(PL3 CHANGE DATE)                                       *         
***********************************************************************         
         SPACE 1                                                                
GETSTMP  NTR1  ,                                                                
         LM    R1,R4,0(R1)                                                      
         MVC   0(3,R3),=C'???'                                                  
         XC    0(3,R4),0(R4)                                                    
*                                                                               
         LA    RF,0(R1,R2)                                                      
         SH    RF,=Y(10)           RF = A(END OF AREA)                          
         LA    RE,1                RE = 1                                       
*                                                                               
         CLC   =C'LEVEL=',0(R2)    FIND LEVEL STAMP                             
         BE    *+12                                                             
GSTMP02  BXLE  R2,RE,*-10                                                       
         B     EXITN               CC = NOT EQUAL - NOT FOUND                   
*                                                                               
         MVC   WORD(3),6(R2)       TEST LEVEL IS A NUMBER                       
         OC    WORD(3),=C'000'                                                  
         CLC   WORD(3),6(R2)                                                    
         BNE   GSTMP02                                                          
         CLC   =C'DATE=',10(R2)                                                 
         BNE   GSTMP02             TEST FOLLOWED BY DATE=                       
         MVC   0(3,R3),6(R2)                                                    
         GOTO1 VDATCON,PARM,(4,15(R2)),(1,(R4))                                 
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RESET DATASET AREA                                                  *         
***********************************************************************         
         SPACE 1                                                                
CLRDATA  NTR1  ,                                                                
         LA    RE,DATASET                                                       
         LA    RF,L'DATASET                                                     
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   DATASET,DELIM                                                    
         MVI   DATASET+1,DELIM                                                  
         LA    RE,DATASET+1-DATALEN                                             
         STH   RE,DATALEN                                                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ADD DATE TO DATA SET                                                *         
*                                                                     *         
* NTRY: P1 = A(PL3 DATE)                                              *         
***********************************************************************         
         SPACE 1                                                                
ADDDATE  NTR1  ,                                                                
         L     R2,0(R1)                                                         
         MVC   DUB,SPACES                                                       
         OC    0(3,R2),0(R2)                                                    
         BZ    ADDDATEX                                                         
         GOTO1 VDATCON,PARM,(1,(R2)),(10,DUB)                                   
*&&US                                                                           
         XC    DUB(2),DUB+3                                                     
         XC    DUB+3(2),DUB                                                     
         XC    DUB(2),DUB+3                                                     
*&&                                                                             
ADDDATEX GOTO1 ADDDATA,PARM,L'DUB,DUB                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD N BLANK ENTRIES                                      *         
*                                                                     *         
* NTRY: R1 = NUMBER OF BLANKS                                         *         
***********************************************************************         
         SPACE 1                                                                
ADDBLANK NTR1  ,                                                                
         LR    R0,R1                                                            
         GOTO1 ADDDATA,PARM,0,SPACES                                            
         ORG   *-2                                                              
         BASR  RE,RF                                                            
         BCT   R0,*-2                                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ADD ENTRY TO DATA SET                                               *         
*                                                                     *         
* NTRY: P1 = L'ENTRY                                                  *         
*       P2 = A(ENTRY)                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDDATA  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LH    R4,DATALEN                                                       
         LA    R4,DATALEN(R4)                                                   
         BCTR  R4,0                                                             
*                                                                               
         LTR   R2,R2               TEST ZERO LENGTH                             
         BZ    ADATA10                                                          
         LA    RF,0(R2,R3)         FIND END OF INPUT                            
ADATA02  BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BH    ADATA04                                                          
         BCT   R2,ADATA02                                                       
         B     ADATA10                                                          
ADATA04  EX    R2,*+4              COPY ENTRY                                   
         MVC   0(0,R4),0(R3)                                                    
         AR    R4,R2                                                            
*                                                                               
ADATA10  MVI   0(R4),DELIM                                                      
         MVI   1(R4),DELIM                                                      
         LA    R4,2(R4)                                                         
         LA    RE,DATALEN                                                       
         SR    R4,RE                                                            
         STH   R4,DATALEN                                                       
*                                                                               
ADDDATAX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LOADLIB PHASE NAME                                         *         
*                                                                     *         
* EXIT: LIBNAME = PHASE NAME                                          *         
*            CC = NOT EQUAL IF END-OF-FILE                            *         
***********************************************************************         
         SPACE 1                                                                
NXTLIB   NTR1  ,                                                                
         OC    LIBNAME,LIBNAME     TEST FIRST TIME                              
         BNZ   NLIB02                                                           
         OPEN  (LIBDCB,INPUT)                                                   
         B     NLIB04                                                           
NLIB02   CLC   LIBDISP,LIBLEN      TEST NEED NEXT RECORD                        
         BL    NLIB10                                                           
*                                                                               
NLIB04   LA    RF,LIBDIR                                                        
         PRINT GEN                                                              
         READ  NLIB06,SF,LIBDCB,(RF),L'LIBDIR                                   
         CHECK NLIB06                                                           
         MVC   LIBDISP,=AL2(L'LIBLEN)                                           
         PRINT NOGEN                                                            
*                                                                               
NLIB10   LH    R3,LIBDISP                                                       
         LA    R4,LIBDIR(R3)                                                    
         MVC   LIBNAME,0(R4)                                                    
*                                                                               
         LA    R3,L'LIBDATA(R3)    BUMP R3 TO DISP. TO NEXT ENTRY               
         CLI   11(R4),X'B1'        TEST THIS ENTRY AN ALIAS                     
         BNE   *+8                                                              
         LA    R3,10(R3)           YES - BUMP PAST ALIAS DATA                   
         STH   R3,LIBDISP                                                       
*                                                                               
         CLC   LIBNAME,EFFS        TEST END-OF-FILE                             
         BE    EXITN                                                            
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* CLOSE THE LOAD LIBRARY                                              *         
***********************************************************************         
         SPACE 1                                                                
CLOLIB   NTR1  ,                                                                
         CLOSE LIBDCB                                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VLOADER  DC    V(LOADER)                                                        
VPANIC   DC    V(PANIC)                                                         
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VDATCON  DC    V(DATCON)                                                        
         SPACE 1                                                                
TSARBUFL DC    A(TLRECL*1000)      BUFFER LENGTH FOR 1000 RECORDS               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
EFFS     DS    0XL32                                                            
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
         SPACE 1                                                                
READ     DC    C'READ      '                                                    
PAN      DC    C'PAN       '                                                    
CLOSE    DC    C'CLOSE     '                                                    
DIR      DC    C'DIRECTORY '                                                    
NOENTRY  DC    C'NO-ENTRY  '                                                    
         SPACE 1                                                                
EINCARD  DC    C'** INVALID CARD **'                                            
EMICARD  DC    C'** MISSING CARD **'                                            
ISTCARD  DC    C'***** START OF INPUT CARDS *****'                              
IENCARD  DC    C'****** END OF INPUT CARDS ******'                              
         SPACE 1                                                                
TRTTAB   DS    0XL256              ** TRANSLATE TABLE **                        
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'                              
         DC    X'101112131415161718191A1B1C1D1E1F'                              
         DC    X'202122232425262728292A2B2C2D2E2F'                              
         DC    X'303132333435363738393A3B3C3D3E3F'                              
         DC    X'404142434445464748494A4B4C4D4E4F'                              
         DC    X'505152535455565758595A5B5C5D5E5F'                              
         DC    X'606162636465666768696A6B6C6D6E6F'                              
         DC    X'707172737475767778797A7B7C7D7E7F'                              
         DC    X'808182838485868788898A8B8C8D8E8F'                              
         DC    X'909192939495969798999A9B9C9D9E9F'                              
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'                              
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                              
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'                              
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'                              
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                              
         DC    X'000102030405060708090A0B0C0D0E0F'                              
         SPACE 1                                                                
***********************************************************************         
* FILE DCBS                                                           *         
***********************************************************************         
         SPACE 1                                                                
PHAFILE  DCB   DDNAME=PHAFILE,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
         SPACE 1                                                                
LIBDCB   DCB   DSORG=PO,MACRF=(R),DDNAME=STEPLIB                                
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
EOT      EQU   X'00'                                                            
IO1      EQU   X'0000'             TSAR AREA 1                                  
IO2      EQU   X'0100'             TSAR AREA 2                                  
IO3      EQU   X'0200'             TSAR AREA 3                                  
DELIM    EQU   C','                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE POOL                                                *         
***********************************************************************         
         SPACE 1                                                                
DPEA     CSECT                                                                  
WORKAREA DS    XL(WORKL)           LOCAL W/S                                    
*                                                                               
WORKCHN  DS    20000D                                                           
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
PARM     DS    8A                                                               
*                                                                               
ATSAR    DS    A                   A(TSAROFF)                                   
*                                                                               
DUB      DS    D                                                                
WORD     DS    F                                                                
WORK     DS    XL80                                                             
*                                                                               
CARDLN   DS    XL1                                                              
CARD     DS    CL80                                                             
PANBOOK  DS    CL10                                                             
PHASE    DS    CL8                                                              
PHASESUF DS    CL8                                                              
*                                                                               
BOOKTAB  DS    20XL(BOOKTABL)                                                   
BOOKTABN EQU   (*-BOOKTAB)/BOOKTABL                                             
BOOKTABX DS    XL1                                                              
*                                                                               
PARTALN  DS    XL1                                                              
PARTA    DS    XL32                                                             
PARTBLN  DS    XL1                                                              
PARTB    DS    XL32                                                             
*                                                                               
TSAR#    DS    H                   NUMBER OF TSAR RECORDS                       
TSARLST1 DS    XL(TLSTL)           TSAR RECORD AREA 1                           
TSARLST2 DS    XL(TLSTL)           TSAR RECORD AREA 2                           
TSARLST3 DS    XL(TLSTL)           TSAR RECORD AREA 3                           
*                                                                               
TSARBUFF DS    A                   ADDRESS OF GETMAIN'D BUFFER                  
*                                                                               
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
*                                                                               
DATALEN  DS    H                   DATASET LENGTH                               
         DS    H                   N/D                                          
DATASET  DS    XL256               DATASET AREA                                 
DATAL    EQU   *-DATALEN                                                        
*                                                                               
LIBNAME  DS    CL8                                                              
LIBDISP  DS    H                                                                
         DS    0H                                                               
LIBDIR   DS    0XL254                                                           
LIBLEN   DS    H                                                                
LIBDATA  DS    7XL36                                                            
LIBDATAN EQU   (*-LIBDATA)/L'LIBDATA                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* PAN DIRECTORY DSECT                                                 *         
***********************************************************************         
         SPACE 1                                                                
DIRD     DSECT                                                                  
DIRNAME  DS    CL10                PANBOOK NAME                                 
DIRLEVEL DS    CL3                 CURRENT LEVEL                                
DIRUSER  DS    CL4                 USER (?? SOME NUMBER)                        
         DS    CL1                                                              
DIRLANG  DS    CL4                 LANGUAGE TYPE (ASMB/DATA ETC)                
         DS    CL1                                                              
DIRSTAT  DS    CL3                 STATUS                                       
DIRDATM  DS    CL8                 DATE LAST MAINTENED                          
DIRDATA  DS    CL8                 DATE LAST ACCESS                             
DIRBLKS  DS    CL5                 BLOCKS                                       
DIRSTMTS DS    CL8                 STATEMENTS                                   
         DS    CL1                                                              
DIRACT   DS    CL3                 LAST ACTION (UPD/REN ETC.)                   
DIRAVG   DS    CL2                 WHAT'S AVG ??                                
         DS    CL4                 NUMBER ?? USUALLY '0000'                     
DIRNAMER DS    CL10                PANBOOK NAME RIGHT JUSTIFIED                 
         DS    CL2                                                              
         DS    CL1                 ?? USUALLY 'N'                               
DIRL     EQU   *-DIRD                                                           
         SPACE 1                                                                
***********************************************************************         
* BOOK TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
BOOKTABD DSECT                                                                  
BOOKMIN  DS    CL10                MINUMUM BOOK LEVEL                           
BOOKMAX  DS    CL10                MAXIMUM BOOK LEVEL                           
BOOKTABL EQU   *-BOOKTABD                                                       
         SPACE 1                                                                
***********************************************************************         
* TSAR RECORD DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
TLSTD    DSECT                                                                  
TLST     DS    0XL256                                                           
TLNUM    DS    XL2                 TSAR LIST RECORD NUMBER                      
*                                                                               
TLREC    DS    0XL254                                                           
*                                                                               
TLKEY    DS    0XL32                                                            
TLKTYP   DS    XL1                                                              
TLKDIRQ  EQU   X'01'               PAN DIRECTORY LIST                           
TLKPHAQ  EQU   X'02'               PHASE BOOK                                   
TLKCATQ  EQU   X'03'               CATALP BOOK                                  
TLKOTHQ  EQU   X'04'               OTHER BOOK                                   
TLKSCRQ  EQU   X'05'               SCREEN BOOK                                  
TLKLIBQ  EQU   X'06'               LOADLIB BOOK                                 
TLKPHASE DS    CL8                 PHASE NAME                                   
TLKBOOK  DS    CL10                PAN BOOK NAME                                
TLKSUFF  DS    CL8                 PHASE/PAN BOOK SUFFIX                        
         ORG   TLKEY+L'TLKEY                                                    
TLKEYL   EQU   *-TLKEY                                                          
*                                                                               
TLDATA   DS    0XL222                                                           
TLBDATA  DS    0XL20                                                            
TLBNAME  DS    CL10                PAN BOOK NAME                                
TLBNAMEL DS    CL1                 PAN BOOK NAME LENGTH (EXECUTABLE)            
TLBLEVEL DS    CL3                 PAN BOOK LEVEL                               
TLBDATE  DS    PL3                 PAN BOOK LAST CHANGE DATE                    
         DS    XL(L'TLBDATA-(*-TLBDATA))                                        
*                                                                               
TLOTHER  DS    0X                  OTHER DATA DEFINED HERE                      
         ORG   TLDATA+L'TLDATA                                                  
TLRECL   EQU   *-TLREC                                                          
TLSTL    EQU   *-TLSTD                                                          
         SPACE 1                                                                
         ORG   TLOTHER             ** PHASE BOOK LIST  **                       
TLPNAME  DS    CL8                 PHASE NAME                                   
TLPNUM   DS    XL3                 PHASE NUMBER                                 
TLPLEVEL DS    CL3                 PHASE LEVEL                                  
TLPDATE  DS    PL3                 PHASE LAST CHANGE DATE                       
         SPACE 1                                                                
         ORG   TLOTHER             ** CATALP BOOK LIST **                       
TLCNAME  DS    CL8                 CATALP NAME                                  
TLCLEVEL DS    CL3                 CATALP LEVEL                                 
TLCDATE  DS    PL3                 CATALP LAST CHANGE DATE                      
         SPACE 1                                                                
         ORG   TLOTHER             ** SCREEN BOOK LIST **                       
TLSDNAME DS    CL10                SCREEN DSECT BOOK NAME                       
TLSDLEV  DS    CL3                 SCREEN DSECT LEVEL                           
TLSDDATE DS    PL3                 SCREEN DSECT DATE                            
TLSNAME  DS    CL8                 SCREEN PHASE NAME                            
TLSNUM   DS    XL3                 SCREEN PHASE NUMBER                          
         SPACE 1                                                                
         ORG   TLDATA              ** LOADLIB **                                
TLDSUF   DS    20XL8               SUFFIXES                                     
TLDSUFN  EQU   (*-TLDSUF)/L'TLDSUF                                              
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         ORG   P                   ** PHASE PRINT LINE **                       
PPBNAME  DS    CL10                                                             
         DS    CL2                                                              
PPBLEVEL DS    CL3                                                              
         DS    CL2                                                              
PPBDATE  DS    CL8                                                              
         DS    CL5                                                              
PPPNAME  DS    CL8                                                              
         DS    CL2                                                              
PPPLEVEL DS    CL3                                                              
         DS    CL2                                                              
PPPDATE  DS    CL8                                                              
         ORG   P                   ** SCREEN PRINT LINE **                      
PSBNAME  DS    CL10                                                             
         DS    CL2                                                              
PSBLEVEL DS    CL3                                                              
         DS    CL2                                                              
PSBDATE  DS    CL8                                                              
         DS    CL5                                                              
PSDNAME  DS    CL10                                                             
         DS    CL2                                                              
PSDLEVEL DS    CL3                                                              
         DS    CL2                                                              
PSDDATE  DS    CL8                                                              
         DS    CL5                                                              
PSPNAME  DS    CL8                                                              
         PRINT ON                                                               
         SPACE 1                                                                
*DDTSARD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040DDTDPEA   09/25/96'                                      
         END                                                                    

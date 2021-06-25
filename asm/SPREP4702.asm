*          DATA SET SPREP4702  AT LEVEL 038 AS OF 09/01/05                      
*PHASE SP4702A                                                                  
*INCLUDE DLFLD                                                                  
                                                                                
*===============================================================                
* CREATE A DOWNLOAD OUTPUT FILE IF OUTPUT TYPE=DOWN                             
*===============================================================                
                                                                                
         TITLE 'SP4702 - SPOTPAK PRDGRP/MKTGRP LISTING'                         
SP4702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4702                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LR    RC,RB                                                            
         AHI   RC,4096                                                          
         USING SP4702+4096,RC                                                   
*                                                                               
         L     RE,=A(SP47R9)                                                    
         STM   R9,RC,0(RE)                                                      
*                                                                               
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
         CLI   MODE,PGR1FRST                                                    
         BE    PGR1F                                                            
         CLI   MODE,PGR2FRST                                                    
         BE    PGR2F                                                            
         CLI   MODE,PGR3FRST                                                    
         BE    PGR3F                                                            
         CLI   MODE,MGR1FRST                                                    
         BE    MGR1F                                                            
         CLI   MODE,MGR2FRST                                                    
         BE    MGR2F                                                            
         CLI   MODE,MGR3FRST                                                    
         BE    MGR3F                                                            
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
                                                                                
*============================================================                   
* RUNFRST                                                                       
*============================================================                   
                                                                                
         L     RE,=A(SP47HL)                                                    
         ST    RE,HEADHOOK                                                      
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*============================================================                   
* REQFRST                                                                       
*============================================================*                  
         SPACE 1                                                                
REQF     MVI   DOWNLOAD,C'N'                                                    
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         L     RF,MCVREMOT         GET REMOTED ADDRESS                          
         USING REMOTED,RF                                                       
         TM    REMOTTYP,X'18'      TEST OUTPUT TYPE = DOWN OR SQL               
         BZ    *+8                                                              
         MVI   DOWNLOAD,C'Y'                                                    
         DROP  RE,RF                                                            
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         JNE   EXIT                                                             
                                                                                
*============================================================                   
* OPEN A REPORT ON THE PRTQUE AND INITALIZE FOR DOWNLOADING                     
*============================================================                   
                                                                                
         L     RE,=A(DLCB)                                                      
         XC    0(256,RE),0(RE)                                                  
D        USING DLCBD,DLCB                                                       
*                                                                               
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
*                                                                               
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A'   LOAD EDITOR                                  
*                                                                               
         MVI   BYTE,QEDITOR                                                     
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
*                                                                               
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
*                                                                               
         MVC   THISMED,QMED        SAVE MEDIA CODE FOR DOWNLOAD                 
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* AT REQLAST, PUT OUT EOR FOR DOWNLOAD OPTION                                   
*=================================================================              
         SPACE 1                                                                
REQL     CLI   DOWNLOAD,C'Y'       TEST DOWNLOADING                             
         BNE   EXIT                NO                                           
         L     R1,=A(DLCB)                                                      
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),(R1)                                                   
         J     EXIT                                                             
         DROP  D                                                                
         EJECT                                                                  
PGR1F    MVI   RCSUBPRG,1                                                       
         MVC   SVP1ADD1,SPACES                                                  
         MVC   SVP1ADD2,SPACES                                                  
         MVC   SVP1ADD3,SPACES                                                  
         MVC   SVP1ADD4,SPACES                                                  
         L     R2,ADPRDGRP         POINT TO PRD GRP REC                         
         USING PRGEL20,R2                                                       
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'20'        GET ADDRESS ELEMENT                          
         BRAS  RE,NEXTEL                                                        
         BNE   PGR1F10                                                          
         MVC   SVP1ADD1,PRGADDR1                                                
         MVC   SVP1ADD2,PRGADDR2                                                
         MVC   SVP1ADD3,PRGADDR3                                                
         MVC   SVP1ADD4,PRGADDR4                                                
*                                                                               
PGR1F10  MVC   THISGRP1(5),PGR1                                                 
         MVI   THISGRP1+5,C' '                                                  
         MVC   THISNAM1,PGR1NM                                                  
         MVC   THISBRK1,PGR1BK                                                  
         B     SET1                                                             
*                                                                               
MGR1F    MVI   RCSUBPRG,2                                                       
         MVC   THISGRP1,MGR1N                                                   
         MVC   THISNAM1,MGR1NM                                                  
         MVC   THISBRK1,MGR1BK                                                  
         MVC   THISCLT,CLT         SET NAMES FOR DOWNLOAD                       
         OC    CLT,CLT                                                          
         BNZ   *+10                                                             
         MVC   THISCLT,=C'ALL'                                                  
         MVC   THISCLNM,CLTNM                                                   
*                                                                               
SET1     DS    0H                                                               
         MVC   THISGRP2,SPACES                                                  
         MVC   THISNAM2,SPACES                                                  
         MVC   THISBRK2,SPACES                                                  
*                                                                               
         MVC   THISGRP3,SPACES                                                  
         MVC   THISNAM3,SPACES                                                  
         MVC   THISBRK3,SPACES                                                  
*                                                                               
         MVC   SVLN1,SPACES                                                     
         MVC   SVLN2,SPACES                                                     
         MVC   SVLN3,SPACES                                                     
         MVC   SVLN4,SPACES                                                     
* SET BREAK TITLES IN HEADLINES                                                 
         MVC   H7,SPACES                                                        
         MVC   H8,SPACES                                                        
         MVC   H7+1(12),THISBRK1                                                
         GOTO1 UNDERLIN,DMCB,(12,H7+1),H8+1                                     
*                                                                               
         MVC   H7+34(7),=CL7'PRODUCT'                                           
         CLI   RCSUBPRG,1                                                       
         BE    *+10                                                             
         MVC   H7+34(7),=CL7'MARKET'                                            
         GOTO1 (RF),(R1),(12,H7+34),H8+34                                       
*                                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   SET1A                                                            
         MVC   H7+99(8),=CL8'ADDRESS'                                           
         GOTO1 (RF),(R1),(8,H7+99),H8+99                                        
*                                                                               
SET1A    MVC   SVH7,H7                                                          
         MVC   SVH8,H8                                                          
*                                                                               
         LA    R0,P+34                                                          
         ST    R0,ADDATA           SAVE DATA ADDRESS                            
*                                                                               
         MVI   USRSW1,0            INDICATE NAMES NOT PRINTED                   
         MVC   SVLN1+1(6),THISGRP1   GROUP CODE                                 
         MVC   SVLN1+8(24),THISNAM1  GROUP NAME                                 
         GOTO1 SQUASHER,DMCB,SVLN1+1,31                                         
         CLI   RCSUBPRG,1          ONLY FOR PRD GRP                             
         BNE   SET1B                                                            
         MVC   SVLN1+99(30),SVP1ADD1                                            
         MVC   SVLN2+99(30),SVP1ADD2                                            
         MVC   SVLN3+99(30),SVP1ADD3                                            
         MVC   SVLN4+99(30),SVP1ADD4                                            
*                                                                               
SET1B    BAS   RE,GETMACC          GET LMT ACC CODE                             
         OC    LMTACC,LMTACC                                                    
         BZ    EXIT                                                             
         CLI   DOWNLOAD,C'Y'       TEST DOWNLOAD                                
         BE    EXIT                                                             
*                                                                               
         LA    R5,SVLN1+1                                                       
         LA    R6,SVLN2+1                                                       
*                                                                               
SET1C    CLI   0(R5),C' '                                                       
         BE    SET1D                                                            
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         B     SET1C                                                            
*                                                                               
SET1D    MVC   1(14,R6),=C'LMT ACC CODE ='                                      
         MVC   16(3,R6),LMTACC                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PGR2F    MVC   THISGRP2(5),PGR2                                                 
         MVC   THISBRK2,PGR2BK                                                  
         MVC   THISNAM2,PGR2NM                                                  
*                                                                               
         MVC   SVP2ADD1,SPACES                                                  
         MVC   SVP2ADD2,SPACES                                                  
         MVC   SVP2ADD3,SPACES                                                  
         MVC   SVP2ADD4,SPACES                                                  
         L     R2,ADPRDGRP         POINT TO PRD GRP REC                         
         USING PRGEL20,R2                                                       
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'20'        GET ADDRESS ELEMENT                          
         BAS   RE,NEXTEL                                                        
         BNE   PGR2F10                                                          
         MVC   SVP2ADD1,PRGADDR1                                                
         MVC   SVP2ADD2,PRGADDR2                                                
         MVC   SVP2ADD3,PRGADDR3                                                
         MVC   SVP2ADD4,PRGADDR4                                                
*                                                                               
PGR2F10  B     SET2                                                             
*                                                                               
MGR2F    MVC   THISGRP2,MGR2N                                                   
         MVC   THISBRK2,MGR2BK                                                  
         MVC   THISNAM2,MGR2NM                                                  
*                                                                               
SET2     DS    0H                                                               
         MVC   SVLN2,SPACES                                                     
*                                                                               
         MVC   H7+34(12),THISBRK2                                               
         GOTO1 UNDERLIN,DMCB,(12,H7+34),H8+34                                   
*                                                                               
         MVC   H7+67(7),=CL7'PRODUCT'                                           
         CLI   RCSUBPRG,1                                                       
         BE    *+10                                                             
         MVC   H7+67(7),=CL7'MARKET'                                            
         GOTO1 (RF),(R1),(12,H7+67),H8+67                                       
*                                                                               
         MVC   SVH7+34(98),H7+34                                                
         MVC   SVH8+34(98),H8+34                                                
*                                                                               
         LA    R0,P+67                                                          
         ST    R0,ADDATA                                                        
*                                                                               
         MVI   USRSW1,0            INDICATE NAMES NOT PRINTED                   
         MVC   SVLN1+34(98),SPACES                                              
         MVC   SVLN1+34(6),THISGRP2   GROUP CODE                                
         MVC   SVLN1+41(24),THISNAM2  GROUP NAME                                
         GOTO1 SQUASHER,DMCB,SVLN1+34,31                                        
         CLI   RCSUBPRG,1          ONLY FOR PRD GRP                             
         BNE   SET2B                                                            
         MVC   SVLN1+99(30),SVP2ADD1                                            
         MVC   SVLN2+99(30),SVP2ADD2                                            
         MVC   SVLN3+99(30),SVP2ADD3                                            
         MVC   SVLN4+99(30),SVP2ADD4                                            
*                                                                               
SET2B    BAS   RE,GETMACC          GET LMT ACC CODE                             
         OC    LMTACC,LMTACC                                                    
         BZ    EXIT                                                             
         LA    R5,SVLN1+34                                                      
         LA    R6,SVLN2+34                                                      
*                                                                               
SET2C    CLI   0(R5),C' '                                                       
         BE    SET2D                                                            
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         B     SET2C                                                            
*                                                                               
SET2D    MVC   1(14,R6),=C'LMT ACC CODE ='                                      
         MVC   16(3,R6),LMTACC                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PGR3F    MVC   THISGRP3,PGR3                                                    
         MVC   THISBRK3,PGR3BK                                                  
         MVC   THISNAM3,PGR3NM                                                  
*                                                                               
         MVC   SVP3ADD1,SPACES                                                  
         MVC   SVP3ADD2,SPACES                                                  
         MVC   SVP3ADD3,SPACES                                                  
         MVC   SVP3ADD4,SPACES                                                  
         L     R2,ADPRDGRP         POINT TO PRD GRP REC                         
         USING PRGEL20,R2                                                       
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'20'        GET ADDRESS ELEMENT                          
         BAS   RE,NEXTEL                                                        
         BNE   PGR3F10                                                          
         MVC   SVP3ADD1,PRGADDR1                                                
         MVC   SVP3ADD2,PRGADDR2                                                
         MVC   SVP3ADD3,PRGADDR3                                                
         MVC   SVP3ADD4,PRGADDR4                                                
*                                                                               
PGR3F10  B     SET3                                                             
*                                                                               
MGR3F    MVC   THISGRP3,MGR3                                                    
         MVC   THISBRK3,MGR3BK                                                  
         MVC   THISNAM3,MGR3NM                                                  
*                                                                               
SET3     DS    0H                                                               
         MVC   SVLN2,SPACES                                                     
*                                                                               
         MVC   H7+67(12),THISBRK3                                               
         GOTO1 UNDERLIN,DMCB,(12,H7+67),H8+67                                   
*                                                                               
         MVC   H7+99(7),=CL7'PRODUCT'                                           
         CLI   RCSUBPRG,1                                                       
         BE    *+10                                                             
         MVC   H7+99(7),=CL7'MARKET'                                            
         GOTO1 (RF),(R1),(12,H7+99),H8+99                                       
*                                                                               
         MVC   SVH7+67(65),H7+67                                                
         MVC   SVH8+67(65),H8+67                                                
*                                                                               
         LA    R0,P+99                                                          
         ST    R0,ADDATA                                                        
*                                                                               
         MVI   USRSW1,0            INDICATE NAMES NOT PRINTED                   
         MVC   SVLN1+67(65),SPACES                                              
         MVC   SVLN1+67(6),THISGRP3   GROUP CODE                                
         MVC   SVLN1+74(24),THISNAM3  GROUP NAME                                
         GOTO1 SQUASHER,DMCB,SVLN1+67,31                                        
         CLI   RCSUBPRG,1          ONLY FOR PRD GRP                             
         BNE   SET3B                                                            
         MVC   SVLN1+99(30),SVP3ADD1                                            
         MVC   SVLN2+99(30),SVP3ADD2                                            
         MVC   SVLN3+99(30),SVP3ADD3                                            
         MVC   SVLN4+99(30),SVP3ADD4                                            
*                                                                               
SET3B    BAS   RE,GETMACC                                                       
         OC    LMTACC,LMTACC                                                    
         BZ    EXIT                                                             
         LA    R5,SVLN1+67                                                      
         LA    R6,SVLN2+67                                                      
*                                                                               
SET3C    CLI   0(R5),C' '                                                       
         BE    SET3D                                                            
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         B     SET3C                                                            
SET3D    MVC   1(14,R6),=C'LMT ACC CODE ='                                      
         MVC   16(3,R6),LMTACC                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PRDF     CLI   USRSW1,0            TEST NAMES PRINTED                           
         BNE   PRDF2                                                            
         BAS   RE,PRTIT            SKIP A LINE                                  
         MVC   P,SVLN1             NO-PRINT ON THIS LINE                        
         MVC   P2,SVLN2                                                         
         MVC   P3,SVLN3                                                         
         MVC   P4,SVLN4                                                         
PRDF2    L     RE,ADDATA                                                        
         MVC   0(3,RE),PRD                                                      
         MVC   4(24,RE),PRDNM                                                   
         BAS   RE,PRTIT                                                         
         MVI   USRSW1,1                                                         
         B     EXIT                                                             
*                                                                               
MKTF     CLI   USRSW1,0                                                         
         BNE   MKTF2                                                            
         BAS   RE,PRTIT            SKIP A LINE                                  
         MVC   P,SVLN1                                                          
         MVC   P2,SVLN2                                                         
MKTF2    L     RE,ADDATA                                                        
         MVC   0(4,RE),MKT                                                      
         MVC   5(24,RE),MKTNM                                                   
         MVC   THISMKT,MKT         SAVE FOR DOWNLOAD                            
         MVC   THISMKNM,MKTNM                                                   
         CLI   DOWNLOAD,C'Y'       TEST DOWNLOAD                                
         BE    MKTF10                                                           
         BAS   RE,PRTIT                                                         
         MVI   USRSW1,1                                                         
         B     EXIT                                                             
*                                                                               
MKTF10   L     R4,=A(DRECTAB)                                                   
         BRAS  RE,OUTPUT                                                        
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
GETMACC  NTR1                                                                   
         XC    LMTACC,LMTACC                                                    
         L     R2,ADMKTGRP                                                      
         LA    R2,24(R2)                                                        
         CLI   0(R2),X'20'         FIND LMT ACC ELEM                            
         BE    GETM2                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETMX                                                            
*                                                                               
GETM2    MVC   LMTACC,2(R2)                                                     
         B     GETMX                                                            
*                                                                               
GETMX    XIT1                                                                   
         EJECT                                                                  
*===============================================================                
* INTERCEPT CALLS TO REPORT TO SUPPRESS PRINTING ON DOWNLOADS                   
*===============================================================                
                                                                                
PRTIT    NTR1                                                                   
         CLI   DOWNLOAD,C'Y'       TEST DOWNLOADING                             
         BE    PRTIT2              YES                                          
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
PRTIT2   MVC   P,SPACES            SUPPRESS PRINT LINES FOR NOW                 
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         J     EXIT                                                             
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF REC                                   
         JE    NEXTELX                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         J     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT EQU                       
         EJECT                                                                  
*================================================================               
* HEADLINE HOOK ROUTINE                                                         
*================================================================               
                                                                                
         DS    0D                                                               
SP47HL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R9,RA,SP47R9                                                     
         L     RC,SP47RC                                                        
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   SP47H2                                                           
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         J     EXIT                                                             
*                                                                               
SP47H2   MVC   H7,SVH7                                                          
         MVC   H8,SVH8                                                          
*                                                                               
         CLI   USRSW1,0                                                         
         JE    EXIT                                                             
         MVC   MID1(11),=C'(CONTINUED)'                                         
* MOVE GROUP NAMES TO P                                                         
         LA    RE,P                                                             
         L     RF,ADDATA                                                        
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),SVLN1 **EXECUTED**                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P2(0),SVLN2         * EXECUTED *                                 
         J     EXIT                                                             
*                                                                               
SP47R9   DC    F'0'                                                             
SP47RA   DC    F'0'                                                             
SP47RB   DC    F'0'                                                             
SP47RC   DC    F'0'                                                             
         EJECT                                                                  
*                                                                               
* THESE FIELDS USED TO GET WIDE PRINT LINE OVERRIDES                            
* THEY GET MOVED INTO THE DLCB TO OVERRIDE MAXLINE                              
*                                                                               
                                                                                
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
DOWNLOAD DC    C'N'                                                             
*                                                                               
         DS    0D                                                               
THISGRP1 DS    CL6                                                              
THISGRP2 DS    CL6                                                              
THISGRP3 DS    CL6                                                              
*                                                                               
THISNAM1 DS    CL24                                                             
THISNAM2 DS    CL24                                                             
THISNAM3 DS    CL24                                                             
*                                                                               
THISBRK1 DS    CL12                                                             
THISBRK2 DS    CL12                                                             
THISBRK3 DS    CL12                                                             
*                                                                               
THISMKT  DS    CL4                                                              
THISMKNM DS    CL24                                                             
THISMED  DS    C                                                                
THISCLT  DS    CL3                                                              
THISCLNM DS    CL24                                                             
*                                                                               
LMTACC   DS    CL3                 LMT ACCESS CODE                              
ELCODE   DS    CL1                                                              
*                                                                               
SVP1ADD1 DC    CL30' '                                                          
SVP1ADD2 DC    CL30' '                                                          
SVP1ADD3 DC    CL30' '                                                          
SVP1ADD4 DC    CL30' '                                                          
SVP2ADD1 DC    CL30' '                                                          
SVP2ADD2 DC    CL30' '                                                          
SVP2ADD3 DC    CL30' '                                                          
SVP2ADD4 DC    CL30' '                                                          
SVP3ADD1 DC    CL30' '                                                          
SVP3ADD2 DC    CL30' '                                                          
SVP3ADD3 DC    CL30' '                                                          
SVP3ADD4 DC    CL30' '                                                          
*                                                                               
SVLN1    DC    CL132' '                                                         
SVLN2    DC    CL132' '                                                         
SVLN3    DC    CL132' '                                                         
SVLN4    DC    CL132' '                                                         
SVH7     DC    CL132' '                                                         
SVH8     DC    CL132' '                                                         
ADDATA   DC    A(0)                                                             
         DS    0D                                                               
DLCB     DS    XL256                                                            
         LTORG                                                                  
         EJECT                                                                  
*=============================================================*                 
* OUTPUT  DATA TO PRTQUE REPORT                                                 
*=============================================================*                 
                                                                                
                                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,P                BLANK PRINT LINES THAT ARE USED              
         LHI   R0,4                                                             
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
*                                                                               
         L     R2,=A(DLCB)                                                      
         USING DLCBD,R2                                                         
*                                                                               
OUTPUT2  L     RE,0(R4)            GET DATA ADDR                                
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         CLI   5(R4),C'T'          TEST TEXT                                    
         BNE   OUTPUT6                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    DLCBFLD(0),SPACES                                                
**NOP**  CLI   ?????,C'Y'          TEST FIXED LENGTH OUTPUT                     
**NOP**  BNE   *+10                                                             
**NOP**  MVC   DLCBLEN,4(R4)       FIX OUTPUT LENGTH                            
         B     OUTPUT18                                                         
*                                                                               
OUTPUT6  CLI   5(R4),C'B'          TEST BINARY                                  
         BNE   OUTPUT14                                                         
**NOP**  CLI   ?????,C'Y'          TEST FIXED LENGTH OUTPUT                     
**NOP**  BE    OUTPUT8                                                          
         MVC   DLCBNDP,7(R4)       SET NUMBER OF DECIMAL PLACES                 
         MVC   DLCBLEN,4(R4)       SET DATE LENGTH                              
         B     OUTPUT20                                                         
*                                                                               
* FIXED LEN NUMERIC OUTPUT                                                      
*                                                                               
OUTPUT8  ICM   R0,15,0(RE)         GET VALUE IN R0                              
         MVI   DLCBTYP,C'N'        TYPE=NUMERIC                                 
*                                                                               
         CLI   7(R4),1             TEST 1 DECIMAL                               
         BNE   OUTPUT10                                                         
         MVC   WORK(11),=X'4021202020202020204B20'                              
         CVD   R0,DUB                                                           
         ED    WORK(11),DUB+3                                                   
         MVC   DLCBFLD(8),WORK+3                                                
         MVI   DLCBLEN,8           FIX OUTPUT LEN                               
         B     OUTPUT30                                                         
*                                                                               
OUTPUT10 CLI   7(R4),2             TEST 2 DECIMAL                               
         BNE   OUTPUT13                                                         
*                                                                               
         MVC   WORK(17),=X'40212020202020202020202020204B2020'                  
         LA    R1,DUB                                                           
         LTR   R0,R0                                                            
         BNM   OUTPUT11                                                         
         MVC   WORK(17),=X'404021202020202020202020204B202060'                  
         LA    R1,DUB+1                                                         
*                                                                               
OUTPUT11 CVD   R0,DUB                                                           
         ED    WORK(17),DUB                                                     
         MVC   DLCBFLD(13),WORK+4                                               
         MVI   DLCBLEN,13                                                       
         B     OUTPUT30                                                         
*                                                                               
OUTPUT13 DC    H'0'                                                             
*                                                                               
OUTPUT14 TM    7(R4),X'01'         TEST CVD REQUIRED                            
         BZ    OUTPUT16                                                         
         ICM   R0,15,0(RE)         GET DATA VALUE                               
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         SLL   RF,4                SET LEN TO UNPK TO                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  DLCBFLD(0),DUB                                                   
         B     OUTPUT20                                                         
*                                                                               
OUTPUT16 CLI   5(R4),C'X'          TEST HEX                                     
         BNE   OUTPUT18                                                         
**NOP**  CLI   ?????,C'Y'          TEST FIXED LENGTH OUTPUT                     
**NOP**  BNE   *+10                                                             
**NOP**  MVC   DLCBLEN,4(R4)       FIX OUTPUT LENGTH                            
*                                                                               
OUTPUT18 CLI   6(R4),0             TEST FIELD CAN END RECORD                    
         BE    OUTPUT20            NO                                           
         CLC   DLCBFLD(1),6(R4)    ELSE COMPARE                                 
         BNH   OUTPUT32            AND POSSIBLY END                             
*                                                                               
OUTPUT20 MVC   DLCBTYP(1),5(R4)                                                 
         CLI   5(R4),C'X'          TEST HEX OUTPUT                              
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'        TELL DLFLD IT'S TEXT                         
*                                                                               
OUTPUT30 MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
         MVI   DLCXDELC,C' '       ALWAYS RESTORE TERMINATOR                    
*                                                                               
         LA    R4,L'RECTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT2                                                          
*                                                                               
OUTPUT32 MVI   DLCBACT,DLCBEOL                                                  
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
                                                                                
BLPRINT  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         CLI   DOWNFRST,C'Y'       TEST FIRST PRINT LINE                        
         BNE   BLPRINT2                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   DOWNFRST,C'N'                                                    
*                                                                               
BLPRINT2 GOTO1 REPORT                                                           
         J     EXIT                                                             
DOWNFRST DC    C'Y'                                                             
         LTORG                                                                  
         EJECT                                                                  
* ENTRIES ARE                                                                   
* AL4(DATA)                                                                     
* AL1(L'DATA)                                                                   
* CL1'TYPE'                                                                     
* C'  '            IF NOT X'00' EOR IF FIELD NOT > THIS VALUE                   
* X'01'            CONVERT THE FIELD TO DECIMAL BEFORE WRITE                    
* OR IF TYPE=B,    LAST BYTE IS NUMBER OF DECIMAL PLACES                        
         SPACE 1                                                                
         DS    0D                                                               
RECTAB   DS    0XL8                                                             
         DC    CL8'DRECTAB*'                                                    
DRECTAB  DS    0XL8                                                             
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISCLNM),AL1(L'THISCLNM),C'T',2X'00'                        
         DC    AL4(THISGRP1),AL1(L'THISGRP1),C'T',2X'00'                        
         DC    AL4(THISNAM1),AL1(L'THISNAM1),C'T',2X'00'                        
         DC    AL4(THISGRP2),AL1(L'THISGRP2),C'T',2X'00'                        
         DC    AL4(THISNAM2),AL1(L'THISNAM2),C'T',2X'00'                        
         DC    AL4(THISGRP3),AL1(L'THISGRP3),C'T',2X'00'                        
         DC    AL4(THISNAM3),AL1(L'THISNAM3),C'T',2X'00'                        
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISMKNM),AL1(L'THISMKNM),C'T',2X'00'                        
         DC    X'FF'                                                            
*                                                                               
       ++INCLUDE SPGENPRG                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
 END                                                                            

*          DATA SET SPSFM1B    AT LEVEL 019 AS OF 10/24/11                      
*PHASE T2171BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2171B - ESTIMATE HEADER LIST                         *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS REPORT ACTION ONLY (ON-SCREEN OR PRINTED)    *         
*                                                                     *         
*  INPUTS       SCREEN T217DB (REPORT)                                *         
*                                                                     *         
*  OUTPUTS      PRINTED OR LISTED REPORT                              *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- PRINT LINE                                      *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - GENERAL USAGE                                   *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2171B  ESTIMATE HEADER LIST PROGRAM'                           
T2171B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2171B                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         LIST RECORDS ONLINE                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
NEQXIT   LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       TM    WHEN,X'7F'          TEST SPOOLED REPORT                          
         BNZ   VK5                 YES                                          
         OI    WHENOK,X'01'        DO SPECIAL SEQUENCE FOR ON-SCREEN            
*                                                                               
         TM    SFMMEDH+4,X'20'     TEST KEY FIELDS ARE VALIDATED                
         BZ    VK5                 NO                                           
         TM    SFMCLTH+4,X'20'                                                  
         BZ    VK5                                                              
         TM    SFMPRDH+4,X'20'                                                  
         BZ    VK5                                                              
         TM    SFMESTH+4,X'20'                                                  
         BZ    VK5                                                              
         TM    SFMDTSH+4,X'20'                                                  
         BZ    VK5                                                              
         TM    SFMOPTSH+4,X'20'                                                 
         BZ    VK5                                                              
*                                                                               
         OC    SVKEY,SVKEY         TEST WE'RE IN THE MIDDLE OF A LIST           
         BNZ   VKX                 YES - DO NOTHING                             
*                                                                               
VK5      XC    SVKEY,SVKEY                                                      
         XC    AFLDH,AFLDH                                                      
         MVI   CLTSW,C'Y'                                                       
         MVI   PRDSW,C'Y'                                                       
         MVI   ESTFOUND,C'N'                                                    
*                                                                               
         LA    R2,SFMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   SVKEY+1(1),BAGYMD                                                
*                                                                               
         LA    R2,SFMCLTH          CLIENT                                       
         CLI   T217FFD+6,C'+'      TEST MARKET LIMIT ACCESS                     
         BE    *+12                YES - ALLOW CLT=ALL                          
         CLI   T217FFD+6,0         TEST ANY OTHER SECURITY LIMIT                
         BNE   VK7                 YES - CAN'T SAY ALL                          
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK7                                                              
         MVC   QCLT,=C'ALL'                                                     
         B     VK9                                                              
*                                                                               
VK7      MVC   QCLT,8(R2)                                                       
         CLI   8(R2),C'*'          TEST OFFICE                                  
         BE    VK9                                                              
         CLI   8(R2),C'$'                                                       
         BE    VK9                                                              
         GOTO1 VALICLT                                                          
*                                                                               
VK9      TM    WHEN,X'40'          TEST NOW REPORT                              
         BNO   VK10                                                             
         CLC   =C'ALL',8(R2)       NO ALL CLT NOW REPORTS                       
         BNE   VK10                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VK10     LA    R2,SFMPRDH          PRODUCT                                      
         CLC   =C'ALL',8(R2)                                                    
         BNE   *+14                                                             
         MVC   QPRD,=C'ALL'                                                     
         B     VK30                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLC   =C'ALL',QCLT                                                     
         BNE   VK20                                                             
         CLC   =C'POL',8(R2)                                                    
         BNE   TRAPERR                                                          
         MVC   QPRD,8(R2)                                                       
         B     VK30                                                             
*                                                                               
VK20     GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
*                                                                               
VK30     LA    R2,SFMESTH          ESTIMATE                                     
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VK40                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK40                                                             
         MVI   QBEST,1                                                          
         MVI   QBESTEND,255                                                     
         B     VK50                                                             
*                                                                               
VK40     XC    BLOCK(64),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVI   ERROR,MISSING                                                    
         CLI   4(R1),0                                                          
         BE    TRAPERR                                                          
         MVI   ERROR,INVALID                                                    
         LA    R7,BLOCK                                                         
         ICM   R0,15,4(R7)         GET FIRST NUMERIC VALUE                      
         BZ    TRAPERR                                                          
         CH    R0,=H'255'                                                       
         BH    TRAPERR                                                          
         STC   R0,QBEST                                                         
         STC   R0,QBESTEND                                                      
*                                                                               
         CLI   1(R7),0             TEST FOR SECOND INPUT FIELD                  
         BZ    VK50                                                             
         ICM   R0,15,8(R7)                                                      
         BZ    TRAPERR                                                          
         CH    R0,=H'255'                                                       
         BH    TRAPERR                                                          
         STC   R0,QBESTEND                                                      
         CLC   QBEST,QBESTEND                                                   
         BNL   TRAPERR                                                          
*                                                                               
VK50     LA    R2,SFMDTSH          DATES                                        
         XC    QSTART,QSTART                                                    
         MVC   QEND,XFF                                                         
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         XC    BLOCK(64),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R7,BLOCK                                                         
         MVI   ERROR,MISSING                                                    
         CLI   0(R7),0                                                          
         BE    TRAPERR                                                          
         GOTO1 DATVAL,DMCB,12(R7),QSTART                                        
         MVI   ERROR,INVALID                                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERR                                                          
         MVC   QEND,QSTART         SET START=END                                
*                                                                               
         CLI   1(R7),0             TEST FOR END DATE                            
         BE    VK60                                                             
         GOTO1 DATVAL,DMCB,22(R7),QEND                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERR                                                          
         CLC   QSTART,QEND                                                      
         BNL   TRAPERR                                                          
*                                                                               
VK60     MVI   DSSW,C'N'           ASSUME NO DOUBLE SPACE                       
         LA    R2,SFMOPTSH         VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VK70                THERE ARE NONE                               
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             TEST 'S=2' OPTION                            
         BNE   TRAPERR                                                          
         CLC   =C'S=2',8(R2)                                                    
         BNE   TRAPERR             NO                                           
         CLI   WHEN,X'80'          TEST ON-SCREEN REPORT                        
         BE    TRAPERR             YES - CAN'T DOUBLE SPACE ON-SCREEN           
         MVI   DSSW,C'Y'           DOUBLE SPACE                                 
*                                                                               
VK70     OI    SFMMEDH+4,X'20'     FLAG KEY FIELDS AS VALIDATED                 
         OI    SFMCLTH+4,X'20'                                                  
         OI    SFMPRDH+4,X'20'                                                  
         OI    SFMESTH+4,X'20'                                                  
         OI    SFMDTSH+4,X'20'                                                  
         OI    SFMOPTSH+4,X'20'                                                 
*                                                                               
VKX      CLI   MODE,PRINTREP       PRINTING REPORT?                             
         BE    EXIT                YES - NO SCREEN TO CLEAR!                    
         LA    R2,SFMHD1H                                                       
         BAS   RE,CLRSCRN          CLEAR LIST FIELDS                            
         B     EXIT                                                             
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       LA    R1,HEDSPECS         SET UP SPECS FOR PRINTED REPORT              
         ST    R1,SPECS                                                         
         LA    R1,HEDHOOK                                                       
         ST    R1,HEADHOOK                                                      
         B     PR02                                                             
*                                                                               
LR       OC    SVKEY+2(11),SVKEY+2 TEST FIRST TIME                              
         BNZ   PR06                NO - CONTINUE WITH NEXT ESTIMATE             
*                                                                               
PR02     BAS   RE,NEXTCLT                                                       
         BE    PR04                A CLIENT HEADER WAS FOUND                    
         CLI   MODE,PRINTREP                                                    
         BNE   PR70                END OF ON-SCREEN LIST                        
*                                                                               
         CLI   ESTFOUND,C'Y'       TEST ANY DATA FOUND                          
         BE    EXIT                YES                                          
         MVI   P+1,0               NO - PRINT ERROR MESSAGE ON REPORT           
         MVI   P2+1,0                                                           
         MVC   P3+1(16),=C'NO RECORDS FOUND'                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
PR04     BAS   RE,NEXTPRD                                                       
         BNE   PR02                NO MORE PRODUCTS FOR THIS CLIENT             
*                                                                               
PR06     BAS   RE,NEXTEST                                                       
         BE    PR08                AN ESTIMATE HEADER WAS FOUND                 
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR04                                                             
         OC    AFLDH,AFLDH         TEST ANY ESTIMATES ON SCREEN                 
         BZ    PR04                NO - DO NEXT PRODUCT                         
         B     PR60                YES - XMIT SCREEN                            
*                                                                               
PR08     CLI   MODE,PRINTREP                                                    
         BE    PR100                                                            
         EJECT                                                                  
* LIST THIS ESTIMATE HEADER ON SCREEN                                           
*                                                                               
PR10     OC    AFLDH,AFLDH         TEST NEW SCREEN                              
         BNZ   PR20                NO                                           
*                                                                               
         LA    R2,SFMHD1H                                                       
         BAS   RE,CLRSCRN          CLEAR LIST FIELDS                            
         USING LLINED,R4                                                        
         LA    R4,SFMHD1           FILL IN CLIENT AND PRODUCT                   
         MVC   LCLIENT,=C'CLIENT'                                               
         MVC   LCLT,EBCCLT                                                      
         MVC   LCLTNAME,CLTNM                                                   
         MVC   LPRODUCT,=C'PRODUCT'                                             
         MVC   LPRD,EBCPRD                                                      
         MVC   LPRDNAME,PRDNM                                                   
         OI    SFMHD1H+6,X'80'     XMIT                                         
         MVC   SFMHD2(51),=C'EST   START/END DATES   DESCRIPTION       +        
                   DEMOS'                                                       
         OI    SFMHD2H+6,X'80'     XMIT                                         
         LA    RF,SFMLST1H                                                      
         ST    RF,AFLDH                                                         
*                                                                               
PR20     L     R4,AFLDH                                                         
         LA    R4,8(R4)            A(BEGINNING OF FIELD)                        
         L     R2,AIO                                                           
         USING ESTHDRD,R2                                                       
         EDIT  EKEYEST,(3,LEST)                                                 
         GOTO1 DATCON,DMCB,(0,ESTART),(8,LESTSTRT)                              
         MVI   LESTDASH,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,EEND),(8,LESTEND)                                 
         MVC   LESTDESC,EDESC                                                   
*                                                                               
         LA    R3,EDEMLST          A(DEMO LIST)                                 
         OC    0(3,R3),0(R3)       TEST ANY DEMOS AT ALL                        
         BZ    PR50                NO                                           
         LA    R6,LDEMOS                                                        
         SR    R5,R5               COUNT DEMOS                                  
*                                                                               
PR30     BAS   RE,CNVRT            PUT DEMO IN LIST LINE                        
         LA    R3,3(R3)            BUMP TO NEXT DEMO                            
         LA    R5,1(R5)                                                         
         CH    R5,=H'4'            TEST MAXIMUM DEMOS ARE LISTED                
         BE    PR40                YES                                          
         OC    0(3,R3),0(R3)       TEST ANY MORE TO PRINT                       
         BZ    PR40                NO                                           
         B     PR30                DO NEXT DEMO                                 
*                                                                               
PR40     BCTR  R6,0                BACK UP TO LAST COMMA                        
         MVI   0(R6),C' '          REPLACE WITH BLANK                           
         DROP  R2,R4                                                            
*                                                                               
PR50     L     RF,AFLDH                                                         
         OI    6(RF),X'80'         XMIT                                         
         ZIC   R0,0(RF)            BUMP TO NEXT LIST FIELD                      
         AR    RF,R0                                                            
         LA    R1,SFMTAGH                                                       
         CR    R1,RF                                                            
         BE    PR60                                                             
         ST    RF,AFLDH            A(NEXT FIELD)                                
         B     PR06                                                             
*                                                                               
PR60     XC    AFLDH,AFLDH                                                      
         OI    GENSTAT2,USMYOK                                                  
         MVC   CONHEAD(26),=C'HIT ENTER TO CONTINUE LIST'                       
         OI    CONSERVH+6,X'81'    MODIFY AND XMIT                              
         OI    SFMMEDH+6,X'40'     POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
PR70     XC    AFLDH,AFLDH                                                      
         OI    GENSTAT2,USMYOK                                                  
         OI    CONSERVH+6,X'81'    MODIFY AND XMIT                              
         NI    SFMMEDH+4,X'DF'     TURN OFF VALIDATED BIT                       
         OI    SFMMEDH+6,X'40'     POSITION CURSOR                              
         LA    R2,SFMLST1H                                                      
         BAS   RE,CLRSCRN          CLEAR LIST FIELDS                            
*                                                                               
         CLI   ESTFOUND,C'Y'       TEST THERE WAS SOME DATA                     
         BNE   *+14                NO                                           
         MVC   CONHEAD(33),=C'END OF LIST - HIT ENTER FOR FIRST'                
         B     *+10                                                             
         MVC   CONHEAD(17),=C'NO DATA GENERATED'                                
         B     EXIT                                                             
         EJECT                                                                  
* PRINT THIS ESTIMATE HEADER                                                    
*                                                                               
PR100    L     R2,AIO                                                           
         USING ESTHDRD,R2                                                       
         LA    R4,P                                                             
         USING PLINED,R4                                                        
*                                                                               
         CLI   CLTSW,C'Y'          TEST NEW CLIENT                              
         BNE   PR110               NO                                           
*                                                                               
         OC    ACTPAGES,ACTPAGES   TEST FIRST PAGE                              
         BZ    PR105               YES                                          
         MVI   P+1,0                                                            
         CLI   DSSW,C'Y'           TEST DOUBLE SPACE                            
         BNE   *+8                 NO                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    ABOX,ABOX           TEST WE HAVE BOXES                           
         BZ    PR105               NO                                           
*                                                                               
         L     R5,ABOX             A(BOX DSECT)                                 
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         CLC   QBEST,QBESTEND                                                   
         BE    *+8                                                              
         MVI   ALLOWLIN,5                                                       
         CLI   DSSW,C'Y'           TEST DOUBLE SPACE                            
         BNE   *+8                 NO                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5                                                               
*                                                                               
PR105    LA    R4,MID1             CLIENT                                       
         MVI   FORCEMID,C'Y'                                                    
         MVC   PCLT,EBCCLT                                                      
         MVC   PCLTNAME,CLTNM                                                   
         LA    R4,P                                                             
*                                                                               
PR110    CLI   PRDSW,C'Y'          PRODUCT                                      
         BNE   PR115                                                            
         CLI   CLTSW,C'Y'                                                       
         BE    *+16                                                             
         MVI   P+1,0                                                            
         LA    R4,P2                                                            
         B     *+8                                                              
         LA    R4,P                                                             
         MVC   PPRD,EBCPRD                                                      
         MVC   PPRDNAME,PRDNM                                                   
*                                                                               
PR115    EDIT  EKEYEST,(3,PEST)                                                 
         GOTO1 DATCON,DMCB,(0,ESTART),(8,PESTSTRT)                              
         MVI   PESTDASH,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,EEND),(8,PESTEND)                                 
         MVC   PESTDESC,EDESC                                                   
*                                                                               
         LA    R3,EDEMLST          A(DEMO LIST)                                 
         OC    0(3,R3),0(R3)       TEST ANY DEMOS AT ALL                        
         BZ    PR140               NO                                           
         LA    R6,PDEMOS                                                        
         SR    R5,R5               COUNT DEMOS                                  
*                                                                               
PR120    BAS   RE,CNVRT            PUT DEMO IN PRINT LINE                       
         LA    R3,3(R3)            BUMP TO NEXT DEMO                            
         LA    R5,1(R5)                                                         
         CH    R5,=H'5'            TEST MAXIMUM DEMOS ARE PRINTED               
         BE    PR130               YES                                          
         OC    0(3,R3),0(R3)       TEST ANY MORE TO PRINT                       
         BZ    PR130               NO                                           
         B     PR120               DO NEXT DEMO                                 
*                                                                               
PR130    BCTR  R6,0                BACK UP TO LAST COMMA                        
         MVI   0(R6),C' '          REPLACE WITH BLANK                           
*                                                                               
PR140    OC    EBOOK,EBOOK                                                      
         BNZ   *+14                                                             
         MVC   PESTBOOK,=C'LATEST'                                              
         B     PR150                                                            
         GOTO1 DATCON,DMCB,(3,EBOOK),(6,PESTBOOK)                               
*                                                                               
PR150    MVC   PESTCOPY,ECOPY                                                   
         MVC   PESTDPT,EDAYMENU                                                 
*                                                                               
         CLC   QBEST,QBESTEND                                                   
         BE    *+8                                                              
         MVI   ALLOWLIN,3                                                       
         CLI   DSSW,C'Y'           TEST DOUBLE SPACE                            
         BNE   *+8                 NO                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   CLTSW,C'N'                                                       
         MVI   PRDSW,C'N'                                                       
*                                                                               
         DROP  R2,R4                                                            
         B     PR06                                                             
         EJECT                                                                  
* GET NEXT CLIENT                                                               
*                                                                               
NEXTCLT  NTR1                                                                   
*                                                                               
         MVI   CLTSW,C'Y'                                                       
         MVC   KEY,SVKEY           RESTORE LAST KEY                             
         LA    R2,KEY                                                           
         USING CLTHDRD,R2                                                       
         MVC   AIO,AIO1                                                         
         CLC   =C'ALL',QCLT                                                     
         BE    NC10                                                             
         CLI   QCLT,C'*'           TEST OFFICE REQUEST                          
         BE    NC10                                                             
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BE    NC10                                                             
         OC    SVKEY+2(11),SVKEY+2 TEST FIRST TIME                              
         BNZ   NEQXIT                                                           
*                                                                               
         MVC   CKEYCLT,BCLT                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEQXIT                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         B     NC40                                                             
*                                                                               
NC10     MVC   KEY+4(9),XFF        FORCE NEXT CLIENT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE      TEST SAME AGYMD                              
         BNE   NEQXIT                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
*                                                                               
         CLI   QCLT,C'*'           TEST OFFICE REQUEST                          
         BNE   NC30                                                             
         CLI   QCLT+1,C'-'         TEST 'ALL BUT'                               
         BE    NC20                                                             
         CLC   COFFICE,QCLT+1                                                   
         BE    NC40                OK                                           
         BL    NC10                                                             
         CLI   QCLT+2,C' '         TEST RANGE                                   
         BNH   NC10                NO                                           
         CLC   COFFICE,QCLT+2                                                   
         BH    NC10                                                             
         B     NC40                OK                                           
*                                                                               
NC20     CLC   COFFICE,QCLT+2      'ALL BUT'                                    
         BE    NC10                                                             
         B     NC40                                                             
*                                                                               
NC30     CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   NC40                                                             
         LA    R1,DUB              PROCESS A LIST OF OFFICES                    
         USING OFFICED,R1                                                       
         XC    DUB,DUB                                                          
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 OFFICER,DMCB,DUB,ACOMFACS                                        
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   NC10                                                             
*                                                                               
NC40     GOTO1 CLUNPK,DMCB,CKEYCLT,EBCCLT                                       
         MVC   CLTNM,CNAME                                                      
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         B     EQXIT                                                            
         DROP  R2                                                               
         EJECT                                                                  
* GET NEXT PRODUCT                                                              
*                                                                               
NEXTPRD  NTR1                                                                   
*                                                                               
         MVI   PRDSW,C'Y'                                                       
         MVC   KEY,SVKEY           RESTORE LAST KEY                             
         CLC   =C'ALL',QPRD        TEST ALL PRD REQ                             
         BE    NP10                                                             
         OC    KEY+4(3),KEY+4      TEST FIRST TIME                              
         BNZ   NEQXIT                                                           
         MVC   KEY+4(3),QPRD                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   NEQXIT                                                           
         B     NP20                                                             
*                                                                               
NP10     MVC   KEY+7(6),XFF        FORCE NEXT PRD                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      TEST SAME A-M/CLT                            
         BNE   NEQXIT                                                           
*                                                                               
NP20     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         USING PRDHDRD,R2                                                       
         MVC   EBCPRD,PKEY+4                                                    
         MVC   PRDNM,PNAME                                                      
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         B     EQXIT                                                            
         DROP  R2                                                               
         EJECT                                                                  
* GET NEXT ESTIMATE                                                             
*                                                                               
NEXTEST  NTR1                                                                   
*                                                                               
         MVC   KEY,SVKEY           RESTORE LAST KEY                             
         CLI   KEY+7,0             TEST FIRST TIME                              
         BNE   NE20                NO                                           
         MVC   KEY+7(1),QBEST      SET ESTIMATE NUMBER                          
*                                                                               
NE10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   NEQXIT                                                           
         MVC   SVKEY,KEY                                                        
         OC    KEY+8(5),KEY+8      TEST BILL RECORD                             
         BNZ   NE20                                                             
         CLC   KEY+7(1),QBEST                                                   
         BL    NE20                                                             
         CLC   KEY+7(1),QBESTEND                                                
         BH    NEQXIT                                                           
         B     NE30                                                             
*                                                                               
NE20     MVC   KEY+8(5),XFF        FORCE NEXT ESTIMATE                          
         B     NE10                                                             
*                                                                               
NE30     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         USING ESTHDRD,R2                                                       
         CLC   ESTART,QEND         EST START AFTER REQ END                      
         BH    NE20                                                             
         CLC   EEND,QSTART         EST END AFTER REQ START                      
         BL    NE20                                                             
         MVC   SVKEY,KEY           SAVE LAST KEY PROCESSED                      
         MVI   ESTFOUND,C'Y'       WE HAVE SOME DATA                            
         B     EQXIT               AND GO PROCESS                               
         DROP  R2                                                               
         EJECT                                                                  
* CONVERT DEMO CODE TO PRINTABLE FORMAT                                         
*  R3 - A(DEMO CODE)                                                            
*  R6 - A(NEXT PRINT POSITION)                                                  
*                                                                               
CNVRT    NTR1                                                                   
*                                                                               
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         XC    BLOCK(256),BLOCK                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'A'                                                    
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGY                            
         BNE   CNV5                                                             
*                                                                               
         CLI   SVCXTRA,C'U'          TEST US CLIENT                             
         BE    CNV5                                                             
         MVI   DBSELMED,C'C'       SET 'C' FOR ALL CANAD MEDIA                  
         DROP  RE                                                               
*                                                                               
CNV5     GOTO1 DEMOCON,DMCB,(0,(R3)),(2,(R6)),(C'S',BLOCK)  7 CHAR              
*                                                                               
CNV10    CLI   0(R6),C' '          LOOK FOR END OF NAME                         
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     CNV10                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         XIT1  REGS=(R6)           R6-NEXT POSSIBLE NAME AREA                   
         EJECT                                                                  
* CLEAR SCREEN (FROM R2)                                                        
*                                                                               
CLRSCRN  NTR1                                                                   
*                                                                               
         LA    RF,SFMTAGH                                                       
         SR    RE,RE                                                            
*                                                                               
CS10     IC    RE,0(R2)                                                         
         SHI   RE,9                                                             
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SHI   RE,8                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
*                                                                               
CS20     IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,RF                                                            
         BNE   CS10                                                             
         B     EXIT                                                             
         EJECT                                                                  
* HEADLINE HOOK                                                                 
*                                                                               
HEDHOOK  NTR1                                                                   
*                                                                               
         CLI   ESTFOUND,C'Y'       TEST THERE IS DATA TO PRINT                  
         BNE   HHX                 NO                                           
*                                                                               
         MVC   H1+1(L'MEDNM),MEDNM                                              
         OC    QSTART,QSTART       TEST START DATE GIVEN                        
         BZ    HED20               NO                                           
         CLC   QSTART,QEND         TEST ONLY ONE DATE GIVEN                     
         BE    HED10               RIGHT                                        
*                                                                               
         MVI   H3+53,C'('                                                       
         MVI   H3+62,C'-'                                                       
         MVI   H3+71,C')'                                                       
         GOTO1 DATCON,DMCB,(0,QSTART),(5,H3+54)                                 
         GOTO1 DATCON,DMCB,(0,QEND),(5,H3+63)                                   
         B     HED20                                                            
*                                                                               
HED10    MVI   H3+57,C'('                                                       
         MVI   H3+66,C')'                                                       
         GOTO1 DATCON,DMCB,(0,QSTART),(5,H3+58)                                 
*                                                                               
HED20    CLI   CLTSW,C'Y'                                                       
         BE    HED30                                                            
         LA    R4,MID1                                                          
         USING PLINED,R4                                                        
         MVC   PCLT,EBCCLT                                                      
         MVC   PCLTNAME,CLTNM                                                   
         MVC   PCLTNAME+L'PCLTNAME+1(11),=C'(CONTINUED)'                        
*                                                                               
HED30    CLI   PRDSW,C'Y'                                                       
         BE    HED40                                                            
         LA    R4,MID2                                                          
         MVC   PPRD,EBCPRD                                                      
         MVC   PPRDNAME,PRDNM                                                   
         DROP  R4                                                               
*                                                                               
HED40    ICM   R4,15,ABOX                                                       
         BZ    HHX                                                              
*                                                                               
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+28,C'C'                                                  
         MVI   BOXCOLS+32,C'C'                                                  
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+71,C'C'                                                  
         MVI   BOXCOLS+28,C'C'                                                  
         MVI   BOXCOLS+111,C'C'                                                 
         MVI   BOXCOLS+118,C'C'                                                 
         MVI   BOXCOLS+122,C'C'                                                 
         MVI   BOXCOLS+126,C'R'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HHX      B     EXIT                                                             
         SPACE 5                                                                
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,52,C'ESTIMATE HEADER LISTING'                                 
         SSPEC H2,52,C'-----------------------'                                 
         SSPEC H1,92,AGYNAME                                                    
         SSPEC H2,92,AGYADD                                                     
         SSPEC H3,92,REPORT                                                     
         SSPEC H3,104,RUN                                                       
         SSPEC H3,2,PAGE                                                        
         SSPEC H6,2,C'CLIENT/PRODUCT'                                           
         SSPEC H6,30,C'EST  START/END DATES'                                    
         SSPEC H6,52,C'DESCRIPTION'                                             
         SSPEC H6,73,C'DEMOS'                                                   
         SSPEC H6,114,C'BOOK  CPY DPT'                                          
         DC    X'00'                                                            
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
XFF      DC    16X'FF'                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                     PRINT LINE                                   
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL4                                                              
PCLTNAME DS    CL20                                                             
*                                                                               
         ORG   PLINED                                                           
         DS    CL4                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPRDNAME DS    CL20                                                             
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PESTSTRT DS    CL8                                                              
PESTDASH DS    CL1                                                              
PESTEND  DS    CL8                                                              
         DS    CL1                                                              
PESTDESC DS    CL20                                                             
         DS    CL1                                                              
PDEMOS   DS    5CL8                                                             
PESTBOOK DS    CL6                                                              
         DS    CL2                                                              
PESTCOPY DS    CL1                                                              
         DS    CL3                                                              
PESTDPT  DS    CL1                                                              
         SPACE 3                                                                
LLINED   DSECT                     ON-SCREEN REPORT LINE                        
LCLIENT  DS    CL6                                                              
         DS    CL1                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LCLTNAME DS    CL20                                                             
         DS    CL4                                                              
LPRODUCT DS    CL7                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LPRDNAME DS    CL20                                                             
*                                                                               
         ORG   LLINED                                                           
LEST     DS    CL3                                                              
         DS    CL2                                                              
LESTSTRT DS    CL8                                                              
LESTDASH DS    CL1                                                              
LESTEND  DS    CL8                                                              
         DS    CL2                                                              
LESTDESC DS    CL20                                                             
         DS    CL2                                                              
LDEMOS   DS    4CL8                                                             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMDBD                                                       
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
         ORG   SYSSPARE                                                         
AFLDH    DS    A                   A(ON-SCREEN FIELD HEADER)                    
QSTART   DS    CL6                 PERIOD START DATE                            
QEND     DS    CL6                 PERIOD END DATE                              
QBEST    DS    XL1                 ESTIMATE RANGE START                         
QBESTEND DS    XL1                 ESTIMATE RANGE END                           
EBCCLT   DS    CL3                 ALPHA CLIENT CODE                            
EBCPRD   DS    CL3                 ALPHA PRODUCT CODE                           
CLTSW    DS    C                   'Y' = BREAK ON CLIENT                        
PRDSW    DS    C                   'Y' = BREAK ON PRODUCT                       
ESTFOUND DS    C                   'Y' = SOME DATA WAS FOUND                    
DSSW     DS    C                   'Y' = DOUBLE SPACE REPORT                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPSFM1B   10/24/11'                                      
         END                                                                    

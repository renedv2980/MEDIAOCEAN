*          DATA SET NEPUP23    AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET NEPUP23    AT LEVEL 065 AS OF 08/02/90                      
*PHASE T32223A,*                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'T32223 - PRB1 REPORT'                                           
T32223   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32223**,RA,R6,RR=R2                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   VAL2                                                             
         LA    RE,TYPE             CLEAR WORK AREA                              
         LA    RF,QTITLE-TYPE                                                   
         XCEF                                                                   
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
VAL2     CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD FOR REPORTS                                      
         SPACE 3                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
**       MVC   8(7,R2),DPTNAME                                                  
**       OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPOPTH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         SPACE 1                                                                
         LA    R2,PUPQSTLH                                                      
         MVI   QTITLE,0                                                         
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         MVC   TITLE,PUPQSTL                                                    
         MVI   QTITLE,1                                                         
         SPACE 1                                                                
VRECX    B     XIT                                                              
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         MVI   BOXOPT,C'Y'                                                      
         MVI   FIELDERR,1                                                       
         MVI   REQLEN,0                                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(3,R4),=C'LEN '   LENGTH FILTERING OPTION                      
         BNE   OPT10                                                            
         MVC   REQLEN,11(R4)                                                    
         CLI   REQLEN,1                                                         
         BL    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL REPORTS                                                  
         SPACE 3                                                                
PREP     NTR1                                                                   
*                                                                               
*                                                                               
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 PUPIO,DMCB,PUPHOOK                                               
         B     PLANEND                                                          
*                                                                               
PUPHOOK  NTR1                                                                   
         CLI   PUPMODE,PLANFRST                                                 
         BNE   PHK5                                                             
         CLI   FRST,0                                                           
         BNE   PHK10                                                            
         MVC   PLANYRSV,PLANYEAR                                                
         MVI   FRST,1                                                           
         MVC   PLANLSV,PLANLENS                                                 
         MVC   PLANNSV,PLANNLEN                                                 
*                                                                               
         ZIC   R1,PLANNSV          GET DISP TO CENTER OUTPUT                    
         MH    R1,=H'4'                                                         
         LA    R2,16                                                            
         SR    R2,R1                                                            
         SRA   R2,1                                                             
         ST    R2,DISP                                                          
         ZIC   R1,PLANNSV          GET LENGTH OF BINRECS                        
         MH    R1,=H'20'                                                        
         LA    R2,BINKLENE                                                      
         AR    R2,R1                                                            
         A     R2,=F'30'           *** CAREFUL HARD CODED ***                   
         ST    R2,BINRLEN                                                       
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         LA    R1,BUFF             A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         L     R3,BINRLEN                                                       
         LA    R4,BINKLENE         DISP OF KEY INTO REC                         
         L     R5,=F'49'         MAX RECS IN BINTBL                             
* POSSIBLE TO SQUEEZEOUT A FEW MORE IF PLANNLEN < 4                             
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         LA    RE,BUFF          CLEAR BINTABLE                                  
         L     RF,=F'5000'                                                      
         XCEF                                                                   
         B     PHKX                                                             
*                                                                               
PHK5     CLI   PUPMODE,PROGMD                                                   
         BNE   PHKX                                                             
         BAS   RE,POSTPROG                                                      
         B     PHKX                                                             
*                                                                               
PHK10    CLC   PLANLSV,PLANLENS                                                 
         BE    PHKX                                                             
         CLC   PLANNSV,PLANNLEN                                                 
         BE    PHKX                                                             
         MVI   PUPMODE,PUPIOEND                                                 
         MVC   P(46),=C'*** ERROR -PLANS DO NOT HAVE SAME SPOT LENGTHS'         
         GOTO1 SPOOL,DMCB,(R8)                                                  
PHKX     B     XIT                                                              
*                                                                               
PLANEND  CLI   PUPMODE,PUPIOEND    IF ERROR/NO TOTS                             
         BE    XIT                                                              
         BAS   RE,GETOTS                                                        
         BAS   RE,PRINTIT          FLUSH DATA AREAS/PRINT                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R3,MYWORK2                                                       
         USING BIND,R3                                                          
         XC    MYWORK2,MYWORK2                                                  
         MVC   BINKDAY,PROGDAYC                                                 
         MVC   BINKTIME,PROGMIL                                                 
         MVC   BINKPROG,PROGCODE                                                
         MVC   BINDAY,PROGDAY                                                   
         MVC   BINTIME,PROGTIME                                                 
         MVC   BINPROG,PROGNAME                                                 
         BAS   RE,GETNUNTS                 RETURNS UNITS IN MYWORK              
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R3)),RR=RELO                             
         L     R3,0(R1)            GET RECORD ADDRESS                           
         ST    R3,APLINE           SAVE REC ADR                                 
         LA    R3,BINQ4            AND ADD RETURNED UNITS                       
         LA    R5,MYWORK                                                        
         ZIC   R4,PLANNSV                                                       
         MH    R4,=H'4'                                                         
PPR3     L     R0,0(R5)                                                         
         L     R1,0(R3)                                                         
         AR    R1,R0                                                            
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,PPR3                                                          
PPRX     B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
* GET UNITS BY PERIOD AND LENGTH FOR ENTIRE PROGRAM                             
*     AND STORES IT IN MYWORK                                                   
*                                                                               
GETNUNTS NTR1                                                                   
         XC    MYWORK,MYWORK                                                    
         LA    R2,PLANPLST                                                      
         ZIC   R3,PLANNPER         NUMBER OF PERIODS                            
GTN3     MVC   PERIOD,0(R2)                                                     
         ZIC   R1,2(R2)            POINT TO PROPER QUARTER SAVEAREA             
         LA    R5,MYWORK                                                        
         ZIC   R0,PLANNSV                                                       
         MH    R0,=H'4'                                                         
         STH   R0,MYHALF                                                        
         MH    R1,MYHALF                                                        
         AR    R5,R1                                                            
         BAS   RE,GETUNTS          GET UNITS FOR THAT PERIOD                    
         LA    R2,4(R2)                                                         
         BCT   R3,GTN3                                                          
         B     XIT                                                              
*                                                                               
*                        R5 POINTS TO QUARTER SAVE AREA IN MYWORK               
*                           RETURNS UNITS BY LENGTH                             
GETUNTS  NTR1                                                                   
         ZIC   R2,PLANNSV                                                       
         LA    R3,PLANLSV                                                       
         CLI   REQLEN,0                                                         
         BE    GTU5                                                             
         MVC   LENGTH,REQLEN                                                    
         LA    R2,1                                                             
         B     *+10                                                             
GTU5     MVC   LENGTH,0(R3)                                                     
         GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS                                                         
         L     R4,0(R5)                                                         
         AR    R1,R4                                                            
         ST    R1,0(R5)                                                         
         LA    R3,1(R3)            BUMP LENGTHS                                 
         LA    R5,4(R5)            BUMP SAVE AREA                               
         BCT   R2,GTU5                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                  ADD UNITS INTO TOTALS                        
GETOTS   NTR1                                                                   
         LA    R3,BUFF                                                          
         ST    R3,ABINTBL                                                       
GET1     LA    R2,BINQ4            FIND START OF TOTALS                         
         ZIC   R1,PLANNSV                                                       
         MH    R1,=H'16'                                                        
         AR    R2,R1                                                            
         ST    R2,ATOTALS                                                       
*                                                                               
         LA    R2,4                                                             
         LA    R3,BINQ4                                                         
GET4     ZIC   R5,PLANNSV                                                       
         L     R4,ATOTALS                                                       
GET5     L     R1,0(R3)                                                         
         L     R0,0(R4)                                                         
         AR    R1,R0                                                            
         ST    R1,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,GET5                                                          
         BCT   R2,GET4                                                          
         L     R3,ABINTBL                                                       
         A     R3,BINRLEN                                                       
         ST    R3,ABINTBL                                                       
         OC    0(5,R3),0(R3)                                                    
         BNZ   GET1                                                             
GETX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*    FLUSH DATA SAVE AREAS                                                      
*                                                                               
PRINTIT  NTR1                                                                   
         USING PLINED,R2                                                        
         USING BIND,R3                                                          
         LA    R3,BUFF                                                          
         ST    R3,ABIN                                                          
PRT1     LA    R2,P                                                             
         OC    0(5,R3),0(R3)                                                    
         BE    PRTX                                                             
         MVC   PPROG,BINPROG                                                    
         MVC   PDAY,BINDAY                                                      
         MVC   PTIME,BINTIME                                                    
         LA    R2,PDATA                                                         
         ST    R2,APLINE                                                        
         A     R2,DISP                                                          
         LA    R3,BINQ4                                                         
         LA    R5,5                                                             
PRT2     ZIC   R4,PLANNSV                                                       
PRT4     OC    0(4,R3),0(R3)                                                    
         BZ    PRT6                                                             
         EDIT  (B4,0(R3)),(3,0(R2))                                             
PRT6     LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PRT4                                                          
         L     R2,APLINE                                                        
         LA    R2,16(R2)                                                        
         ST    R2,APLINE                                                        
         A     R2,DISP                                                          
         BCT   R5,PRT2                                                          
         BAS   RE,WRITIT                                                        
         L     R3,ABIN                                                          
         A     R3,BINRLEN                                                       
         ST    R3,ABIN                                                          
         B     PRT1                                                             
PRTX     MVI   SPACING,2           SKIP LINES BEFORE TOTALS                     
         BAS   RE,WRITIT                                                        
         LA    R3,BUFF             ADD UP SEC LEN TOTS PER QUARTER              
         ST    R3,APLINE                                                        
PRTX1    LA    R4,FINTOTS                                                       
         LA    R2,BINQ4                                                         
         ZIC   R5,PLANNSV                                                       
         MH    R5,=H'5'                                                         
PRTX2    L     R1,0(R2)                                                         
         L     R0,0(R4)                                                         
         AR    R1,R0                                                            
         ST    R1,0(R4)                                                         
         LA    R2,4(R2)            BUMP BINREC AREA                             
         LA    R4,4(R4)            BUMP TO NXT TOTAL AREA                       
         BCT   R5,PRTX2                                                         
         L     R3,APLINE                                                        
         A     R3,BINRLEN                                                       
         ST    R3,APLINE                                                        
         OC    0(5,R3),0(R3)                                                    
         BNZ   PRTX1                                                            
         LA    R2,P                WRITE TOTALS                                 
         USING PLINED,R2                                                        
         MVC   P+10(12),=C'***TOTALS***'                                        
         LA    R2,PDATA                                                         
         ST    R2,APLINE                                                        
         A     R2,DISP                                                          
         LA    R3,FINTOTS                                                       
         LA    R5,5                                                             
         ZIC   R4,PLANNSV                                                       
PRTX3    EDIT  (B4,0(R3)),(3,0(R2))                                             
         LA    R3,4(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,PRTX3                                                         
         L     R2,APLINE                                                        
         LA    R2,16(R2)                                                        
         ST    R2,APLINE                                                        
         A     R2,DISP                                                          
         ZIC   R4,PLANNSV                                                       
         BCT   R5,PRTX3                                                         
         BAS   RE,WRITIT                                                        
PRTXX    B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVI   HEAD2+1,C'_'     UNDERLINE SYSTEM NAME                           
         MVC   HEAD2+2(21),HEAD2+1                                              
         MVC   WORK(40),TITLE      REPORT TITLE                                 
         OC    WORK,SPACES                                                      
         CLI   WORK,C' '                                                        
         BH    HOOK2                                                            
         SPACE 1                                                                
         CLI   QTITLE,0                                                         
         BE    *+14                                                             
         MVC   WORK(40),TITLE                                                   
         B     HOOK2                                                            
         MVC   WORK(40),=CL40'UNIT SUMMARY BY QUARTER'                          
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         SPACE 1                                                                
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
         MVC   HEAD5+9(4),PUPNET                                                
         MVC   HEAD4+62(7),PUPDPT                                               
         MVC   HEAD5+59(4),PUPPLAN                                              
         LA    R2,HEAD5+64                                                      
         ZIC   R1,PLANYRSV                                                      
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,0(R2))                                                   
         LA    R2,HEAD7                                                         
         USING PLINED,R2                                                        
         ZIC   R1,PLANYRSV                                                      
         BCTR  R1,0                                                             
         STC   R1,TYPE                                                          
         LA    R4,5                                                             
         LA    R2,PDATA                                                         
         LA    R3,QUALPHA                                                       
HOOK2B   MVC   0(16,R2),0(R3)                                                   
         CLI   2(R3),C'A'                                                       
         BE    HOOK3                                                            
         ZIC   R1,TYPE                                                          
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,10(R2))                                                  
HOOK3    LA    R2,16(R2)                                                        
         LA    R3,16(R3)                                                        
         ZIC   R1,PLANYRSV                                                      
         STC   R1,TYPE                                                          
         BCT   R4,HOOK2B                                                        
         LA    R2,HEAD8                                                         
         MVC   PPROG,=C'----PROGRAM-----'                                       
         MVC   PDAY,=C'DAY'                                                     
         MVC   PTIME,=C'---TIME----'                                            
         LA    R2,PDATA                                                         
         LA    R5,5                                                             
*        CLI   PLANNSV,4                                                        
*        BE    HOOK4                                                            
         ST    R2,APLINE                                                        
         A     R2,DISP                                                          
HOOK4    ZIC   R4,PLANNSV                                                       
         LA    R3,PLANLSV                                                       
         CLI   REQLEN,0                                                         
         BE    HOOK4B                                                           
         LA    R4,1                                                             
         LA    R3,REQLEN                                                        
HOOK4B   MVI   0(R2),C':'                                                       
         EDIT  (B1,0(R3)),(2,1(R2))                                             
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,HOOK4B                                                        
         L     R2,APLINE                                                        
         LA    R2,16(R2)                                                        
         ST    R2,APLINE                                                        
         A     R2,DISP                                                          
         BCT   R5,HOOK4                                                         
         DROP  R2                                                               
*                                                                               
*                                                                               
         CLI   BOXOPT,C'N'                                                      
         BE    HOOKX                                                            
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         LA    R3,BOXROWS                                                       
         LA    R3,5(R3)                                                         
         MVI   0(R3),C'T'                                                       
         LA    R3,3(R3)                                                         
         MVI   0(R3),C'M'                                                       
         LA    R3,65(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R3,BOXCOLS                                                       
         USING PLINED,R3                                                        
         MVI   0(R3),C'L'                                                       
         LA    R3,PDATA                                                         
         MVI   0(R3),C'C'                                                       
         LA    R5,4                                                             
         LA    R3,16(R3)                                                        
         MVI   0(R3),C'C'                                                       
         BCT   R5,*-8                                                           
         LA    R3,16(R3)                                                        
         MVI   0(R3),C'R'                                                       
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 1                                                                
*                                                                               
         SPACE 3                                                                
WRITIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'MSGQUART),MSGQUART                                     
         B     MYEND                                                            
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
DRQLIST  DC    AL1(4,1,2,3)                                                     
         SPACE 1                                                                
MSGQUART DC    C'** ERROR ** QUARTER MUST BE 1-4'                               
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         SPACE 1                                                                
QUALPHA  DC    C'  4TH QTR 19    '                                              
         DC    C'  1ST QTR 19    '                                              
         DC    C'  2ND QTR 19    '                                              
         DC    C'  3RD QTR 19    '                                              
         DC    C'  ANNUAL TOTAL  '                                              
         SPACE 1                                                                
         SPACE 1                                                                
SAVEPROF DC    X'0000'                                                          
         EJECT                                                                  
*              SPECS FOR PHASE                                                  
         SPACE 3                                                                
HEDSPECS DS    0D                                                               
         SSPEC H1,2,C'NETWORK UPFRONT SYSTEM'                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'NETWORK'                                                  
         SSPEC H4,55,C'DAYPART'                                                 
         SSPEC H5,55,C'PLAN      1988'                                          
         SPACE 1                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,115,PAGE                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         SPACE 1                                                                
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPD3D                                                       
         EJECT                                                                  
         SPACE 1                                                                
*                             WORK AREA                                         
RELO     DS    F                                                                
TYPE     DS    CL1                                                              
REQLEN   DS    CL1                                                              
FRST     DS    CL1                                                              
PLANLSV  DS    CL4                                                              
PLANNSV  DS    CL1                                                              
PLANYRSV DS    CL1                                                              
DISP     DS    F                                                                
APLINE   DS    F                                                                
ABIN     DS    F                                                                
ABINTBL  DS    F                                                                
ATOTALS  DS    F                                                                
BINRLEN  DS    F                   LENGTH OF BINREC                             
*                                  VARIABLE-DEPENDS ON PLANNLEN                 
BINDMCB  DS    6F                                                               
FINTOTS  DS    20F                                                              
MYHALF   DS    CL2                                                              
MYWORK   DS    CL80                                                             
MYWORK2  DS    CL200                                                            
TITLE    DS    CL40                                                             
QTITLE   DS    CL1                                                              
         DS    0F                                                               
         SPACE 3                                                                
*                                                                               
BIND     DSECT                  DSECT FOR BINSRCH RECORDS                       
BINKDAY  DS    CL1                                                              
BINKTIME DS    CL4                                                              
BINKPROG DS    CL6                                                              
BINKLENE EQU   *-BINKDAY                                                        
BINDAY   DS    CL3                                                              
BINTIME  DS    CL11                                                             
BINPROG  DS    CL16                                                             
BINQ4    DS    CL16          Q4    UNITS BY LENGTH                              
         DS    CL16          Q1    (NOTE ACTUAL LENGTH=4XPLANNLEN)              
         DS    CL16          Q2                                                 
         DS    CL16          Q3                                                 
         DS    CL16          TOTAL AREA                                         
         SPACE 3                                                                
*                                                                               
PLINED   DSECT                                                                  
         DS    CL1                                                              
PPROG    DS    CL16                                                             
         DS    CL1                                                              
PDAY     DS    CL3                                                              
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PDATA    DS    CL80                                                             
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEPUP23   05/01/02'                                      
         END                                                                    

*          DATA SET DDPANLMCMP AT LEVEL 009 AS OF 05/27/05                      
*                                                                               
*THIS PHASE SHOULD BE REPLACED BY DDLMCOMP, 5/27/05                             
*PLEASE STOP USING THIS.                                                        
*ANY FURTHER DEVELOPMENT/UPDATES, PLEASE DO SO IN DDLMCOMP                      
*                                                                               
*                                                                               
*PHASE PANLMCA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DDINFO                                                                 
*                                                                               
***********************************************************************         
*THIS PROGRAM IS CALLED TO CHECK IF IT IS OKAY TO PROMOTE A NEW VERSION         
*OF PHASE LIVE.                                                                 
*THIS PROGRAM GENERATES 2 LISTINGS OF OBJECT MODULES'S INFO FOR 2 GIVEN         
*LOAD MODULES.  THEN, IT COMPARES THE BOOKNAME, LEVEL STAMP AND DATE.           
*IT OUTPUTS THE RESULT OF COMPARSION TO A DATASET AND IT SETS THE               
*CONDITION CODE ACCORDINGLY.                                                    
*                                                                               
*                                                                               
*THE PROGRAM IS VERY SENSITIVE TO THE FORMAT OF LOAD MODULE.                    
*THESE ARE THE ASSUMPTIONS:                                                     
*(1) THE LOAD MODULE DOESN'T HAVE ANY SEGMENTS.                                 
*(2) THE 1ST RECORD MUST BE X'2080' CESD(S)                                     
*(3) FOLLOWING THAT, ARE IDRS RECORDS  X'80'                                    
*(4) RIGHT AFTER THEM, MUST BE A CONTROL RECORD X'01' OR X'0D'                  
*(5) THEN COMES X'02'/X'03'/X'0E'/X'0F' OR X'01' AGAIN CONTROL RECORDS          
*(6) X'02' ARE SKIPPED                                                          
*(7) ONLY X'03' AND X'01' AND X'0F' ARE CONSIDERED.                             
*(8) X'0E'/X'0F' IS ALSO CONSIDERED AND IT IS THE LAST RECORD.                  
*                                                                               
*FOR MORE INFOMATION ON LOAD MODULE RECORD FORMAT, SEE                          
*TITLE: Z/OS V1R3.0-V1R4.0 MVS PROGRAM MANAGEMENT: ADVANCED FACILITIES          
*DOCUMENT NUMBER: SA22-7644-02                                                  
*APPENDIX B.2                                                                   
*                                                                               
***********************************************************************         
PANLMC   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*PANLMC*,=V(REGSAVE),R9                                        
*                                                                               
***********************************************************************         
*--------PATCH HERE TO EXIT UNCONDITIONALLY---------------------------*         
***********************************************************************         
         BC    0,EXIT                                                           
***********************************************************************         
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
*GET THE MEMBER NAME OF THE NEW LM                                              
         GOTO1 =V(DDINFO),DMCB,(8,DDNAME),TUNIT,0                               
         L     RE,DMCB+8                                                        
         MVC   LMNAME,0(RE)                                                     
*                                                                               
         USING PTD,R4                                                           
         L     R4,=A(PHASETAB)                                                  
         MVI   WHICH,OLD                                                        
*                                                                               
PMC010   LR    R8,R4               SAVE A(CURRENT PHASE TABLE)                  
         OPEN  INFILE                                                           
         SR    R3,R3               # OF CSECT COUNTER                           
*                                                                               
*FIND THE 1ST CESD RECORD (X'2080')                                             
*                                                                               
PMC011   GET   INFILE                                                           
         CLC   0(2,R1),=X'2080'    CESD RECORD                                  
         BNE   PMC011                                                           
         B     PMC030                                                           
*                                                                               
PMC020   GET   INFILE                                                           
         CLC   0(2,R1),=X'2080'    CESD RECORD                                  
         BNE   PMC090              NO MORE                                      
*                                                                               
*EXTRACT THE CESD DATA (PHASE NAME, LMOD LOC, LENGTH AND TYPE)                  
*                                                                               
PMC030   ZICM  R2,6(R1),2          BYTE COUNT OF CESD DATA                      
         SRL   R2,4                /CESD DATA SIZE,16                           
         LA    R1,8(R1)            BEGINNING OF CESD DATA                       
*                                                                               
PMC040   ZIC   RE,8(R1)            CESD TYPE                                    
         SLL   RE,28                                                            
         SRL   RE,28               GET RID OF 1ST NIBBLE                        
         STC   RE,BYTE                                                          
*                                                                               
         CLI   BYTE,SD                                                          
         BE    PMC050                                                           
         CLI   BYTE,PC                                                          
         BE    PMC050                                                           
         CLI   BYTE,CM                                                          
         BE    PMC050                                                           
         CLI   BYTE,PR                                                          
         BE    PMC050                                                           
         B     PMC060                                                           
*                                                                               
PMC050   MVC   PTNAM,0(R1)         CSECT NAME                                   
         MVC   PTTYP,BYTE          TYPE                                         
         MVC   PTLOC,9(R1)         LOCATION                                     
         MVC   PTLEN,13(R1)        LENGTH                                       
         MVC   PTFLAG,WHICH        OLD/NEW                                      
*                                                                               
         AHI   R4,PTQ                                                           
         LA    R3,1(R3)                                                         
PMC060   AHI   R1,16                                                            
         BCT   R2,PMC040                                                        
         B     PMC020                                                           
*                                                                               
PMC090   LR    R7,R1                                                            
         GOTO1 =V(XSORT),DMCB,(R8),(R3),PTQ,3,0                                 
         MVC   0(2,R4),=X'FFFF'       END OF TABLE                              
         LR    R1,R7                                                            
*                                                                               
         CLI   WHICH,OLD                                                        
         BNE   *+12                                                             
         STH   R3,COUNT1                                                        
         B     PMC101                                                           
         STH   R3,COUNT2                                                        
         B     PMC101                                                           
*                                                                               
*SKIP ALL LOAD MODULE IDRS (X'80')                                              
*                                                                               
PMC100   GET   INFILE                                                           
PMC101   CLI   0(R1),X'80'         IDR RECORD                                   
         BNE   PMC199                                                           
*                                                                               
         CLI   2(R1),X'02'         LINKAGE EDITOR DATA                          
         BNE   PMC100                                                           
*                                  SAVE TIME AND DATE LAST LINKED               
         CLI   WHICH,OLD                                                        
         BNE   *+20                                                             
         MVC   OLDLDATE,15(R1)                                                  
         MVC   OLDLTIME,18(R1)                                                  
         B     PMC100                                                           
*                                                                               
         MVC   NEWLDATE,15(R1)                                                  
         MVC   NEWLTIME,18(R1)                                                  
         B     PMC100                                                           
*                                                                               
PMC199   EQU   *                                                                
         LR    R4,R8               PT BACK TO A(CURRENT PHASE TABLE)            
*                                                                               
*MUST BE A CONTROL RECORD X'01' OR X'0D'                                        
*                                                                               
         CLI   0(R1),X'01'         CONTROL RECORD                               
         BE    PMC199X                                                          
         CLI   0(R1),X'0D'         CONTROL RECORD WITH EOM                      
         BE    PMC199X                                                          
         MVI   RETCODE,4           SET RETURN CODE AS 4                         
         B     EXIT                                                             
PMC199X  EQU   *                                                                
*                                                                               
         L     R5,8(R1)            STORE THE STARTING ADDRESS                   
*                                                                               
PMC200   ZICM  RE,13(R1),3         SIZE OF THE TEXT RECORD FOLLOWED             
         L     R6,8(R1)                                                         
*                                                                               
         SR    R6,R5               BEGIN LMOD LOC                               
         LR    R7,R6                                                            
         AR    R7,RE               END LMOD LOC                                 
*                                                                               
         GET   INFILE              READ THE TEXT                                
PMC220   ZICM  RF,PTLOC,3                                                       
         CR    RF,R7                                                            
         BH    PMC290                                                           
*                                                                               
         ZICM  RE,PTLEN,3                                                       
         CHI   RE,60                                                            
         BL    PMC260              SKIP PHASE W/LENGTH < 60                     
*                                                                               
         AR    RF,RE                                                            
         CR    RF,R7                                                            
         BH    PMC290                                                           
*                                                                               
         SR    RF,R6                                                            
         AHI   RF,-100                                                          
         AR    RF,R1                                                            
*                                                                               
*SEARCH FOR BOOK, LEVEL, DATE                                                   
*                                                                               
         LA    RE,100                                                           
*ASSUME LAST LABEL (BOOK=,LEVEL=,DATE=) IS AT LEAST 6 BYTES LONG                
*THIS IS AVOID "CLC" AREA THAT MAY BE OUT OF RANGE. 4/23/04, YYUN               
         SHI   RE,6                                                             
*                                                                               
PMC240   CLC   =C'BOOK=',0(RF)                                                  
         BNE   *+10                                                             
         MVC   PTBNAM,5(RF)                                                     
         CLC   =C'LEVEL=',0(RF)                                                 
         BNE   *+10                                                             
         MVC   PTBLEV,6(RF)                                                     
*                                                                               
         CLC   =C'DATE=',0(RF)     DATE=MMMDD/YY                                
         BNE   PMC250                                                           
*                                                                               
         CLI   5(RF),C'0'                                                       
         BL    PMC242                                                           
         MVC   PTBDAT,5(RF)        MM/DD/YY                                     
         B     PMC250                                                           
*                                                                               
PMC242   MVC   MONTH,5(RF)                                                      
         L     R2,=A(MONTAB)                                                    
PMC245   CLC   MONTH,0(R2)                                                      
         BE    PMC248                                                           
         CLI   0(R2),X'FF'                                                      
         BE    PMC250                                                           
         AHI   R2,L'MONTAB                                                      
         B     PMC245                                                           
PMC248   MVC   PTBDAT(2),3(R2)     GET THE MONTH #                              
         MVI   PTBDAT+2,C'/'       MM/                                          
         MVC   PTBDAT+3(5),8(RF)   MM/DD/YY                                     
*                                                                               
PMC250   AHI   RF,1                                                             
         BCT   RE,PMC240           NEXT CHAR                                    
*                                                                               
PMC260   AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BE    EOFINF                                                           
         B     PMC220                                                           
*                                                                               
PMC290   CLI   EOM,C'Y'                                                         
         BE    EOFINF                                                           
         GET   INFILE                                                           
         CLI   0(R1),X'0E'         RELOCATION DICTIONARY REC W/ EOM             
         BE    EOFINF                                                           
         CLI   0(R1),X'01'         CONTROL RECORD                               
         BE    PMC200                                                           
         CLI   0(R1),X'0D'         CONTROL RECORD W/EOM                         
         BE    PMC299                                                           
         CLI   0(R1),X'03'         CONTROL + RLD RECORD                         
         BE    PMC200                                                           
         CLI   0(R1),X'0F'         CONTROL + RLD RECORD W/EOM                   
         BNE   PMC290                                                           
*                                                                               
PMC299   MVI   EOM,C'Y'                                                         
         B     PMC200                                                           
*                                                                               
EOFINF   CLI   FIRST,C'Y'                                                       
         BNE   EOFINF2                                                          
         CLOSE INFILE                                                           
         L     R4,=A(PHASETB2)                                                  
         MVC   INFILE(DCBQ),INFILE2                                             
         MVI   FIRST,C'N'                                                       
         MVI   EOM,C'N'                                                         
         MVI   WHICH,NEW                                                        
         B     PMC010                                                           
EOFINF2  CLOSE INFILE                                                           
*                                                                               
*                                                                               
*&&DO                                                                           
***********************************************************************         
* PRINT OUT THE CSECT LISTING FOR BOTH LOAD MODULES                   *         
***********************************************************************         
         LA    R3,2                                                             
         L     R4,=A(PHASETAB)                                                  
         MVC   TITLE(34),=C'CSECT LISTING OF "OLD" LOAD MODULE'                 
*                                                                               
         MVC   P(15),=CL15'LAST BINDED ON '                                     
         MVI   WORK,X'00'                                                       
         MVC   WORK+1(3),OLDLDATE  X'000YYDDD'                                  
         GOTO1 =V(DATCON),DMCB,(6,WORK),(11,P+16)                               
         MVC   P+26(2),=CL2'AT'                                                 
         UNPK  WORK(7),OLDLTIME                                                 
         MVC   P+30(2),WORK+1                                                   
         MVI   P+32,C':'                                                        
         MVC   P+33(2),WORK+3                                                   
         MVI   P+35,C':'                                                        
         MVC   P+36(2),WORK+5                                                   
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     PMC320                                                           
*                                                                               
PMC300   L     R4,=A(PHASETB2)                                                  
         MVC   TITLE(34),=C'CSECT LISTING OF "NEW" LOAD MODULE'                 
*                                                                               
         MVC   P(15),=CL15'LAST BINDED ON '                                     
         MVI   WORK,X'00'                                                       
         MVC   WORK+1(3),NEWLDATE  X'000YYDDD'                                  
         GOTO1 =V(DATCON),DMCB,(6,WORK),(11,P+16)                               
         MVC   P+26(2),=CL2'AT'                                                 
         UNPK  WORK(7),NEWLTIME                                                 
         MVC   P+30(2),WORK+1                                                   
         MVI   P+32,C':'                                                        
         MVC   P+33(2),WORK+3                                                   
         MVI   P+35,C':'                                                        
         MVC   P+36(2),WORK+5                                                   
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PMC320   MVC   P(L'HEADING),HEADING                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PMC350   GOTO1 =V(HEXOUT),DMCB,PTLOC,PLOC,L'PTLOC                               
         MVC   PNAM,PTNAM                                                       
         GOTO1 =V(HEXOUT),DMCB,PTLEN,PLEN,L'PTLEN                               
         MVC   PBNAM,PTBNAM                                                     
         MVC   PBLEV,PTBLEV                                                     
         MVC   PBDAT,PTBDAT                                                     
         CLI   PTBDAT,C'0'                                                      
         BL    PMC360                                                           
         GOTO1 =V(DATCON),DMCB,(4,PTBDAT),(20,PTBDAT2)                          
PMC360   MVC   PBDAT2,PTBDAT2                                                   
*                                                                               
PRTLNE   GOTO1 =V(PRINTER)                                                      
*                                                                               
         AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   PMC350                                                           
*                                                                               
         MVC   LINE,=PL2'75'                                                    
         BCT   R3,PMC300                                                        
*&&                                                                             
***********************************************************************         
* MERGE 2 PHASE TABLES TOGETHER AND SORT THEM BY CSECT NAMES          *         
***********************************************************************         
         L     R5,=A(PHASETB3)                                                  
         L     R4,=A(PHASETAB)                                                  
         LH    R3,COUNT1                                                        
PMC410   MVC   0(PTQ,R5),0(R4)                                                  
         AHI   R4,PTQ                                                           
         AHI   R5,PTQ                                                           
         BCT   R3,PMC410                                                        
*                                                                               
         L     R4,=A(PHASETB2)                                                  
         LH    R3,COUNT2                                                        
PMC420   MVC   0(PTQ,R5),0(R4)                                                  
         AHI   R4,PTQ                                                           
         AHI   R5,PTQ                                                           
         BCT   R3,PMC420                                                        
*                                                                               
         LH    R6,COUNT1                                                        
         LH    RE,COUNT2                                                        
         AR    R6,RE                                                            
         STH   R6,COUNT3                                                        
         L     R8,=A(PHASETB3)                                                  
         GOTO1 =V(XSORT),DMCB,(R8),(R6),PTQ,9,3                                 
*                                                                               
         MVC   LINE,=PL2'75'                                                    
*                                                                               
*&&DO                                                                           
***********************************************************************         
*PRINT OUT CSECT LISTING FOR BOTH LOAD MODULE                         *         
***********************************************************************         
         MVC   TITLE(60),=CL60'CSECT LISTING OF BOTH LOAD MODULE SORTED+        
                BY CSECT NAME'                                                  
         L     R4,=A(PHASETB3)                                                  
*                                                                               
PMC520   GOTO1 =V(HEXOUT),DMCB,PTLOC,PLOC,L'PTLOC                               
         MVC   PNAM,PTNAM                                                       
         GOTO1 =V(HEXOUT),DMCB,PTLEN,PLEN,L'PTLEN                               
         MVC   PBNAM,PTBNAM                                                     
         MVC   PBLEV,PTBLEV                                                     
         MVC   PBDAT,PTBDAT                                                     
         MVC   PBDAT2,PTBDAT2                                                   
         MVC   PFLAG,PTFLAG                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         AHI   R4,PTQ                                                           
         BCT   R6,PMC520                                                        
*                                                                               
*&&                                                                             
***********************************************************************         
*CHECK FOR ADDED/DELETED/OUTDATED/OUTLEVELED CSECTS                             
***********************************************************************         
         OPEN  (OUTFILE,OUTPUT)                                                 
         MVI   FLAG,0                                                           
         LH    R6,COUNT3                                                        
         L     R4,=A(PHASETB3)                                                  
         LR    R5,R4                                                            
         AHI   R5,PTQ                                                           
*                                                                               
PMC610   CLC   PTFLAG,PTFLAG-PTD(R5)                                            
         BE    PMC620                                                           
         CLC   PTNAM,PTNAM-PTD(R5)                                              
         BE    PMC670                                                           
*                                                                               
PMC620   CLC   PTNAM,SPACES                                                     
         BNH   PMC650                                                           
*                                                                               
         MVC   PNAM,PTNAM                                                       
         MVC   PBNAM,PTBNAM                                                     
         MVC   PBLEV,PTBLEV                                                     
         MVC   PBDAT,PTBDAT                                                     
         MVC   PFLAG,PTFLAG                                                     
         CLI   PTFLAG,OLD                                                       
         BNE   *+14                                                             
         MVC   P+70(20),=CL20'DELETED'                                          
         OI    FLAG,FDEL                                                        
         CLI   PTFLAG,NEW                                                       
         BNE   *+14                                                             
         MVC   P+70(20),=CL20'ADDED'                                            
         OI    FLAG,FADD                                                        
*                                                                               
*DON'T REPORT ADDED/DELETE CSECTS FOR NOW, YYUN, 10/3/03                        
*        PUT   OUTFILE,P                                                        
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
*                                                                               
PMC650   AHI   R4,PTQ                                                           
         AHI   R5,PTQ                                                           
         BCT   R6,PMC610                                                        
         B     PMC690                                                           
*                                                                               
*                                                                               
PMC670   DS    0H                                                               
         MVC   PNAM,PTNAM                                                       
         MVC   PBNAM,PTBNAM                                                     
         MVC   PBLEV,PTBLEV                                                     
         MVC   PBDAT,PTBDAT                                                     
         MVC   PFLAG,PTFLAG                                                     
*                                                                               
         CLI   PTBDAT,C'0'                                                      
         BL    PMC670B                                                          
         GOTO1 =V(DATCON),DMCB,(4,PTBDAT),(20,PTBDAT2)                          
PMC670B  LA    R2,PTBDAT-PTD(R5)                                                
         LA    R3,PTBDAT2-PTD(R5)                                               
         CLI   0(R2),C'0'                                                       
         BL    PMC670X                                                          
         GOTO1 =V(DATCON),DMCB,(4,(R2)),(20,(R3))                               
PMC670X  DS    0H                                                               
*                                                                               
         CLC   PTBDAT2,PTBDAT2-PTD(R5)                                          
         BNE   PMC672                                                           
         MVC   P+70(20),=CL20'THE SAME'                                         
*DON'T REPORT MATCHED (DATE & LEVEL) CSECTS FOR NOW, YYUN, 10/3/03              
*        PUT   OUTFILE,P                                                        
*        GOTO1 =V(PRINTER)                                                      
         B     PMC680                                                           
                                                                                
PMC672   CLC   PTBDAT2,PTBDAT2-PTD(R5)                                          
         BNH   PMC674                                                           
         MVC   P+70(20),=CL20'OUTDATED'                                         
         OI    FLAG,FDATE                                                       
         MVC   EMDTMEM,PTBNAM                                                   
         MVC   EMDTLM,LMNAME                                                    
         PUT   OUTFILE,EMSGDATE                                                 
*        MVC   P(L'EMSGDATE),EMSGDATE                                           
*        GOTO1 =V(PRINTER)                                                      
         B     PMC680                                                           
*                                                                               
PMC674   CLC   PTBLEV,PTBLEV-PTD(R5)                                            
         BNH   PMC680                                                           
         MVC   P+70(20),=CL20'LEVEL STAMP LOW'                                  
         OI    FLAG,FLVL                                                        
*DON'T REPORT LEVEL STAMP PROBLEM CSECTS FOR NOW, YYUN, 10/3/03                 
*        PUT   OUTFILE,P                                                        
*        GOTO1 =V(PRINTER)                                                      
         B     PMC680                                                           
*                                                                               
PMC680   AHI   R4,PTQ*2                                                         
         AHI   R5,PTQ*2                                                         
         BCTR  R6,0                                                             
         BCT   R6,PMC610                                                        
*                                                                               
PMC690   DS    0H                                                               
         CLOSE OUTFILE                                                          
*                                                                               
         TM    FLAG,FDATE          ANY OUTDATED MEMBERS?                        
         BNO   *+8                                                              
         MVI   RETCODE,8           YES - SET RETURN CODE AS 8                   
*                                                                               
EXIT     XBASE  RC=RETCODE,RL=1                                                 
         LTORG                                                                  
*                                                                               
DMCB     DS    8F                                                               
FULL     DS    F                                                                
BYTE     DS    X                                                                
WHICH    DS    X                                                                
FLAG     DS    X                                                                
FDATE    EQU   X'80'                                                            
FLVL     EQU   X'40'                                                            
FADD     EQU   X'20'                                                            
FDEL     EQU   X'10'                                                            
FIRST    DC    C'Y'                                                             
EOM      DC    C'N'                                                             
MONTH    DS    CL3                                                              
HEADING  DC    C' LMOD LOC   CSECT NAME  LENGTH      BOOKNAME   LEVEL  +        
                  DATE'                                                         
*                                                                               
*                                                                               
         DC    C'**DCB**'                                                       
INFILE   DCB   DDNAME=OLD,DSORG=PS,MACRF=GL,EODAD=EOFINF                        
DCBQ     EQU   *-INFILE                                                         
INFILE2  DCB   DDNAME=NEW,DSORG=PS,MACRF=GL,EODAD=EOFINF                        
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,MACRF=PM                                 
*                                                                               
TUNIT    DC    AL2(6)          TEXT UNIT FOR GETTING PDS MEMBER NAME            
DDNAME   DC    CL8'NEW'                                                         
LMNAME   DS    CL10                                                             
*                                                                               
RETCODE  DC    X'0'                                                             
*                                                                               
EMSGDATE DS    CL80                                                             
         ORG   EMSGDATE                                                         
         DC    C'MEMBER '                                                       
EMDTMEM  DC    CL10' '                                                          
         DC    C' '                                                             
         DC    C'IN LOAD MODULE '                                               
EMDTLM   DC    CL8' '                                                           
         DC    C' '                                                             
         DC    C'IS OUTDATED.  RELINK REQUIRED.'                                
         DC    CL8' '                                                           
         ORG                                                                    
*                                                                               
COUNT1   DC    H'0'                                                             
COUNT2   DC    H'0'                                                             
COUNT3   DC    H'0'                                                             
*                                                                               
OLDLDATE DS    PL3                 LAST LINKED DATE (OLD PHASE)                 
OLDLTIME DS    PL4                 LAST LINKED TIME (OLD PHASE)                 
NEWLDATE DS    PL3                 LAST LINKED DATE (NEW PHASE)                 
NEWLTIME DS    PL4                 LAST LINKED TIME (NEW PHASE)                 
*                                                                               
WORK     DS    CL20                                                             
*                                                                               
         DC    C'MONTHTAB'                                                      
MONTAB   DS    0CL5                                                             
         DC    CL3'JAN',CL2'01'                                                 
         DC    CL3'FEB',CL2'02'                                                 
         DC    CL3'MAR',CL2'03'                                                 
         DC    CL3'APR',CL2'04'                                                 
         DC    CL3'MAY',CL2'05'                                                 
         DC    CL3'JUN',CL2'06'                                                 
         DC    CL3'JUL',CL2'07'                                                 
         DC    CL3'AUG',CL2'08'                                                 
         DC    CL3'SEP',CL2'09'                                                 
         DC    CL3'OCT',CL2'10'                                                 
         DC    CL3'NOV',CL2'11'                                                 
         DC    CL3'DEC',CL2'12'                                                 
         DC    X'FF'                                                            
*                                                                               
         DC    C'PHASETAB'                                                      
PHASETAB DS    1000XL(PTQ)                                                      
         DC    C'PHASETB2'                                                      
PHASETB2 DS    1000XL(PTQ)                                                      
         DC    C'PHASETB3'                                                      
PHASETB3 DS    1000XL(PTQ)                                                      
*                                                                               
*                                                                               
PTD      DSECT                                                                  
PTLOC    DS    XL3                                                              
PTNAM    DS    CL8                                                              
PTFLAG   DS    C                                                                
OLD      EQU   C'O'                                                             
NEW      EQU   C'W'                                                             
PTLEN    DS    XL3                                                              
PTTYP    DS    X                   ONLY THE 2ND NIBBLE IS USED                  
SD       EQU   X'00'               1ST NIBBLE IS IGNORED                        
PC       EQU   X'04'                                                            
CM       EQU   X'05'                                                            
PR       EQU   X'06'                                                            
PTBNAM   DS    CL10                                                             
PTBLEV   DS    CL3                                                              
PTBDAT   DS    CL8                 MM/DD/YY                                     
PTBDAT2  DS    CL8                 YYYYMMDD                                     
PTQ      EQU   *-PTD                                                            
*                                                                               
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
         DS    1C                                                               
PLOC     DS    CL6                                                              
         DS    5C                                                               
PNAM     DS    CL8                                                              
         DS    4C                                                               
PLEN     DS    CL6                                                              
*        DS    7C                                                               
*PTYP     DS    CL2                                                             
          DS    6C                                                              
PBNAM    DS    CL10                                                             
         DS    2C                                                               
PBLEV    DS    CL3                                                              
         DS    4C                                                               
PBDAT    DS    CL8                                                              
         DS    2C                                                               
PFLAG    DS    C                                                                
         DS    1C                                                               
PBDAT2   DS    CL8                                                              
         ORG                                                                    
         PRINT OFF                                                              
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDPANLMCMP05/27/05'                                      
         END                                                                    

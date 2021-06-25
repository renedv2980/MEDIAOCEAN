*          DATA SET YYUNLMLST  AT LEVEL 062 AS OF 05/27/05                      
*                                                                               
*THIS PHASE SHOULD BE REPLACED BY DDLMCOMP, 5/27/05                             
*PLEASE STOP USING THIS.                                                        
*ANY FURTHER DEVELOPMENT/UPDATES, PLEASE DO SO IN DDLMCOMP                      
*                                                                               
*                                                                               
*PHASE YYUNLMA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE XSORT                                                                  
*INCLUDE DATCON                                                                 
*                                                                               
*                                                                               
*THIS PROGRAM GENERATES A LISTING OF OBJECT MODULES'S INFO WITHIN A             
*GIVEN LOAD MODULE.  THE PROGRAM IS VERY SENSITIVE TO THE FORMAT OF             
*LOAD MODULE.  THESE ARE THE ASSUMPTIONS:                                       
*(1) THE 1ST RECORD MUST BE X'2080' CESD(S)                                     
*(2) FOLLOWING THAT, ARE IDRS RECORDS  X'80'                                    
*(3) RIGHT AFTER THEM, MUST BE A CONTROL RECORD X'01' OR X'0D'                  
*(4) THEN COMES X'02' OR X'03' OR X'0E' OR X'01' AGAIN CONTROL RECORDS          
*(5) X'02' ARE SKIPPED                                                          
*(6) ONLY X'03' AND X'01' ARE CONSIDERED.                                       
*(7) X'0E' IS ALSO CONSIDERED AND IT IS THE LAST RECORD.                        
*                                                                               
*                                                                               
YYUNLM   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*YYUNLM*,=V(REGSAVE),R9                                        
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         USING PTD,R4                                                           
*                                                                               
         L     R4,=A(PHASETAB)                                                  
*                                                                               
NXTFILE  LR    R8,R4                                                            
         OPEN  INFILE                                                           
         SR    R3,R3                                                            
*                                                                               
*FIND THE 1ST CESD RECORD (X'2080')                                             
*                                                                               
LOOP1    GET   INFILE                                                           
         CLC   0(2,R1),=X'2080'    CESD RECORD                                  
         BNE   LOOP1                                                            
         B     L000                                                             
*                                                                               
NEXT     GET   INFILE                                                           
         CLC   0(2,R1),=X'2080'    CESD RECORD                                  
         BNE   OUT                                                              
*                                                                               
*EXTRACT THE CESD DATA (PHASE NAME, LMOD LOC, LENGTH AND TYPE)                  
*                                                                               
L000     ZICM  R2,6(R1),2          BYTE COUNT OF CESD DATA                      
         SRL   R2,4                /CESD DATA SIZE,16                           
         LA    R1,8(R1)            BEGINNING OF CESD DATA                       
*                                                                               
L005     ZIC   RE,8(R1)            CESD TYPE                                    
         SLL   RE,28                                                            
         SRL   RE,28               GET RID OF 1ST NIBBLE                        
         STC   RE,BYTE                                                          
*                                                                               
         CLI   BYTE,SD                                                          
         BE    L010                                                             
         CLI   BYTE,PC                                                          
         BE    L010                                                             
         CLI   BYTE,CM                                                          
         BE    L010                                                             
         CLI   BYTE,PR                                                          
         BE    L010                                                             
         B     L025                                                             
*                                                                               
L010     MVC   PTNAM,0(R1)                                                      
         MVC   PTTYP,BYTE                                                       
         MVC   PTLOC,9(R1)                                                      
         MVC   PTLEN,13(R1)                                                     
*                                                                               
L020     AHI   R4,PTQ                                                           
         LA    R3,1(R3)                                                         
L025     AHI   R1,16                                                            
         BCT   R2,L005                                                          
         B     NEXT                                                             
*                                                                               
OUT      DS    0H                                                               
         LR    R7,R1                                                            
         GOTO1 =V(XSORT),DMCB,(R8),(R3),PTQ,3,0                                 
         MVC   0(2,R4),=X'FFFF'       END OF TABLE                              
         LR    R1,R7                                                            
         B     HERE                                                             
*                                                                               
*SKIP ALL LOAD MODULE IDRS (X'80')                                              
*                                                                               
LOOP2    GET   INFILE                                                           
HERE     CLI   0(R1),X'80'         IDR RECORD                                   
         BNE   LOOP2X                                                           
*                                                                               
         CLI   2(R1),X'02'         LINKAGE EDITOR DATA                          
         BNE   LOOP2                                                            
*                                  SAVE TIME AND DATE LAST LINKED               
         CLI   FIRST,C'Y'                                                       
         BNE   *+20                                                             
         MVC   OLDLDATE,15(R1)                                                  
         MVC   OLDLTIME,18(R1)                                                  
         B     LOOP2                                                            
*                                                                               
         MVC   NEWLDATE,15(R1)                                                  
         MVC   NEWLTIME,18(R1)                                                  
         B     LOOP2                                                            
*                                                                               
LOOP2X   EQU   *                                                                
         LR    R4,R8                                                            
*                                                                               
*                                                                               
*MUST BE A CONTROL RECORD X'01' OR X'0D'                                        
*                                                                               
         CLI   0(R1),X'01'                                                      
         BE    GETONE                                                           
         CLI   0(R1),X'0D'                                                      
         BE    GETONE                                                           
         MVI   RETCODE,4           SET RETURN CODE AS 4                         
         B     EXIT                                                             
GETONE   EQU   *                                                                
*                                                                               
         L     R5,8(R1)            STORE THE STARTING ADDRESS                   
*                                                                               
BACK     ZICM  RE,13(R1),3         SIZE OF THE TEXT RECORD FOLLOWED             
         L     R6,8(R1)                                                         
*                                                                               
         SR    R6,R5               BEGIN LMOD LOC                               
         LR    R7,R6                                                            
         AR    R7,RE               END LMOD LOC                                 
*                                                                               
         GET   INFILE              READ THE TEXT                                
LOOP3    ZICM  RF,PTLOC,3                                                       
         CR    RF,R7                                                            
         BH    NEXT2                                                            
*                                                                               
         ZICM  RE,PTLEN,3                                                       
         CHI   RE,60                                                            
         BL    SKIP                                                             
*                                                                               
         AR    RF,RE                                                            
         CR    RF,R7                                                            
         BH    NEXT2                                                            
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
LOOP4    CLC   =C'BOOK=',0(RF)                                                  
         BNE   *+10                                                             
         MVC   PTBNAM,5(RF)                                                     
         CLC   =C'LEVEL=',0(RF)                                                 
         BNE   *+10                                                             
         MVC   PTBLEV,6(RF)                                                     
*                                                                               
         CLC   =C'DATE=',0(RF)                                                  
         BNE   LP4_50                                                           
*                                                                               
         CLI   5(RF),C'0'                                                       
         BL    LP4_20                                                           
         MVC   PTBDAT,5(RF)        MM/DD/YY                                     
         B     LP4_50                                                           
*                                                                               
LP4_20   MVC   MONTH,5(RF)                                                      
         L     R2,=A(MONTAB)                                                    
LP4_30   CLC   MONTH,0(R2)                                                      
         BE    LP4_40                                                           
         CLI   0(R2),X'FF'                                                      
         BE    LP4_50                                                           
         AHI   R2,L'MONTAB                                                      
         B     LP4_30                                                           
LP4_40   MVC   PTBDAT(2),3(R2)     GET THE MONTH #                              
         MVI   PTBDAT+2,C'/'       MM/                                          
         MVC   PTBDAT+3(5),8(RF)   MM/DD/YY                                     
*                                                                               
LP4_50   AHI   RF,1                                                             
         BCT   RE,LOOP4                                                         
*                                                                               
SKIP     AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BE    DONE                                                             
         B     LOOP3                                                            
*                                                                               
NEXT2    CLI   EOM,C'Y'                                                         
         BE    DONE                                                             
         GET   INFILE                                                           
         CLI   0(R1),X'0E'                                                      
         BE    DONE                                                             
         CLI   0(R1),X'01'                                                      
         BE    BACK                                                             
         CLI   0(R1),X'0D'                                                      
         BE    MARKEND                                                          
         CLI   0(R1),X'03'                                                      
         BE    BACK                                                             
         CLI   0(R1),X'0F'         CONTROL + RLD RECORD W/EOM                   
         BNE   NEXT2                                                            
MARKEND  MVI   EOM,C'Y'                                                         
         B     BACK                                                             
*                                                                               
DONE     CLI   FIRST,C'Y'                                                       
         BNE   EOFINF2                                                          
EOFINF   CLOSE INFILE                                                           
         L     R4,=A(PHASETB2)                                                  
         MVC   INFILE(DCBQ),INFILE2                                             
         MVI   FIRST,C'N'                                                       
         MVI   EOM,C'N'                                                         
         B     NXTFILE                                                          
EOFINF2  CLOSE INFILE                                                           
*                                                                               
*                                                                               
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
         B     PRTHEAD                                                          
*                                                                               
NXTTAB   L     R4,=A(PHASETB2)                                                  
         MVC   OUTFILE(DCBQ),OUTFILE2                                           
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
PRTHEAD  MVC   P(L'HEADING),HEADING                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         OPEN  (OUTFILE,OUTPUT)                                                 
*                                                                               
LOOP5    GOTO1 =V(HEXOUT),DMCB,PTLOC,PLOC,L'PTLOC                               
         MVC   PNAM,PTNAM                                                       
         GOTO1 =V(HEXOUT),DMCB,PTLEN,PLEN,L'PTLEN                               
         MVC   PBNAM,PTBNAM                                                     
         MVC   PBLEV,PTBLEV                                                     
         MVC   PBDAT,PTBDAT                                                     
*                                                                               
PRTLNE   PUT   OUTFILE,P                                                        
         GOTO1 =V(PRINTER)                                                      
         AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   LOOP5                                                            
*                                                                               
         MVC   LINE,=PL2'75'                                                    
         CLOSE OUTFILE                                                          
         BCT   R3,NXTTAB                                                        
*                                                                               
EXIT     XBASE  RC=RETCODE,RL=1                                                 
         LTORG                                                                  
*                                                                               
DMCB     DS    8F                                                               
BYTE     DS    X                                                                
FIRST    DC    C'Y'                                                             
EOM      DC    C'N'                                                             
RETCODE  DC    X'0'                                                             
MONTH    DS    CL3                                                              
HEADING  DC    C' LMOD LOC   CSECT NAME  LENGTH      BOOKNAME   LEVEL  +        
                  DATE'                                                         
*                                                                               
*                                                                               
         DC    C'**DCB**'                                                       
INFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=GL,EODAD=EOFINF                     
DCBQ     EQU   *-INFILE                                                         
INFILE2  DCB   DDNAME=INFILE2,DSORG=PS,MACRF=GL,EODAD=EOFINF2                   
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,MACRF=PM                                 
OUTFILE2 DCB   DDNAME=OUTFILE2,DSORG=PS,MACRF=PM                                
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
*                                                                               
*                                                                               
PTD      DSECT                                                                  
PTLOC    DS    XL3                                                              
PTNAM    DS    CL8                                                              
PTLEN    DS    XL3                                                              
PTTYP    DS    X                   ONLY THE 2ND NIBBLE IS USED                  
SD       EQU   X'00'               1ST NIBBLE IS IGNORED                        
PC       EQU   X'04'                                                            
CM       EQU   X'05'                                                            
PR       EQU   X'06'                                                            
PTBNAM   DS    CL10                                                             
PTBLEV   DS    CL3                                                              
PTBDAT   DS    CL8                                                              
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
         ORG                                                                    
         PRINT OFF                                                              
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062YYUNLMLST 05/27/05'                                      
         END                                                                    

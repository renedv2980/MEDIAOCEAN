*          DATA SET DDPANLIST  AT LEVEL 006 AS OF 03/23/04                      
*PHASE PANLISTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE XSORT                                                                  
*                                                                               
*                                                                               
*                                                                               
*PART I:                                                                        
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
PANFAC   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*PANFAC*,=V(REGSAVE),R9                                        
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         MVC   TITLE(42),=C'LOAD MODULE/PAN MEMBER LEVEL STAMP COMPARE'         
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         USING PTD,R4                                                           
         L     R4,=A(PHASETAB)                                                  
*                                                                               
         LR    R8,R4                                                            
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
         BE    LOOP2                                                            
*                                                                               
         LR    R4,R8                                                            
*                                                                               
*                                                                               
*MUST BE A CONTROL RECORD X'01' OR X'0D'                                        
*                                                                               
         CLI   0(R1),X'01'                                                      
         BE    *+14                                                             
         CLI   0(R1),X'0D'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
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
         AHI   RF,-60                                                           
         AR    RF,R1                                                            
*                                                                               
*SEARCH FOR BOOK, LEVEL, DATE                                                   
*                                                                               
         LA    RE,60                                                            
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
         CLC   =C'DATE=',0(RF)                                                  
         BNE   *+10                                                             
         MVC   PTBDAT,5(RF)                                                     
         AHI   RF,1                                                             
         BCT   RE,LOOP4                                                         
*                                                                               
SKIP     AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BE    DONE                                                             
         B     LOOP3                                                            
*                                                                               
NEXT2    GET   INFILE                                                           
         CLI   0(R1),X'0E'                                                      
         BE    DONE                                                             
         CLI   0(R1),X'01'                                                      
         BE    BACK                                                             
         CLI   0(R1),X'03'                                                      
         BE    BACK                                                             
         B     NEXT2                                                            
*                                                                               
DONE     DS    0H                                                               
EOFINF   CLOSE INFILE                                                           
*                                                                               
*                                                                               
*PART II:                                                                       
*                                                                               
*BASE ON THAT LIST OF THE PANBOOK NAMES, LOOK UP THE PAN BOOK MEMBER            
*AND COMPARE THE LEVEL STAMP AND DATE.  PRINT OUT A LIST OF BOOK THAT           
*DOESN'T HAVE MATCH LEVEL STAMP OR DATE.                                        
*                                                                               
         L     R4,=A(PHASETAB)                                                  
*                                                                               
P210     DS    0H                                                               
         OC    PTBNAM,PTBNAM                                                    
         BZ    P2NEXT                                                           
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',PTBNAM,RECORD            
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BO    P2NF                                                             
*                                                                               
         MVC   PPTBNAM,PTBNAM                                                   
*                                                                               
         MVC   PPTBLEV,RECORD+40                                                
         CLC   PTBLEV,RECORD+40                                                 
         BNE   P2LEV               LEVEL STAMPS DON'T MATCH                     
*                                                                               
         MVC   PPTBDAT,RECORD+50                                                
         CLC   PTBDAT,RECORD+50                                                 
         BNE   P2DAT               DATES DON'T MATCH                            
         B     P2NEXT                                                           
*                                                                               
P2NF     MVC   P(10),PTBNAM                                                     
         MVI   P+12,C':'                                                        
         MVC   P+13(10),=CL10'NOT FOUND'                                        
*                                                                               
         MVC   PANNAME,SPACES                                                   
         OC    PTBNAM,SPACES                                                    
         LA    RE,PTBNAM+L'PTBNAM-1                                             
*                                  PT TO THE LAST NON-BLANK CHAR                
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         LA    RF,PTBNAM                                                        
         SR    RE,RF               RE = LEN OF BOOK NAME W/O LAST CHAR          
         BCTR  RE,0                -1 FOR EX                                    
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PANNAME(0),PTBNAM                                                
*                                                                               
         MVC   PPTBNAM,PANNAME                                                  
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',PANNAME,RECORD           
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BNO   P2NF20                                                           
*                                                                               
         MVI   P+25,C'|'                                                        
         MVC   P+27(L'PANNAME),PANNAME                                          
         MVI   P+39,C':'                                                        
         MVC   P+40(10),=CL10'NOT FOUND'                                        
         MVC   PPTBDES(10),=CL10'NOT FOUND'                                     
         GOTO1 =V(PRINTER)                                                      
         B     P2NEXT                                                           
*                                                                               
P2NF20   MVC   PPTBLEV,RECORD+40                                                
         CLC   PTBLEV,RECORD+40                                                 
         BE    P2NF30                                                           
*                                  LEVEL STAMPS DON'T MATCH                     
         MVI   P+25,C'|'                                                        
         MVC   P+27(L'PANNAME),PANNAME                                          
         MVI   P+39,C':'                                                        
         MVC   P+40(24),=CL24'LEVEL STAMPS DON''T MATCH'                        
         MVC   PPTBDES(24),=CL24'LEVEL STAMPS DON''T MATCH'                     
         GOTO1 =V(PRINTER)                                                      
         B     P2NEXT                                                           
*                                                                               
P2NF30   MVC   PPTBDAT,RECORD+50                                                
         CLC   PTBDAT,RECORD+50                                                 
         BE    P2NF40                                                           
*                                  DATES DON'T MATCH                            
         MVI   P+25,C'|'                                                        
         MVC   P+27(L'PANNAME),PANNAME                                          
         MVI   P+39,C':'                                                        
         MVC   P+40(17),=CL17'DATES DON''T MATCH'                               
         MVC   PPTBDES(17),=CL17'DATES DON''T MATCH'                            
         GOTO1 =V(PRINTER)                                                      
         B     P2NEXT                                                           
*                                                                               
P2NF40   MVI   P+25,C'|'                                                        
         MVC   P+27(L'PANNAME),PANNAME                                          
         MVI   P+39,C':'                                                        
         MVC   P+40(6),=CL6'MATCH!'                                             
         GOTO1 =V(PRINTER)                                                      
         B     P2NEXT                                                           
*                                                                               
*                                                                               
P2LEV    MVC   P(10),PTBNAM                                                     
         MVI   P+12,C':'                                                        
         MVC   P+13(24),=CL24'LEVEL STAMPS DON''T MATCH'                        
         MVC   PPTBDES(24),=CL24'LEVEL STAMPS DON''T MATCH'                     
         GOTO1 =V(PRINTER)                                                      
         B     P2NEXT                                                           
*                                                                               
P2DAT    MVC   P(10),PTBNAM                                                     
         MVI   P+12,C':'                                                        
         MVC   P+13(17),=CL17'DATES DON''T MATCH'                               
         MVC   PPTBDES(17),=CL17'DATES DON''T MATCH'                            
         GOTO1 =V(PRINTER)                                                      
         B     P2NEXT                                                           
*                                                                               
P2NEXT   GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
         AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   P210                                                             
*                                                                               
         MVC   LINE,=PL2'75'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*                                                                               
*PRINT THE LIST OF THE LOAD MODULES                                             
*                                                                               
         L     R4,=A(PHASETAB)                                                  
         MVC   P(L'HEADING0),HEADING0                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'HEADING),HEADING                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
LOOP5    GOTO1 =V(HEXOUT),DMCB,PTLOC,PLOC,L'PTLOC                               
         MVC   PNAM,PTNAM                                                       
         GOTO1 =V(HEXOUT),DMCB,PTLEN,PLEN,L'PTLEN                               
         MVC   PBNAM,PTBNAM                                                     
         MVC   PBLEV,PTBLEV                                                     
         MVC   PBDAT,PTBDAT                                                     
         MVC   PPBNAM,PPTBNAM                                                   
         MVC   PPBLEV,PPTBLEV                                                   
         MVC   PPBDAT,PPTBDAT                                                   
         MVC   PPBDES,PPTBDES                                                   
         OC    P,SPACES                                                         
*                                                                               
PRTLNE   GOTO1 =V(PRINTER)                                                      
         AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   LOOP5                                                            
*                                                                               
*                                                                               
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
DMCB     DS    8F                                                               
BYTE     DS    X                                                                
RECORD   DS    CL80                                                             
PANNAME  DS    CL10                                                             
HEADING0 DC    C'                                        LOADMOD LEVEL +        
               STAMP              PAN MEMBER LEVEL STAMP               +        
               WARNING'                                                         
HEADING  DC    C' LMOD LOC    CSECT NAME    LENGTH       BOOKNAME  LEVE+        
               L     DATE         BOOKNAME  LEVEL     DATE           --+        
               ---------'                                                       
*                                                                               
*                                                                               
         DC    C'**DCB**'                                                       
INFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=GL,EODAD=EOFINF                     
*                                                                               
         DC    C'PHASETAB'                                                      
PHASETAB DS    1000XL(PTQ)                                                      
         DC    C'********'                                                      
*                                                                               
PTD      DSECT                                                                  
PTLOC    DS    XL3                                                              
PTNAM    DS    CL8                 CSECT NAME                                   
PTLEN    DS    XL3                                                              
PTTYP    DS    X                   ONLY THE 2ND NIBBLE IS USED                  
SD       EQU   X'00'               1ST NIBBLE IS IGNORED                        
PC       EQU   X'04'                                                            
CM       EQU   X'05'                                                            
PR       EQU   X'06'                                                            
PTBNAM   DS    CL10                PAN BOOK NAME (FROM LOAD MODULE)             
PTBLEV   DS    CL3                 LEVEL STAMP   (FROM LOAD MODULE)             
PTBDAT   DS    CL8                 DATE          (FROM LOAD MODULE)             
PPTBNAM  DS    CL10                PAN BOOK NAME (FROM PAN.APPL.LIB)            
PPTBLEV  DS    CL3                 LEVEL STAMP   (FROM PAN.APPL.LIB)            
PPTBDAT  DS    CL8                 DATE          (FROM PAN.APPL.LIB)            
PPTBDES  DS    CL25                DESCRIPTION OF ERROR                         
PTQ      EQU   *-PTD                                                            
*                                                                               
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
         DS    1C                                                               
PLOC     DS    CL6                                                              
         DS    6C                                                               
PNAM     DS    CL8                                                              
         DS    6C                                                               
PLEN     DS    CL6                                                              
         DS    7C                                                               
PBNAM    DS    CL10                                                             
         DS    1C                                                               
PBLEV    DS    CL3                                                              
         DS    4C                                                               
PBDAT    DS    CL8                                                              
         DS    7C                                                               
PPBNAM   DS    CL10                                                             
         DS    1C                                                               
PPBLEV   DS    CL3                                                              
         DS    4C                                                               
PPBDAT   DS    CL8                                                              
         DS    4C                                                               
PPBDES   DS    CL25                                                             
         ORG                                                                    
         PRINT OFF                                                              
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDPANLIST 03/23/04'                                      
         END                                                                    

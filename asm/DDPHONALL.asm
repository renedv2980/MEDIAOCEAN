*          DATA SET DDPHONALL  AT LEVEL 107 AS OF 05/01/02                      
*PHASE PHONALL,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*                                                                               
***********************************************************************         
****************    TURN ON CAPS ON!    *******************************         
***********************************************************************         
*                                                                               
         TITLE 'PHONALL - COMPOSITE DDS TELEPHONE LIST'                         
PHONALL  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**PHON**,=V(REGSAVE),RA,R9                                     
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(255),SPACES                                             
         SPACE 1                                                                
PALL2    GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    PHONEND                                                          
         MVC   P(80),C                                                          
         BAS   RE,SPLAT                                                         
         CLI   C,C'*'              COMMENTS                                     
         BE    PALL2                                                            
         CLC   C(6),=C'TITLE='                                                  
         BNE   PALL3                                                            
         MVC   TITLE,C+6                                                        
         GOTO1 =V(CENTER),DMCB,TITLE,42                                         
         B     PALL2                                                            
         SPACE 1                                                                
PALL3    CLC   C(6),=C'SUBTITLE='                                               
         BNE   PALL4                                                            
         MVC   SUBTITLE,C+9                                                     
         GOTO1 =V(CENTER),DMCB,SUBTITLE,20                                      
         B     PALL2                                                            
         SPACE 1                                                                
PALL4    CLC   C(8),=C'COMPANY='   COMPANY FILTER                               
         BNE   PALL5                                                            
         MVC   COMPANY,C+8                                                      
         B     PALL2                                                            
         SPACE 1                                                                
PALL5    CLC   C(5),=C'DEPT='      DEPARTMENT FILTER                            
         BNE   PALL6                                                            
         MVC   DEPT,C+5                                                         
         B     PALL2                                                            
         SPACE 1                                                                
PALL6    CLC   C(5),=C'SORT='      SORT OPTION                                  
         BNE   PALL8                                                            
         MVC   SORTOPT,C+5                                                      
         B     PALL2                                                            
         SPACE 1                                                                
*                                  FORMATS                                      
*                                  1=SURNAME,FIRST,PROFS,EXT                    
*                                  2=FIRST,SURNAME,PROFS,EXT                    
*                                  3=SURNAME,FIRST,HOME                         
*                                  4=FIRST,SURNAME,HOME                         
*                                  5=EXT,FIRST,SURNAME,PROFS                    
*                                  6=PROFS,FIRST,SURNAME,EXT                    
PALL8    CLC   C(7),=C'FORMAT='                                                 
         BNE   PALL10                                                           
         MVC   FORMAT,C+7                                                       
         B     PALL2                                                            
         SPACE 1                                                                
PALL10   CLC   C(7),=C'FILTER='    FILTER FILTER!                               
         BNE   PALL12                                                           
         MVC   FILTER,C+7                                                       
         B     PALL2                                                            
         SPACE 1                                                                
PALL12   CLC   C(6),=C'BREAK='     BREAK LENGTH                                 
         BNE   PALL14                                                           
         MVC   BREAK,C+6                                                        
         MVC   BREAKTYP,C+8                                                     
         B     PALL2                                                            
         SPACE 1                                                                
PALL14   CLC   C(7),=C'COPIES='    NUMBER OF COPIES                             
         BNE   PALL16                                                           
         PACK  DUB,C+7(2)                                                       
         CVB   R1,DUB                                                           
         STC   R1,COPIES                                                        
         B     PALL2                                                            
         SPACE 1                                                                
PALL16   CLC   C(5),=C'FILE='      FILE CONTROL - MAIN/SUBSIDIARY ETC           
         BNE   PALL17                                                           
         MVC   FILECON,C+5                                                      
         B     PALL2                                                            
         SPACE 1                                                                
PALL17   CLC   C(8),=C'HIDEHOME'   HIDE HOME PHONE IF FILTER 1 = N              
         BNE   PALL18                                                           
         MVI   HIDEHOME,C'Y'                                                    
         B     PALL2                                                            
         SPACE 1                                                                
PALL18   CLC   C(5),=C'EJECT'      SKIP TO NEW PAGE                             
         BNE   PALL20                                                           
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         B     PALL2                                                            
         SPACE 1                                                                
PALL20   CLC   C(3),=C'RUN'        JUST DO IT!                                  
         BNE   PALLERR                                                          
         ZIC   R0,COPIES                                                        
         SPACE 1                                                                
PRUN     BAS   RE,PHONINIT                                                      
         BAS   RE,PHONLIST                                                      
         MVC   LASTDATA,SPACES                                                  
         MVC   LASTDEPT,SPACES                                                  
         MVC   THISDEPT,SPACES                                                  
         BCT   R0,PRUN                                                          
         SPACE 1                                                                
         MVC   TITLE,SPACES                                                     
         MVC   SUBTITLE,SPACES                                                  
         MVC   DEPT,SPACES                                                      
         MVC   COMPANY,SPACES                                                   
         MVC   SORTOPT,SPACES                                                   
         MVC   FILTER,SPACES                                                    
         MVC   FILECON,SPACES                                                   
         MVC   FORMAT,SPACES                                                    
         MVC   BREAK,SPACES                                                     
         MVC   BREAKTYP,SPACES                                                  
         MVI   COPIES,1                                                         
         MVI   HIDEHOME,C'N'                                                    
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         BAS   RE,SETOFF                                                        
         B     PALL2                                                            
         SPACE 1                                                                
PALLERR  MVC   P(25),=C'ABOVE CARD NOT RECOGNIZED'                              
         BAS   RE,SPLAT                                                         
         B     PALL2                                                            
         SPACE 1                                                                
PHONEND  GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
         XBASE                                                                  
         EJECT                                                                  
*              INITIALIZE FOR A LISTING                                         
         SPACE 3                                                                
PHONINIT NTR1                                                                   
         OPEN  (MARKIN,(INPUT))                                                 
         CLI   SORTOPT,C'Y'                                                     
         BNE   PINIT2                                                           
         GOTO1 =V(SORTER),PARA,SORTCARD,RECCARD                                 
         SPACE 1                                                                
PINIT2   GOTO1 =V(DATCON),PARA,(5,0),(0,WORK)                                   
         PACK  DUB,WORK+2(2)       TRANSLATE MONTH                              
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'10'                                                        
         LA    R1,MONTABLE(R1)                                                  
         MVC   DATE,SPACES                                                      
         MVC   DATE(9),1(R1)       UP TO 9 CHARACTER LOWER CASE                 
         ZIC   R2,0(R1)            PICK UP LENGTH                               
         LA    R2,DATE+1(R2)                                                    
         MVC   0(2,R2),=C'19'                                                   
         MVC   2(2,R2),WORK        POP IN YEAR                                  
         GOTO1 =V(CENTER),DMCB,DATE,14                                          
         SPACE 1                                                                
         MVC   ACTCOLS,COLS1                                                    
         MVC   ACTTEXT,TEXT1                                                    
         MVC   ACTSEQ,SEQ1                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'1'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS2                                                    
         MVC   ACTTEXT,TEXT2                                                    
         MVC   ACTSEQ,SEQ2                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'2'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS3                                                    
         MVC   ACTTEXT,TEXT3                                                    
         MVC   ACTSEQ,SEQ3                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'3'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS4                                                    
         MVC   ACTTEXT,TEXT4                                                    
         MVC   ACTSEQ,SEQ4                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'4'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS5                                                    
         MVC   ACTTEXT,TEXT5                                                    
         MVC   ACTSEQ,SEQ5                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'5'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS6                                                    
         MVC   ACTTEXT,TEXT6                                                    
         MVC   ACTSEQ,SEQ6                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'6'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS7                                                    
         MVC   ACTTEXT,TEXT7                                                    
         MVC   ACTSEQ,SEQ7                                                      
         MVI   UP,2                                                             
         CLI   FORMAT,C'7'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS8                                                    
         MVC   ACTTEXT,TEXT8                                                    
         MVC   ACTSEQ,SEQ8                                                      
         MVI   UP,2                                                             
         CLI   FORMAT,C'8'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLS9                                                    
         MVC   ACTTEXT,TEXT9                                                    
         MVC   ACTSEQ,SEQ9                                                      
         MVI   UP,2                                                             
         CLI   FORMAT,C'9'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLSM                                                    
         MVC   ACTTEXT,TEXTM                                                    
         MVC   ACTSEQ,SEQM                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'M'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
         MVC   ACTCOLS,COLSN                                                    
         MVC   ACTTEXT,TEXTN                                                    
         MVC   ACTSEQ,SEQN                                                      
         MVI   UP,3                                                             
         CLI   FORMAT,C'N'                                                      
         BE    PINIT4                                                           
         SPACE 1                                                                
PINIT4   CLC   SUBTITLE,SPACES                                                  
         BE    *+10                                                             
         MVC   ACTSEQ,SUBTITLE                                                  
         MVC   ASTACK,ASTINIT                                                   
         ZIC   R1,UP                                                            
         MH    R1,=H'50'                                                        
         ST    R1,NSTINIT                                                       
         MVC   NSTACK,NSTINIT                                                   
         B     XIT                                                              
         EJECT                                                                  
*              MAIN PROGRAM FLOW                                                
         SPACE 3                                                                
PHONLIST NTR1                                                                   
         SPACE 1                                                                
READ     MVC   IO,SPACES                                                        
         GET   MARKIN,IO                                                        
         CLC   IO(5),=C'FILE='                                                  
         BNE   READ2                                                            
         MVC   FILENOW,IO+5                                                     
         B     READ                                                             
         SPACE 1                                                                
READ2    CLC   FILECON,SPACES                                                   
         BE    READ4                                                            
         CLC   FILECON,FILENOW                                                  
         BNE   READ                                                             
         SPACE 1                                                                
READ4    CLI   HIDEHOME,C'Y'       OPTION TO HIDE HOME PHONE                    
         BNE   READ6                                                            
         CLI   IFILT,C'Y'          UNLESS FILTER 1 IS Y                         
         BE    READ6                                                            
         MVC   IHOME,SPACES                                                     
         SPACE 1                                                                
READ6    BAS   RE,ANYFILT          CHECK ANY FILTERING                          
         BNE   READ                MAY REJECT THIS RECORD                       
         SPACE 1                                                                
         CLI   FORMAT,C'8'                                                      
         BNE   READ8                                                            
         MVC   P(132),IO                                                        
         BAS   RE,SPLAT                                                         
         B     READ                                                             
         SPACE 1                                                                
*                                  FILL TEMP WITH DETAILS                       
READ8    MVC   THISDEPT(3),IDEPT                                                
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 1                                     
         MVC   TEMP+01(14),INAME                                                
         MVC   TEMP+16(12),IFIRST                                               
         MVC   TEMP+29(04),IEXT                                                 
         MVC   TEMP+34(07),IPROFS                                               
         CLI   FORMAT,C'1'                                                      
         BE    FORMDONE                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 2                                     
         MVC   TEMP+01(12),IFIRST                                               
         MVC   TEMP+14(14),INAME                                                
         MVC   TEMP+29(04),IEXT                                                 
         MVC   TEMP+34(07),IPROFS                                               
         CLI   FORMAT,C'2'                                                      
         BE    FORMEXT                                                          
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 3                                     
         MVC   TEMP+01(14),INAME                                                
         MVC   TEMP+16(12),IFIRST                                               
         MVC   TEMP+29(12),IHOME                                                
         CLI   FORMAT,C'3'                                                      
         BE    FORMHOME                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 4                                     
         MVC   TEMP+01(12),IFIRST                                               
         MVC   TEMP+14(14),INAME                                                
         MVC   TEMP+29(12),IHOME                                                
         CLI   FORMAT,C'4'                                                      
         BE    FORMHOME                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 5                                     
         MVC   TEMP+01(04),IEXT                                                 
         MVC   TEMP+06(12),IFIRST                                               
         MVC   TEMP+19(14),INAME                                                
         MVC   TEMP+34(07),IPROFS                                               
         CLI   FORMAT,C'5'                                                      
         BE    FORMEXT                                                          
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 6                                     
         MVC   TEMP+01(07),IPROFS                                               
         MVC   TEMP+09(12),IFIRST                                               
         MVC   TEMP+22(14),INAME                                                
         MVC   TEMP+37(04),IEXT                                                 
         CLI   FORMAT,C'6'                                                      
         BE    FORMPROF                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 7                                     
         MVC   TEMP+01(14),INAME                                                
         MVC   TEMP+16(12),IFIRST                                               
         MVC   TEMP+29(04),IEXT                                                 
         MVC   TEMP+34(07),IPROFS                                               
         MVC   TEMP+42(12),IHOME                                                
         CLI   FORMAT,C'7'                                                      
         BE    FORMDONE                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 8                                     
         MVC   TEMP+01(14),INAME                                                
         MVC   TEMP+16(12),IFIRST                                               
         MVC   TEMP+29(04),IEXT                                                 
         MVC   TEMP+34(05),IPROFS                                               
         MVC   TEMP+40(12),IHOME                                                
         MVC   TEMP+53(03),ICOMP                                                
         MVC   TEMP+57(03),IDEPT                                                
         MVC   TEMP+61(04),IFILT                                                
         CLI   FORMAT,C'8'                                                      
         BE    FORMDONE                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT 9                                     
         MVC   TEMP+01(66),INAME                                                
         CLI   FORMAT,C'9'                                                      
         BE    FORMDONE                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT M                                     
         MVC   TEMP+01(12),INAME                                                
         MVC   TEMP+14(11),IFIRST                                               
         MVC   TEMP+26(04),IEXT                                                 
         MVC   TEMP+31(12),IHOME                                                
         CLI   FORMAT,C'M'                                                      
         BE    FORMDONE                                                         
         SPACE 1                                                                
         MVC   TEMP,SPACES         FORMAT N                                     
         MVC   TEMP+01(11),IFIRST                                               
         MVC   TEMP+13(12),INAME                                                
         MVC   TEMP+26(04),IEXT                                                 
         MVC   TEMP+31(12),IHOME                                                
         CLI   FORMAT,C'N'                                                      
         BE    FORMDONE                                                         
         SPACE 1                                                                
         B     READ                                                             
         SPACE 1                                                                
FORMEXT  CLI   IEXT,C'0'           IF EXT. IS NEEDED                            
         BL    READ                                                             
         B     FORMDONE                                                         
         SPACE 1                                                                
FORMPROF CLI   IPROFS,C' '          IF PROFS IS NEEDED                          
         BNH   READ                                                             
         B     FORMDONE                                                         
         SPACE 1                                                                
FORMHOME CLI   IHOME,C' '          IF HOME IS NEEDED                            
         BNH   READ                                                             
         B     FORMDONE                                                         
         SPACE 1                                                                
FORMDONE CLI   SORTOPT,C'Y'        IF NOT SORTING                               
         BE    DOSORT                                                           
         BAS   RE,STACKEM             PUT INTO STACK                            
         B     READ                                                             
         SPACE 1                                                                
DOSORT   CLI   TEMP+1,C' '                                                      
         BNH   READ                                                             
         MVC   SORTIO,SPACES                                                    
         MVC   SORTIO+8(80),TEMP                                                
         CLI   SORTOPT+1,C'D'                                                   
         BNE   *+10                                                             
         MVC   SORTIO(4),IDEPT                                                  
         CLI   SORTIO+3,C'*'                                                    
         BNE   *+8                                                              
         MVI   SORTIO+3,0                                                       
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTIO                                   
         B     READ                                                             
         SPACE 1                                                                
MARKEOF  CLOSE (MARKIN)                                                         
         CLI   SORTOPT,C'Y'                                                     
         BE    LOOP                                                             
         BAS   RE,PAGE                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CHECK ANY FILTERING                                              
         SPACE 3                                                                
ANYFILT  NTR1                                                                   
         LA    R2,COMPANY          COMPANY                                      
         LA    R3,ICOMP                                                         
         LA    R4,3                                                             
         BAS   RE,TESTFILT                                                      
         BNE   NO                                                               
         SPACE 1                                                                
         LA    R2,DEPT             DEPARTMENT                                   
         LA    R3,IDEPT                                                         
         LA    R4,3                                                             
         BAS   RE,TESTFILT                                                      
         BNE   NO                                                               
         SPACE 1                                                                
         LA    R2,FILTER           FILTERS                                      
         LA    R3,IFILT                                                         
         LA    R4,1                                                             
         LA    R0,4                                                             
         SPACE 1                                                                
ANYFILT2 BAS   RE,TESTFILT                                                      
         BNE   NO                                                               
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,ANYFILT2                                                      
         B     YES                                                              
         SPACE 1                                                                
TESTFILT NTR1                                                                   
         BCTR  R4,0                                                             
         EX    R4,TEST1            FILTER SPACES?                               
         BE    YES                                                              
*****    EX    R4,TEST2            IS DATA SPACES                               
*****    BE    YES                                                              
         EX    R4,TEST3            IS DATA ASTERISKS                            
         BE    YES                                                              
         EX    R4,TEST4            DO THEY AGREE                                
         BE    YES                                                              
         B     NO                                                               
         SPACE 1                                                                
TEST1    CLC   0(0,R2),SPACES                                                   
TEST2    CLC   0(0,R3),SPACES                                                   
TEST3    CLC   0(0,R3),=C'******'                                               
TEST4    CLC   0(0,R2),0(R3)                                                    
         SPACE 1                                                                
NO       LA    R1,R1                                                            
         B     TESTXIT                                                          
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         SPACE 1                                                                
TESTXIT  LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              NOW READ THE RECORDS BACK AND PRINT                              
         SPACE 3                                                                
LOOP     GOTO1 =V(SORTER),PARA,=C'GET'                                          
         L     R6,PARA+4                                                        
         LTR   R6,R6                                                            
         BZ    LOOPEND                                                          
         MVC   THISDEPT(3),0(R6)                                                
         MVC   TEMP,8(R6)                                                       
         BAS   RE,STACKEM                                                       
         B     LOOP                                                             
         SPACE 1                                                                
LOOPEND  BAS   RE,PAGE                                                          
         GOTO1 =V(SORTER),PARA,=C'END'                                          
         B     XIT                                                              
         EJECT                                                                  
*              STACK DATA UP                                                    
         SPACE 3                                                                
STACKEM  NTR1                                                                   
         L     R2,ASTACK                                                        
         L     R3,NSTACK                                                        
         CLI   BREAK,C' '          ANY BREAK SPECIFIED?                         
         BE    STACK10                                                          
         ZIC   R1,BREAK                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         BCTR  R1,0                                                             
         CLI   BREAKTYP,C'D'                                                    
         BE    STDPT                                                            
         CLI   BREAKTYP,C'G'                                                    
         BE    STGAP                                                            
         SPACE 1                                                                
STLET    CLI   TEMP+1,C' '         BREAK OF LETTER                              
         BE    STACK10                                                          
         EX    R1,MATCHEM                                                       
         EX    R1,SAVEM                                                         
         BE    STACK10                                                          
         CH    R3,=H'4'            MUST BE AT LEAST 4 LINES LEFT                
         BNL   STLET2                                                           
         BAS   RE,PAGE                                                          
         L     R2,ASTINIT                                                       
         L     R3,NSTINIT                                                       
         SPACE 1                                                                
STLET2   LA    R2,80(R2)                                                        
         EX    R1,SHOWEM                                                        
         LA    R2,80+80(R2)                                                     
         SH    R3,=H'3'                                                         
         B     STACK10                                                          
         SPACE 1                                                                
STGAP    CLI   TEMP+1,C' '         JUST LEAVE A GAP                             
         BE    STACK10                                                          
         EX    R1,MATCHEM                                                       
         EX    R1,SAVEM                                                         
         BE    STACK10                                                          
         C     R3,NSTINIT          NO GAP IF TOP OF PAGE                        
         BE    STACK10                                                          
         CH    R3,=H'2'            MUST BE AT LEAST 2 LINES LEFT                
         BNL   STGAP2                                                           
         BAS   RE,PAGE                                                          
         L     R2,ASTINIT                                                       
         L     R3,NSTINIT                                                       
         SPACE 1                                                                
STGAP2   LA    R2,80(R2)                                                        
         SH    R3,=H'1'                                                         
         B     STACK10                                                          
         SPACE 1                                                                
STDPT    CLI   THISDEPT,C' '       BREAK OF DEPARTMENT                          
         BNH   STACK10                                                          
         CLC   THISDEPT(3),LASTDEPT                                             
         MVC   LASTDEPT(3),THISDEPT                                             
         BE    STACK10                                                          
         CH    R3,=H'4'            MUST BE AT LEAST 4 LINES LEFT                
         BNL   STDPT2                                                           
         BAS   RE,PAGE                                                          
         L     R2,ASTINIT                                                       
         L     R3,NSTINIT                                                       
         SPACE 1                                                                
STDPT2   LA    R2,80(R2)                                                        
         LA    RF,DPTTABLE         LOOK UP DEPARTMENT TABLE                     
         SPACE 1                                                                
STDPT4   CLC   0(3,RF),THISDEPT                                                 
         BE    STDPT6                                                           
         CLC   0(3,RF),=C'???'                                                  
         BE    STDPT6                                                           
         LA    RF,24(RF)                                                        
         B     STDPT4                                                           
         SPACE 1                                                                
DPTTABLE DS    0F                                                               
         DC    C'ATL     ',CL16'ATLANTA'                                        
         DC    C'BUR     ',CL16'BURBANK'                                        
         DC    C'CHI     ',CL16'CHICAGO'                                        
         DC    C'DAL     ',CL16'DALLAS'                                         
         DC    C'LA      ',CL16'LOS ANGELES'                                    
         DC    C'NY      ',CL16'NEW YORK'                                       
         SPACE 1                                                                
         DC    C'CA      ',CL16'COMM. ADMIN'                                    
         DC    C'CS      ',CL16'CLIENT SERVICE'                                 
         DC    C'EX      ',CL16'EXECUTIVE'                                      
         DC    C'FI      ',CL16'FINANCE'                                        
         DC    C'MKT     ',CL16'MARKETING'                                      
         DC    C'PER     ',CL16'PERSONNEL'                                      
         DC    C'OPS     ',CL16'OPERATIONS'                                     
         DC    C'SYS     ',CL16'SYSTEMS'                                        
         DC    C'????????',CL16'UNKNOWN'                                        
         SPACE                                                                  
STDPT6   MVC   1(16,R2),8(RF)                                                   
         LA    R2,80+80(R2)                                                     
         SH    R3,=H'3'                                                         
         B     STACK10                                                          
         SPACE 1                                                                
MATCHEM  CLC   TEMP+1(0),LASTDATA                                               
SAVEM    MVC   LASTDATA(0),TEMP+1                                               
SHOWEM   MVC   6(0,R2),TEMP+1                                                   
         SPACE 1                                                                
STACK10  MVC   0(80,R2),TEMP                                                    
         LA    R2,80(R2)                                                        
         BCT   R3,STACK12                                                       
         BAS   RE,PAGE                                                          
         L     R2,ASTINIT                                                       
         L     R3,NSTINIT                                                       
         SPACE 1                                                                
STACK12  ST    R2,ASTACK                                                        
         ST    R3,NSTACK                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINT OUT PAGE                                                   
         SPACE 3                                                                
PAGE     NTR1                                                                   
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         BAS   RE,SETHEAD                                                       
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P+01(42),TITLE                                                   
         MVC   P+92(20),ACTSEQ                                                  
         MVC   P+114(14),DATE                                                   
         CLI   UP,3                                                             
         BE    PAGEB                                                            
         MVC   P,SPACES                                                         
         MVC   P+00(42),TITLE                                                   
         MVC   P+93(20),ACTSEQ                                                  
         MVC   P+115(14),DATE                                                   
         SPACE 1                                                                
         CLI   FORMAT,C'7'                                                      
         BNE   PAGEB                                                            
         MVC   P,SPACES                                                         
         MVC   P+00(42),TITLE                                                   
         MVC   P+82(20),ACTSEQ                                                  
         MVC   P+104(14),DATE                                                   
         SPACE 1                                                                
PAGEB    BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         SPACE 1                                                                
         BAS   RE,SETDET                                                        
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P+00(44),ACTTEXT                                                 
         MVC   P+44(44),ACTTEXT                                                 
         MVC   P+88(44),ACTTEXT                                                 
         CLI   FORMAT,C'M'                                                      
         BE    PAGE1                                                            
         CLI   FORMAT,C'N'                                                      
         BE    PAGE1                                                            
         MVC   P,SPACES                                                         
         MVC   P+01(42),ACTTEXT                                                 
         MVC   P+45(42),ACTTEXT                                                 
         MVC   P+89(42),ACTTEXT                                                 
         CLI   UP,3                                                             
         BE    PAGE1                                                            
         MVC   P+00(66),ACTTEXT                                                 
         MVC   P+66(66),ACTTEXT                                                 
         SPACE 1                                                                
PAGE1    BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         SPACE 1                                                                
         LA    R2,STACK                                                         
         LA    R3,50                                                            
         MH    R3,=H'80'                                                        
         LR    R4,R3                                                            
         AR    R3,R2                                                            
         AR    R4,R3                                                            
         LA    R0,50                                                            
         SPACE 1                                                                
PAGE2    MVC   P+00(44),0(R2)                                                   
         MVC   P+44(44),0(R3)                                                   
         MVC   P+88(44),0(R4)                                                   
         CLI   FORMAT,C'M'                                                      
         BE    PAGE3                                                            
         CLI   FORMAT,C'N'                                                      
         BE    PAGE3                                                            
         SPACE 1                                                                
         MVC   P,SPACES                                                         
         MVC   P+01(42),0(R2)                                                   
         MVC   P+45(42),0(R3)                                                   
         MVC   P+89(42),0(R4)                                                   
         CLI   UP,3                                                             
         BE    PAGE3                                                            
         MVC   P,SPACES                                                         
         MVC   P+00(66),0(R2)                                                   
         MVC   P+66(66),0(R3)                                                   
         SPACE 1                                                                
PAGE3    GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         MVC   0(80,R2),SPACES                                                  
         MVC   0(80,R3),SPACES                                                  
         MVC   0(80,R4),SPACES                                                  
         LA    R2,80(R2)                                                        
         LA    R3,80(R3)                                                        
         LA    R4,80(R4)                                                        
         BCT   R0,PAGE2                                                         
         B     XIT                                                              
         SPACE 1                                                                
SETDET   NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,MYROWS                                                   
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+00(44),ACTCOLS                                           
         MVC   BOXCOLS+44(44),ACTCOLS                                           
         MVC   BOXCOLS+88(44),ACTCOLS                                           
         CLI   FORMAT,C'M'                                                      
         BE    XIT                                                              
         CLI   FORMAT,C'N'                                                      
         BE    XIT                                                              
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+01(42),ACTCOLS                                           
         MVC   BOXCOLS+45(42),ACTCOLS                                           
         MVC   BOXCOLS+89(42),ACTCOLS                                           
         CLI   UP,3                                                             
         BE    XIT                                                              
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+00(66),ACTCOLS                                           
         MVC   BOXCOLS+66(66),ACTCOLS                                           
         B     XIT                                                              
         SPACE 1                                                                
SETHEAD  NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS,C'T'                                                     
         MVI   BOXROWS+2,C'B'                                                   
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+00(44),COLSH2                                            
         MVC   BOXCOLS+88(44),COLSH2                                            
         CLI   FORMAT,C'M'                                                      
         BE    XIT                                                              
         CLI   FORMAT,C'N'                                                      
         BE    XIT                                                              
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+01(42),COLSH                                             
         MVC   BOXCOLS+89(42),COLSH                                             
         CLI   UP,3                                                             
         BE    XIT                                                              
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+00(42),COLSH                                             
         MVC   BOXCOLS+90(42),COLSH                                             
         CLI   FORMAT,C'7'                                                      
         BNE   XIT                                                              
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS+00(42),COLSH                                             
         MVC   BOXCOLS+79(42),COLSH                                             
         B     XIT                                                              
         SPACE 1                                                                
SETOFF   NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'N'                                                     
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
C        DS    CL80                                                             
         SPACE 1                                                                
DUB      DS    D                                                                
TITLE    DC    CL50' '                                                          
SUBTITLE DC    CL20' '                                                          
DEPT     DC    CL3' '                                                           
COMPANY  DC    CL3' '                                                           
FILTER   DC    CL4' '                                                           
FORMAT   DC    CL1' '                                                           
BREAK    DC    CL1' '                                                           
BREAKTYP DC    CL1' '                                                           
HIDEHOME DC    CL1'N'                                                           
COPIES   DC    AL1(01)                                                          
FILECON  DC    CL16' '                                                          
FILENOW  DC    CL16' '                                                          
LASTDATA DC    CL8' '                                                           
THISDEPT DC    CL8' '                                                           
LASTDEPT DC    CL8' '                                                           
SORTOPT  DC    CL8' '                                                           
UP       DC    AL1(3)                                                           
NSTACK   DS    F                                                                
ASTACK   DS    A                                                                
NSTINIT  DC    F'150'                                                           
ASTINIT  DC    A(STACK)                                                         
         SPACE 1                                                                
WORK     DS    CL32                                                             
DMCB     DS    6F                                                               
SPACES   DS    CL256                                                            
CITY     DS    CL64                                                             
TEMP     DS    CL80                                                             
SORTIO   DS    CL88                                                             
HEAD     DS    CL8                                                              
IO       DS    0CL255                                                           
         DC    CL01' '                                                          
INAME    DC    CL14' '                                                          
         DC    CL01' '                                                          
IFIRST   DC    CL12' '                                                          
         DC    CL01' '                                                          
IEXT     DC    CL04' '                                                          
         DC    CL01' '                                                          
IPROFS   DC    CL07' '                                                          
         DC    CL01' '                                                          
IHOME    DC    CL12' '                                                          
         DC    CL01' '                                                          
ICOMP    DC    CL03'DDS'                                                        
         DC    CL01' '                                                          
IDEPT    DC    CL03'CS '                                                        
         DC    CL01' '                                                          
IFILT    DC    CL04' '                                                          
         DC    CL255' '                                                         
SORTCARD DC    CL80'SORT FIELDS=(1,64,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=88'                                    
PARA     DS    6F                                                               
PFILL    DS    CL1                                                              
P        DC    CL165' '                                                         
P2       DC    CL165' '                                                         
P3       DC    CL165' '                                                         
         SPACE 1                                                                
COLS1    DC    CL80'L              C            C    C       R'                 
TEXT1    DC    CL80'    Surname      First Name  Ext.  Profs  '                 
SEQ1     DC    C'  Surname Sequence  '                                          
         SPACE 1                                                                
COLS2    DC    CL80'L            C              C    C       R'                 
TEXT2    DC    CL80'  First Name     Surname     Ext.  Profs  '                 
SEQ2     DC    C'First Name Sequence '                                          
         SPACE 1                                                                
COLS3    DC    CL80'L              C            C            R'                 
TEXT3    DC    CL80'    Surname      First Name   Home Phone  '                 
SEQ3     DC    C'  Surname Sequence  '                                          
         SPACE 1                                                                
COLS4    DC    CL80'l            C              C            R'                 
TEXT4    DC    CL80'  First Name     Surname      Home Phone   '                
SEQ4     DC    C'First Name Sequence '                                          
         SPACE 1                                                                
COLS5    DC    CL80'L    C            C              C       R'                 
TEXT5    DC    CL80' Ext.  First Name     Surname      Profs  '                 
SEQ5     DC    C' Extension Sequence '                                          
         SPACE 1                                                                
COLS6    DC    CL80'L       C            C              C    R'                 
TEXT6    DC    CL80'  Profs   First Name     Surname     Ext. '                 
SEQ6     DC    C'Profs Code Sequence '                                          
         SPACE 1                                                                
COLS7    DC    CL42'L              C            C    C       C'                 
         DC    CL38'            R'                                              
TEXT7    DC    CL42'    Surname      First Name  Ext.  Profs  '                 
         DC    CL38' Telephone   '                                              
SEQ7     DC    C'  Surname Sequence  '                                          
         SPACE 1                                                                
COLS8    DC    CL40'L              C            C    C     C'                   
         DC    CL40'            C   C   C    R'                                 
TEXT8    DC    CL40'    Surname      First Name  Ext. Profs '                   
         DC    CL40' Telephone   Cmp Dpt Filt'                                  
SEQ8     DC    C'  Surname Sequence  '                                          
         SPACE 1                                                                
COLS9    DC    CL40'L                                       '                   
         DC    CL40'    C      C             R'                                 
TEXT9    DC    CL40'          Name/Department            Pro'                   
         DC    CL40'fs    Ext.    Telephone   '                                 
SEQ9     DC    C' Subsidiary Listing '                                          
         SPACE 1                                                                
COLSM    DC    CL40'L            C           C    C         '                   
         DC    CL40'   R                      '                                 
TEXTM    DC    CL40'   Surname    First Name  Ext.  Telephon'                   
         DC    CL40'e                         '                                 
SEQM     DC    C'  Surname Sequence   '                                         
         SPACE 1                                                                
COLSN    DC    CL40'L           C            C    C         '                   
         DC    CL40'   R                      '                                 
TEXTN    DC    CL40' First Name    Surname    Ext.  Telephon'                   
         DC    CL40'e                         '                                 
SEQN     DC    C' First name Sequence '                                         
         SPACE 1                                                                
COLSH    DC    CL80'L                                        R'                 
COLSH2   DC    CL80'L                                          R'               
         SPACE 1                                                                
ACTCOLS  DS    CL80                                                             
ACTTEXT  DS    CL80                                                             
ACTSEQ   DS    CL20                                                             
MYROWS   DC    C'     T M'                                                      
         DC    50C' '                                                           
         DC    CL100'B'                                                         
         SPACE 1                                                                
MONTABLE DS    0F                                                               
         DC    AL1(7),CL9'January'                                              
         DC    AL1(8),CL9'February'                                             
         DC    AL1(5),CL9'March'                                                
         DC    AL1(5),CL9'April'                                                
         DC    AL1(3),CL9'May'                                                  
         DC    AL1(4),CL9'June'                                                 
         DC    AL1(4),CL9'July'                                                 
         DC    AL1(6),CL9'August'                                               
         DC    AL1(9),CL9'September'                                            
         DC    AL1(7),CL9'October'                                              
         DC    AL1(8),CL9'November'                                             
         DC    AL1(8),CL9'December'                                             
         SPACE 1                                                                
DATE     DC    CL14' '                                                          
MARKIN   DCB   DSORG=PS,EODAD=MARKEOF,RECFM=FB,BLKSIZE=2640,           X        
               DDNAME=MARKIN,MACRF=(GM),LRECL=132                               
         SPACE 1                                                                
         LTORG                                                                  
STACK    DC    200CL80' '                                                       
       ++INCLUDE DDBIGBOX                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107DDPHONALL 05/01/02'                                      
         END                                                                    

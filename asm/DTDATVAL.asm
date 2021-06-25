*          DATA SET DTDATVAL   AT LEVEL 002 AS OF 07/03/19                      
*PHASE DTDATVAL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATVAO                                                                 
*INCLUDE DATVAN                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'TEST DATVAL'                                                    
**********************************************************************          
* THIS VERSION RELAYS ON A RM OF THE OLD, LIVE AND NEW VERSION                  
* YOU CAN REPORT 1, 2 OR ALL 3 VALUES AT ONCE. THAT IS                          
* LIVE VERSION, OLD VERSION AND NEW VERSION                                     
* RUNS THROUGH 4 ITERATIONS DATVAL'S                                            
* P1(1) = 1 MM/DD/YY                                                            
*         2 MM/DD                                                               
*         3 MM/YY                                                               
*         4 DAY                                                                 
*                                                                               
* FOR EXAMPLE                                                                   
* O 80       <-- X'80' = OLD VERSION, DISPLAY DATE AS NON DDS FORMAT            
* SPECIFY IN COLUMN 1 IF YOU WANT DATVAL VERSION O=OLD, N=NEW, L=LIVE           
* SPECIFY ON SAME LINE WHAT THE P2(1) VALUES ARE                                
*                                                                               
* RUNS THOUGH DATE VARIATIONS STARTING IN COLUMN 3                              
*                                                                               
* FOR EXAMPLE                                                                   
*   10/1/18                                                                     
*                                                                               
**********************************************************************          
         PRINT  GEN                                                             
TEST     CSECT                                                                  
         NBASE 0,*TDTVAL*,WORK                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(28),=CL28'  TEST DATVAL SUBROUTINE'                        
         LA    R4,MID1                                                          
         MVI   OPTLP2,0                                                         
         MVI   OPTOP2,0                                                         
         MVI   OPTNP2,0                                                         
         MVC   OPTLP2C,=C'00'                                                   
         MVC   OPTOP2C,=C'00'                                                   
         MVC   OPTNP2C,=C'00'                                                   
         USING PVALD,R4                                                         
         MVC   PINPUT,=CL12'INPUT DATE'                                         
         MVC   PPARM2,=C'P2'                                                    
*&&US*&& MVC   POUTPUT0,=CL6'M/D/Y'                                             
*&&UK*&& MVC   POUTPUT0,=CL6'D/M/Y'                                             
         MVC   PRTN0(3),=C'RT0'                                                 
         MVC   PLEN0(2),=C'LN'                                                  
*&&US*&& MVC   POUTPUT1,=CL6'M/D  '                                             
*&&UK*&& MVC   POUTPUT1,=CL6'D/M  '                                             
         MVC   PRTN1(3),=C'RT1'                                                 
         MVC   PLEN1(2),=C'LN'                                                  
         MVC   POUTPUT2,=CL6'M/Y  '                                             
         MVC   PRTN2(3),=C'RT2'                                                 
         MVC   PLEN2(2),=C'LN'                                                  
         MVC   POUTPUT3,=CL6'DAY  '                                             
         MVC   PRTN3(3),=C'RT3'                                                 
         MVC   PLEN3(2),=C'LN'                                                  
         DROP  R4                                                               
*                                                                               
NEXT     GOTO1 =V(CARDS),PARAM,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
*                                                                               
NEXT02   MVC   ACTION,C            T DATE                                       
         MVI   BYTE,0                                                           
         XC    PARAM(24),PARAM                                                  
         CLI   ACTION,C'*'         PRINT COMMENT CARD                           
         BE    NEXTCARD                                                         
         CLI   ACTION,C'L'         OPTION P2(1)= VALUE LIVE                     
         BNE   NEXT04                                                           
         MVC   OPTLP2C,C+2                                                      
         GOTO1 =V(HEXIN),DMCB,OPTLP2C,OPTLP2,2,0                                
*                                                                               
NEXT04   CLI   ACTION,C'O'         OPTION P2(1)= VALUE OLD                      
         BNE   NEXT06                                                           
         MVC   OPTOP2C,C+2                                                      
         GOTO1 =V(HEXIN),DMCB,OPTOP2C,OPTOP2,2,0                                
*                                                                               
NEXT06   CLI   ACTION,C'N'         OPTION P2(1)= VALUE NEW                      
         BNE   NEXT10                                                           
         MVC   OPTNP2C,C+2                                                      
         GOTO1 =V(HEXIN),DMCB,OPTNP2C,OPTNP2,2,0                                
*                                                                               
NEXT10   LA    R4,P                                                             
         USING PVALD,R4                                                         
         CLI   ACTION,C' '         IF BLANK DO 0 TO 3                           
         BNE   NEXTCARD                                                         
         MVC   PINPUT,C+2                                                       
                                                                                
**********************************************************************          
*  P2=X'00'    LIVE                                                             
**********************************************************************          
         CLI   OPTLP2,X'FF'                                                     
         BE    SKIPLIVE                                                         
         MVI   PVER,C'L'                                                        
         MVC   PPARM2,OPTLP2C                                                   
         MVI   PACT0,C'0'                                                       
         MVI   PACT0+1,C'='                                                     
         GOTOR =V(DATVAL),PARAM,(0,C+2),(OPTLP2,POUTPUT0)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN0,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN0,1,0                                
*                                                                               
         MVI   PACT1,C'1'                                                       
         MVI   PACT1+1,C'='                                                     
         GOTOR =V(DATVAL),PARAM,(1,C+2),(OPTLP2,POUTPUT1)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN1,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN1,1,0                                
*                                                                               
         MVI   PACT2,C'2'                                                       
         MVI   PACT2+1,C'='                                                     
         GOTOR =V(DATVAL),PARAM,(2,C+2),(OPTLP2,POUTPUT2)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN2,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN2,1,0                                
*                                                                               
         MVI   PACT3,C'3'                                                       
         MVI   PACT3+1,C'='                                                     
         GOTOR =V(DATVAL),PARAM,(3,C+2),(OPTLP2,POUTPUT3)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN3,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN3,1,0                                
         GOTOR =V(PRINTER)                                                      
                                                                                
**********************************************************************          
*  P2=X'00'    OLD                                                              
**********************************************************************          
SKIPLIVE CLI   OPTOP2,X'FF'                                                     
         BE    SKIPOLD                                                          
         MVI   PVER,C'O'                                                        
         MVC   PPARM2,OPTOP2C                                                   
         MVI   PACT0,C'0'                                                       
         MVI   PACT0+1,C'='                                                     
         GOTOR =V(DATVAO),PARAM,(0,C+2),(OPTOP2,POUTPUT0)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN0,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN0,1,0                                
*                                                                               
         MVI   PACT1,C'1'                                                       
         MVI   PACT1+1,C'='                                                     
         GOTOR =V(DATVAO),PARAM,(1,C+2),(OPTOP2,POUTPUT1)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN1,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN1,1,0                                
*                                                                               
         MVI   PACT2,C'2'                                                       
         MVI   PACT2+1,C'='                                                     
         GOTOR =V(DATVAO),PARAM,(2,C+2),(OPTOP2,POUTPUT2)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN2,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN2,1,0                                
*                                                                               
         MVI   PACT3,C'3'                                                       
         MVI   PACT3+1,C'='                                                     
         GOTOR =V(DATVAO),PARAM,(3,C+2),(OPTOP2,POUTPUT3)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN3,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN3,1,0                                
         GOTOR =V(PRINTER)                                                      
                                                                                
**********************************************************************          
*  P2=X'00'    NEW                                                              
**********************************************************************          
SKIPOLD  CLI   OPTNP2,X'FF'                                                     
         BE    NEXT                                                             
         MVI   PVER,C'N'                                                        
         MVC   PPARM2,OPTNP2C                                                   
         MVI   PACT0,C'0'                                                       
         MVI   PACT0+1,C'='                                                     
         GOTOR =V(DATVAN),PARAM,(0,C+2),(OPTNP2,POUTPUT0)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN0,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN0,1,0                                
*                                                                               
         MVI   PACT1,C'1'                                                       
         MVI   PACT1+1,C'='                                                     
         GOTOR =V(DATVAN),PARAM,(1,C+2),(OPTNP2,POUTPUT1)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN1,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN1,1,0                                
*                                                                               
         MVI   PACT2,C'2'                                                       
         MVI   PACT2+1,C'='                                                     
         GOTOR =V(DATVAN),PARAM,(2,C+2),(OPTNP2,POUTPUT2)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN2,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN2,1,0                                
*                                                                               
         MVI   PACT3,C'3'                                                       
         MVI   PACT3+1,C'='                                                     
         GOTOR =V(DATVAN),PARAM,(3,C+2),(OPTNP2,POUTPUT3)                       
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN3,1,0                                
         GOTOR =V(HEXOUT),DMCB,PARAM+3,PLEN3,1,0                                
         GOTOR =V(PRINTER)                                                      
*                                                                               
NEXTCARD GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
ERROR    DS    0H                                                               
*                                                                               
EXIT     XBASE                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
OPTLP2   DS    X                                                                
OPTOP2   DS    X                                                                
OPTNP2   DS    X                                                                
         DS    X                                                                
OPTLP2C  DS    CL2                                                              
OPTOP2C  DS    CL2                                                              
OPTNP2C  DS    CL2                                                              
         DS    CL2                                                              
PARAM    DS    6F                                                               
DMCB     DS    6F                                                               
SAVE     DS    20F                                                              
*                                                                               
DATEIN   DS    CL6                                                              
DAYIN    DS    CL3                                                              
         DS    CL1                                                              
DATEOUT  DS    CL6                                                              
DAYOUT   DS    CL3                                                              
         DS    CL1                                                              
*                                                                               
C        DS    CL80                                                             
         ORG   C                                                                
CDATE    DS    CL8                 DATE (ANY DATVAL FORMAT)                     
         DS    C                                                                
CTH      DS    CL2                 HOUR                                         
CTM      DS    CL2                 MINUTE                                       
CSIGN    DS    C                   SET TO - IF WANT BACKWARDS                   
CHOURS   DS    CL5                 NUMBER OF HOURS TO RETAIN                    
         DS    C                                                                
CCTRY    DS    CL2                 COUNTRY CODE                                 
         DS    C                                                                
CHOL     DS    CL1                 SET TO H IF WANT OFFICIAL HOLIDAY            
         DS    C                                                                
CTALENT  DS    CL1                 SET TO T/U IF WANT TALENT CALENDARS          
         DS    C                                                                
CCALC    DS    CL1                 SET TO C IF WANT CALCULATE                   
         DS    C                                                                
CX       EQU   *                                                                
         ORG                                                                    
*                                                                               
BLOCK    DS    0XL16                                                            
RETC     DS    X                                                                
CTRY     DS    X                                                                
ACTION   DS    C                                                                
HRS      DS    XL2                                                              
FLAG     DS    XL1                                                              
RETI     DS    XL1                                                              
IDY      DS    X                                                                
IDM      DS    X                                                                
IDD      DS    X                                                                
ITH      DS    X                                                                
ITM      DS    X                                                                
ODY      DS    X                                                                
ODM      DS    X                                                                
ODD      DS    X                                                                
OTH      DS    X                                                                
OTM      DS    X                                                                
*                                                                               
WORK     DS    1000D                                                            
         EJECT                                                                  
PVALD    DSECT                                                                  
PINPUT   DS    CL12                                                             
         DS    C                                                                
PPARM2   DS    CL2                                                              
         DS    C                                                                
PVER     DS    C                                                                
         DS    C                                                                
PACT0    DS    C                                                                
         DS    C                                                                
POUTPUT0 DS    CL6                                                              
         DS    C                                                                
PRTN0    DS    CL2                                                              
         DS    C                                                                
PLEN0    DS    CL2                                                              
         DS    C                                                                
PACT1    DS    C                                                                
         DS    C                                                                
POUTPUT1 DS    CL6                                                              
         DS    C                                                                
PRTN1    DS    CL2                                                              
         DS    C                                                                
PLEN1    DS    CL2                                                              
         DS    C                                                                
PACT2    DS    C                                                                
         DS    C                                                                
POUTPUT2 DS    CL6                                                              
         DS    C                                                                
PRTN2    DS    CL2                                                              
         DS    C                                                                
PLEN2    DS    CL2                                                              
         DS    C                                                                
PACT3    DS    C                                                                
         DS    C                                                                
POUTPUT3 DS    CL6                                                              
         DS    C                                                                
PRTN3    DS    CL2                                                              
         DS    C                                                                
PLEN3    DS    CL2                                                              
         DS    C                                                                
PVALLNQ  EQU   PVALD                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DTDATVAL  07/03/19'                                      
         END                                                                    

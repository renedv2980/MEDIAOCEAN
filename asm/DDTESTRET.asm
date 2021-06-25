*          DATA SET DDTESTRET  AT LEVEL 005 AS OF 10/06/16                      
*PHASE GETRETA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE GETRET                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'A TEST PROGRAM TO PROCESS DATES'                                
         PRINT  GEN                                                             
TEST     CSECT                                                                  
         NBASE 0,**TEST**,WORK                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(28),=CL28'  TEST GET RETAIN SUBROUTINE'                    
*                                                                               
NEXT     L     R3,=V(GETRET)       R3=A(GETRET CSECT NAME)                      
         GOTO1 =V(CARDS),PARAM,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
*                                                                               
NEXT1    XC    BLOCK,BLOCK                                                      
         GOTO1 =V(DATVAL),PARAM,C,DATEIN                                        
         CLC   DATEIN,=C'000000'                                                
         BNE   NEXT1A                                                           
         MVC   P+4(20),C                                                        
         MVC   P+0(03),=C'***'                                                  
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
NEXT1A   GOTO1 =V(DATCON),PARAM,(X'00',DATEIN),(X'03',IDY)                      
         GOTO1 =V(GETDAY),PARAM,(X'00',DATEIN),(X'00',DAYIN)                    
*                                                                               
NEXT2    PACK  DUB,CTH             SET HOUR                                     
         CVB   R0,DUB                                                           
         STC   R0,ITH                                                           
         PACK  DUB,CTM             SET MINUTE                                   
         CVB   R0,DUB                                                           
         STC   R0,ITM                                                           
*                                                                               
NEXT3    CLC   CHOURS(4),=C'FFFF'  SET RETAIN HOURS                             
         BE    *+14                                                             
         CLC   CHOURS(4),=C'PERM'                                               
         BNE   NEXT3A                                                           
         MVC   HRS,=X'FFFF'                                                     
         BE    PROCESS                                                          
NEXT3A   PACK  DUB,CHOURS          SET HOURS                                    
         CVB   R0,DUB                                                           
         C     R0,=F'65535'                                                     
         BNH   *+8                                                              
         L     R0,=F'65535'                                                     
         STCM  R0,3,HRS                                                         
         MVI   FLAG,0              SET NEGATIVE REQUIRED                        
         CLI   CHOURS-1,C'-'                                                    
         BNE   *+8                                                              
         MVI   FLAG,X'80'          SET GO BACKWARD FLAG                         
*                                                                               
NEXT4    OC    CCTRY,=C'00'        SET COUNTRY                                  
         PACK  DUB,CCTRY                                                        
         CVB   R0,DUB                                                           
         STC   R0,CTRY                                                          
         LTR   R0,R0                                                            
         BZ    NEXT5                                                            
         OI    FLAG,X'20'          REQUEST RETURN OF COUNTRY CODE               
*                                                                               
NEXT5    CLI   CHOL,C'S'           SET OK TO START ON WEEKEND/HOLIDAY           
         BNE   *+8                                                              
         OI    FLAG,X'40'                                                       
         CLI   CHOL,C'E'           SET OK TO END ON WEEKEND/HOLIDAY             
         BNE   *+8                                                              
         OI    FLAG,X'04'                                                       
         CLI   CHOL,C'B'           SET OK TO START AND END ON HOLIDAY           
         BNE   *+8                                                              
         OI    FLAG,X'44'                                                       
*                                                                               
NEXT6    CLI   CTALENT,C'T'        SET TALENT CALENDAR REQUIRED                 
         BNE   *+8                                                              
         OI    FLAG,X'10'                                                       
         CLI   CTALENT,C'U'        SET TALENT UNION CALENDAR REQUIRED           
         BNE   *+8                                                              
         OI    FLAG,X'01'                                                       
*                                                                               
NEXT7    CLI   CCALC,C'C'          SET CALCULATE ALWAYS                         
         BNE   *+8                                                              
         OI    FLAG,X'08'                                                       
*                                                                               
PROCESS  LA    R1,BLOCK            GO TO GETRET ROUTINE                         
         LR    RF,R3                                                            
         BASR  RE,RF                                                            
         LA    RF,72(RD)           POINT TO GETRET'S W/S                        
         MVC   SAVE(20),0(RF)                                                   
*                                                                               
OUTPUT   LA    R2,P                DISPLAY INPUT CARD DATA                      
         MVC   0(03,R2),DAYIN                                                   
         LA    R2,4(R2)                                                         
         MVC   0(CX-C,R2),C                                                     
         LA    R2,CX-C(R2)                                                      
*                                                                               
OUTPUT1  LA    R2,1(R2)            DISPLAY RETURN CODE IF NON ZERO              
         CLI   RETC,0                                                           
         BE    OUTPUT2                                                          
         MVC   0(1,R2),RETC                                                     
         OI    0(R2),X'F0'                                                      
*                                                                               
OUTPUT2  LA    R2,2(R2)            DISPLAY OUTPUT DATE                          
         GOTO1 =V(DATCON),PARAM,(X'03',ODY),(X'00',DATEOUT)                     
         GOTO1 =V(GETDAY),PARAM,(X'00',DATEOUT),(X'00',DAYOUT)                  
         MVC   0(3,R2),DAYOUT                                                   
         LA    R2,4(R2)                                                         
         GOTO1 =V(DATCON),PARAM,(X'03',ODY),(X'0A',(R2))                        
         LA    R2,8(R2)                                                         
*                                                                               
OUTPUT3  LA    R2,1(R2)            DISPLAY OUTPUT TIME                          
         SR    R0,R0                                                            
         IC    R0,OTH              OUTPUT HOUR                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         IC    R0,OTM              OUTPUT MINUTE                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R2),DUB                                                      
         LA    R2,4(R2)                                                         
*                                                                               
         TM    RETI,X'80'          TEST IF DATE IS WEEKEND (W)                  
         BZ    *+8                                                              
         MVI   1(R2),X'A6'                                                      
         TM    RETI,X'40'          TEST IF DATE IS HOLIDAY (H)                  
         BZ    *+8                                                              
         MVI   1(R2),X'88'                                                      
         TM    RETI,X'C0'          TEST IF DATE IS WEEKEND/HOLIDAY (H)          
         BNO   *+8                                                              
         MVI   1(R2),C'H'                                                       
         TM    RETI,X'20'          TEST IF DATE WAS CALCULATED (C)              
         BZ    *+8                                                              
         MVI   2(R2),X'83'                                                      
         GOTO1 =V(HEXOUT),PARAM,BLOCK,4(R2),16,=C'MIX'                          
*                                                                               
OUTPUT4  GOTO1 =V(PRINTER)                                                      
         ICM   R0,15,=V(GETRET)    GET A(NEW VERSION)                           
         BZ    OUTPUT5             THERE ISNT ONE                               
         CR    R0,R3                                                            
         BE    OUTPUT5                                                          
         LR    R3,R0                                                            
         B     NEXT1               REPEAT FOR NEW VERSION (IF ANY)              
OUTPUT5  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
ERROR    DS    0H                                                               
*                                                                               
EXIT     XBASE                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
PARAM    DS    6F                                                               
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
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDTESTRET 10/06/16'                                      
         END                                                                    

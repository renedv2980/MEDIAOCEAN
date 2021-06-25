*          DATA SET DDCALPRINT AT LEVEL 008 AS OF 05/01/02                      
*PHASE CALPRINT,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE APRINT                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
         TITLE 'CALPRINT - CALENDARS'                                           
CALPRINT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**CAPR**,=V(REGSAVE)                                           
         L     RA,=V(BOXAREA)                                                   
         USING BOXD,RA                                                          
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
READ     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    CAL2                                                             
         CLC   C(2),=C'19'                                                      
         BNE   READ2                                                            
         MVC   YEAR,C+2                                                         
         B     READ                                                             
         SPACE 1                                                                
READ2    MVC   TITLE,C                                                          
         B     READ                                                             
         EJECT                                                                  
*              CONTROL CALENDAR PRINTING                                        
         SPACE 3                                                                
CAL2     LA    R2,1                                                             
         LA    R3,12                                                            
         SPACE 1                                                                
CAL4     EDIT  (R2),(2,MONTH),FILL=0                                            
         BAS   RE,CAL6                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,CAL4                                                          
         EOJ                                                                    
         SPACE 1                                                                
CAL6     NTR1                                                                   
         LA    R2,DAYLINE          CLEAR DAY LINES                              
         LA    R3,6                                                             
         SPACE 1                                                                
CAL8     MVC   0(105,R2),SPACES                                                 
         LA    R2,105(R2)                                                       
         BCT   R3,CAL8                                                          
         MVC   DAY,=C'01'                                                       
         GOTO1 =V(GETDAY),PARA,YEAR,WORK                                        
         ZIC   R2,PARA             DAY NUMBER (1-7)                             
         BCTR  R2,0                                                             
         MH    R2,=H'15'                                                        
         LA    R2,DAYLINE(R2)      POSITION INTO FIRST LINE                     
         SPACE 1                                                                
CAL10    MVC   0(2,R2),DAY         SEED WITH DAY NUMBERS                        
         CLI   0(R2),C'0'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         GOTO1 =V(ADDAY),PARA,YEAR,WORK,1                                       
         CLC   YEAR(4),WORK                                                     
         BNE   CAL12                                                            
         MVC   DAY,WORK+4                                                       
         LA    R2,15(R2)                                                        
         B     CAL10                                                            
         EJECT                                                                  
*              NOW PRINT THE PAGE                                               
         SPACE 3                                                                
CAL12    MVC   BOXCOLS,MYCOLS      SET UP BOXES                                 
         MVC   BOXROWS(60),MYROWS5                                              
         MVC   BOXROWS+60(40),SPACES                                            
         CLC   DAYLINE6,SPACES                                                  
         BE    *+10                                                             
         MVC   BOXROWS(60),MYROWS6                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         SPACE 1                                                                
         MVC   P+7(60),TITLE                                                    
         PACK  DUB,MONTH                                                        
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTAB(R1)                                                    
         MVC   P+95(3),0(R1)                                                    
         MVC   P+99(2),=C'19'                                                   
         MVC   P+101(2),YEAR                                                    
         MVI   SPACING,1                                                        
         BAS   RE,OUT                                                           
         GOTO1 =V(UNDERLIN),PARA,(60,TITLE),(X'1C',P+7)                         
         MVC   P+95(8),=8X'1C'                                                  
         MVI   SPACING,4                                                        
         BAS   RE,OUT                                                           
         MVC   P,DAYHEAD                                                        
         MVI   SPACING,2                                                        
         BAS   RE,OUT                                                           
         LA    R2,DAYLINE                                                       
         LA    R3,5                                                             
         MVI   SPACING,10                                                       
         CLC   DAYLINE6,SPACES                                                  
         BE    CAL14                                                            
         LA    R3,6                                                             
         MVI   SPACING,8                                                        
         SPACE 1                                                                
CAL14    MVC   P+3(105),0(R2)                                                   
         BAS   RE,OUT                                                           
         LA    R2,105(R2)                                                       
         BCT   R3,CAL14                                                         
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
OUT      NTR1                                                                   
         EDIT  (1,SPACING),(2,PARA),FILL=0                                      
         MVC   WORK(2),=C'BL'                                                   
         MVC   WORK+2(2),PARA                                                   
         GOTO1 =V(PRINT),PARA,P-1,WORK                                          
         MVC   P,SPACES                                                         
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
MYCOLS   DC    C'  L              C              C           '                  
         DC    C'   C              C             CC          '                  
         DC    C'    C              R                        '                  
         SPACE 1                                                                
DAYHEAD  DC    C'       MONDAY        TUESDAY       WEDNESDAY'                  
         DC    C'       THURSDAY       FRIDAY         SATURDA'                  
         DC    C'Y       SUNDAY                              '                  
         SPACE 1                                                                
MYROWS5  DC    C'    T M         M         M   '                                
         DC    C'      M         M         B   '                                
         SPACE 1                                                                
MYROWS6  DC    C'    T M       M       M       '                                
         DC    C'M       M       M       B     '                                
         SPACE 1                                                                
PFILL    DC    C' '                                                             
P        DC    CL132' '                                                         
         DC    C' '                                                             
SPACES   DC    CL132' '                                                         
TITLE    DC    CL60'CALENDAR'                                                   
MONTAB   DC    C'JANFEBMARAPRMAYJUN'                                            
         DC    C'JULAUGSEPOCTNOVDEC'                                            
         SPACE 1                                                                
DUB      DS    D                                                                
WORK     DS    CL32                                                             
PARA     DS    6F                                                               
C        DC    CL80' '                                                          
YEAR     DC    C'83'                                                            
MONTH    DC    C'01'                                                            
DAY      DC    C'01'                                                            
         SPACE 1                                                                
DAYLINE  DC    CL105' '                                                         
         DC    CL105' '                                                         
         DC    CL105' '                                                         
         DC    CL105' '                                                         
         DC    CL105' '                                                         
DAYLINE6 DC    CL105' '                                                         
         SPACE 1                                                                
SPACING  DC    X'01'                                                            
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DDCALPRINT05/01/02'                                      
         END                                                                    

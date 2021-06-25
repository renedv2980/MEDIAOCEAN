*          DATA SET DHCALENDAR AT LEVEL 073 AS OF 05/01/02                      
*PHASE CALPRINA,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
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
         SPACE 1                                                                
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
         SPACE 1                                                                
READ     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    CAL2                                                             
         CLC   C(5),=C'YEAR='                                                   
         BNE   READ2                                                            
         MVC   CENTURY(4),C+5                                                   
         PACK  DUB,CENTURY                                                      
         CVB   R1,DUB                                                           
         STC   R1,BCENTURY                                                      
         B     READ                                                             
         SPACE 1                                                                
READ2    CLC   C(6),=C'TITLE='                                                  
         BNE   READ4                                                            
         MVC   TITLE(L'TITLE/2),C+6                                             
         B     READ                                                             
         SPACE 1                                                                
READ4    L     R1,ANXTEVNT                                                      
         MVC   0(L'EVENTBUF,R1),C  MMDD=EVENT                                   
         LA    R1,L'EVENTBUF(R1)                                                
         ST    R1,ANXTEVNT                                                      
         B     READ                                                             
         EJECT                                                                  
*              INITIALIZE                                                       
         SPACE 3                                                                
CAL2     MVC   WORK(L'TITLE/2),TITLE NOW EXPAND TITLE                           
         MVC   TITLE,SPACES                                                     
         LA    R1,TITLE                                                         
         LA    RF,WORK                                                          
         LA    R0,L'TITLE/2                                                     
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,2(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         SPACE 1                                                                
         LA    RF,DAYHEAD+1        SET UP DAY HEADLINE                          
         LA    RE,DAYTAB                                                        
         LA    R1,MYCOLS           AND BOX COLUMNS                              
         LA    R0,7                                                             
READZ    MVC   (PERDAY-L'DAYTAB)/2(L'DAYTAB,RF),0(RE) CENTER DAY NAME           
         MVI   0(R1),C'C'          SET COLUMN                                   
         LA    RF,PERDAY(RF)                                                    
         LA    RE,L'DAYTAB(RE)                                                  
         LA    R1,PERDAY(R1)                                                    
         BCT   R0,READZ                                                         
         MVI   MYCOLS,C'L'         FIX FIRST AND LAST COLUMNS                   
         MVI   0(R1),C'R'                                                       
         EJECT                                                                  
*              CONTROL CALENDAR PRINTING                                        
         SPACE 3                                                                
         LA    R2,1                                                             
         LA    R3,12                                                            
         SPACE 1                                                                
CAL4     EDIT  (R2),(2,MONTH),FILL=0                                            
         BAS   RE,CAL6                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,CAL4                                                          
         XBASE                                                                  
         SPACE 1                                                                
CAL6     NTR1                                                                   
         LA    R2,DAYLINE          CLEAR DAY LINES                              
         LA    R3,6                                                             
         SPACE 1                                                                
CAL8     MVC   0(L'DAYLINE,R2),SPACES                                           
         LA    R2,L'DAYLINE(R2)                                                 
         BCT   R3,CAL8                                                          
         MVC   DAY,=C'01'                                                       
         GOTO1 =V(GETDAY),PARA,(BCENTURY,YEAR),WORK                             
         ZIC   R2,PARA             DAY NUMBER (1=MONDAY)                        
         CLI   PARA,7              IF DAY IS SUNDAY                             
         BNE   *+6                                                              
         XR    R2,R2               WE WANT IT TO BE FIRST                       
         MH    R2,=AL2(PERDAY)                                                  
         LA    R2,DAYLINE(R2)      POSITION INTO FIRST LINE                     
         SPACE 1                                                                
CAL10    MVC   0(2,R2),DAY         SEED WITH DAY NUMBERS                        
         CLI   0(R2),C'0'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         GOTO1 =V(ADDAY),PARA,YEAR,(X'20',WORK),1                               
         CLC   YEAR(4),WORK                                                     
         BNE   CAL12                                                            
         MVC   DAY,WORK+4                                                       
         LA    R2,PERDAY(R2)                                                    
         B     CAL10                                                            
         EJECT                                                                  
*              PRINT HEADINGS FOR MONTH                                         
         SPACE 3                                                                
CAL12    MVC   BOXCOLS(L'MYCOLS),MYCOLS      SET UP BOXES                       
         MVI   BOXWIDTH+3,165      SET WIDTH                                    
         MVC   BOXROWS(60),MYROWS5                                              
         MVC   BOXROWS+60(40),SPACES                                            
         CLC   DAYLINE6,SPACES                                                  
         BE    *+10                                                             
         MVC   BOXROWS(60),MYROWS6                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         SPACE 1                                                                
         MVC   P+7(L'TITLE),TITLE  TITLE TO PRINT LINE                          
         PACK  DUB,MONTH                                                        
         CVB   R2,DUB                                                           
         BCTR  R2,0                                                             
         MH    R2,=AL2(L'MONTAB)                                                
         LA    R2,MONTAB(R2)                                                    
         MVC   P+L'TITLE+8(L'MONTAB),0(R2) MONTH TO PRINT LINE                  
         LA    R1,P+L'TITLE+8+L'MONTAB-1                                        
         CLI   0(R1),C' '          FIND END OF MONTH LITERAL                    
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   3(1,R1),CENTURY     AND MOVE IN YEAR                             
         MVC   5(1,R1),CENTURY+1                                                
         MVC   7(1,R1),YEAR                                                     
         MVC   9(1,R1),YEAR+1                                                   
         MVC   WORK,SPACES         UNDERLINE TITLE & MONTH IN WORK              
         LA    R0,L'TITLE                                                       
         GOTO1 =V(UNDERLIN),PARA,((R0),P+7),(X'BF',WORK+7)                      
         LA    R0,L'MONTAB+10                                                   
         GOTO1 (RF),(R1),((R0),P+L'TITLE+8),(X'BF',WORK+L'TITLE+8)              
         BAS   RE,OUT              PRINT TITLE AND MONTH                        
         SPACE 1                                                                
         MVC   P,WORK                                                           
         MVI   SPACING,4                                                        
         BAS   RE,OUT              AND THEIR UNDERLINES                         
         MVC   P,DAYHEAD                                                        
         MVI   SPACING,2                                                        
         BAS   RE,OUT              PRINT DAY HEADINGS                           
         EJECT                                                                  
*              PRINT BODY OF MONTH                                              
         SPACE 3                                                                
         LA    R2,DAYLINE                                                       
         LA    R3,5                                                             
         LA    R4,10                                                            
         CLC   DAYLINE6,SPACES                                                  
         BE    CAL14                                                            
         LA    R3,6                                                             
         LA    R4,8                                                             
         SPACE 1                                                                
CAL14    MVC   P+3(L'DAYLINE),0(R2) DAY NUMBERS TO PRINT LINE                   
         LR    RF,R4               RF=N'LINES PER BOX                           
         B     *+12                                                             
CAL15    BAS   RE,EVENT            HANDLE EVENTS                                
         BE    CAL16                                                            
         BAS   RE,OUT              PRINT THIS LINE                              
         BCT   RF,CAL15            1 LESS LINE REMAINING IN THIS BOX            
         B     CAL18                                                            
         SPACE 1                                                                
CAL16    STC   RF,SPACING                                                       
         BAS   RE,OUT                                                           
CAL18    LA    R2,L'DAYLINE(R2)                                                 
         BCT   R3,CAL14                                                         
         GOTO1 =V(PRINT),PARA,SPACES-1,=C'BC01'                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOOK UP A DATE IN TABLE                               
         SPACE 1                                                                
*                                  R2=A(CURRENT DAYLINE)                        
EVENT    NTR1                                                                   
         LA    R4,7                                                             
         SPACE 1                                                                
EV1      CLC   0(2,R2),SPACES      NO DAY HERE                                  
         BE    EV4                                                              
         CLI   0(R2),C' '          INSURE WE HAVE 2 DIGITS                      
         BNE   *+8                                                              
         MVI   0(R2),C'0'                                                       
         SPACE 1                                                                
         LA    R3,EVENTBUF         LOOK FOR AN EVENT                            
EV2      CLC   0(L'EVENTBUF,R3),SPACES END-OF-TABLE                             
         BE    EV4                                                              
         CLC   MONTH(2),0(R3)      CHECK MONTH                                  
         BNE   *+14                                                             
         CLC   0(2,R2),2(R3)       AND DAY IN DAYLINE                           
         BE    *+12                                                             
         LA    R3,L'EVENTBUF(R3)   TRY NEXT TABLE ENTRY                         
         B     EV2                                                              
         SPACE 1                                                                
         LA    RF,7                FOUND A MATCH                                
         SR    RF,R4               SET DISPLACEMENT INTO PRINT LINE             
         MH    RF,=AL2(PERDAY)                                                  
         LA    RF,P+1(RF)                                                       
         MVC   0(PERDAY-1,RF),5(R3) AND MOVE IN EVENT                           
         XC    0(L'EVENTBUF,R3),0(R3) INSURE WE DON'T USE ENTRY AGAIN           
         SPACE 1                                                                
EV4      LA    R2,PERDAY(R2)       BUMP TO NEXT DAY                             
         BCT   R4,EV1                                                           
         SPACE 1                                                                
         CLC   P,SPACES            RETURN CC EQUAL IF NOTHING FOUND             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
OUT      NTR1                                                                   
         EDIT  (1,SPACING),(2,PARA),FILL=0,WRK=C                                
         MVC   DUB(2),=C'BL'                                                    
         MVC   DUB+2(2),PARA                                                    
         GOTO1 =V(PRINT),PARA,P-1,DUB                                           
         MVI   SPACING,1           RESET SPACING                                
         MVC   P,SPACES            CLEAR PRINT LINE                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
MYCOLS   DS    CL165' '                                                         
DAYHEAD  DC    CL165' '                                                         
         SPACE 1                                                                
MYROWS5  DC    C'    T M         M         M   '                                
         DC    C'      M         M         B   '                                
         SPACE 1                                                                
MYROWS6  DC    C'    T M       M       M       '                                
         DC    C'M       M       M       B     '                                
         SPACE 1                                                                
         DC    C' '                FILL CHAR                                    
P        DC    CL165' '                                                         
         DC    C' '                FILL CHAR                                    
SPACES   DC    CL165' '                                                         
         SPACE 1                                                                
TITLE    DC    CL120'CALENDAR'                                                  
DUB      DS    D                                                                
WORK     DS    CL165                                                            
PARA     DS    6F                                                               
C        DC    CL80' '                                                          
BCENTURY DC    AL1(19)                                                          
CENTURY  DC    C'19'                                                            
YEAR     DC    C'88'                                                            
MONTH    DC    C'01'                                                            
DAY      DC    C'01'                                                            
ANXTEVNT DC    A(EVENTBUF)                                                      
         SPACE 1                                                                
DAYLINE  DC    CL161' '                                                         
         DC    CL161' '                                                         
         DC    CL161' '                                                         
         DC    CL161' '                                                         
         DC    CL161' '                                                         
DAYLINE6 DC    CL161' '                                                         
PERDAY   EQU   L'DAYLINE/7                                                      
         SPACE 1                                                                
SPACING  DC    X'01'                                                            
         EJECT                                                                  
DAYTAB   DC    CL9'SUNDAY   '                                                   
         DC    CL9'MONDAY   '                                                   
         DC    CL9'TUESDAY  '                                                   
         DC    CL9'WEDNESDAY'                                                   
         DC    CL9'THURSDAY '                                                   
         DC    CL9'FRIDAY   '                                                   
         DC    CL9'SATURDAY '                                                   
         DC    CL9'SUNDAY   '                                                   
         SPACE 1                                                                
MONTAB   DC    CL18'J A N U A R Y'                                              
         DC    CL18'F E B R U A R Y'                                            
         DC    CL18'M A R C H'                                                  
         DC    CL18'A P R I L'                                                  
         DC    CL18'M A Y'                                                      
         DC    CL18'J U N E'                                                    
         DC    CL18'J U L Y'                                                    
         DC    CL18'A U G U S T'                                                
         DC    CL18'S E P T E M B E R'                                          
         DC    CL18'O C T O B E R'                                              
         DC    CL18'N O V E M B E R'                                            
         DC    CL18'D E C E M B E R'                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 3                                                                
EVENTBUF DC    400CL27' '                                                       
         SPACE 3                                                                
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073DHCALENDAR05/01/02'                                      
         END                                                                    

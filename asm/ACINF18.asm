*          DATA SET ACINF18    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T60512A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE OF'                         
T60512   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE OFFICE (CODE) ACCOUNTS INFO PROG          
*              TYPE : ENG = 'OF' / GERMAN = '??'                                
*                                                                               
*                                                                               
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60512)                                               
         DC    A(FILTABLE-T60512)                                               
         DC    A(PREHEADS-T60512)                                               
         DC    A(PRETABLE-T60512)                                               
         DC    A(HEADINGS-T60512)                                               
         DC    A(DATTABLE-T60512)                                               
         DC    A(KNTRYPNT-T60512)                                               
         DC    A(FNTRYPNT-T60512)                                               
         DC    A(DNTRYPNT-T60512)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'01'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'OFFICECODE'                                                 
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(OFFKOFF-OFFRECD)                                             
         DC    AL1(L'OFFKOFF)                                                   
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
*                                                                               
*              NOT USED                                                         
*                                                                               
         SPACE 1                                                                
FILTABLE DS    0H                                                               
*&&DO                                                                           
         DC    CL10'OFFLIST'                                                    
         DC    CL2'OL'                                                          
         DC    CL8'YES'                                                         
         DC    X'40'                                                            
         DC    AL2(AKEY-GWS)                                                    
*        DC    AL2(OFFRSTAT-OFFRECD)                                            
*        DC    AL1(L'OFFRSTAT)                                                  
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'OFFLIST'                                                    
         DC    CL2'OL'                                                          
         DC    CL8'NO'                                                          
         DC    X'40'                                                            
         DC    AL2(AKEY-GWS)                                                    
*        DC    AL2(OFFRSTAT-OFFRECD)                                            
*        DC    AL1(L'OFFRSTAT)                                                  
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
         EJECT                                                                  
*              PRE-HEADING LINE - TO CONTAIN CONSTANT ELEMENTS OF               
*                                 EXPRESSIONS IN FORM X=Y,A=B                   
*                                                                               
*              CL39      C                      SCREEN COLS  2-40               
*              CL39      C                      SCREEN COLS 41-79               
*                                                                               
PREHEADS DC    CL39'                                       '                    
         DC    CL39'                                       '                    
*                                                                               
PREHEDG  DC    CL39'                                       '                    
         DC    CL39'                                       '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
HEADINGS DC    CL39'OFFICE SHORT NAME   ------------ LONG N'                    
         DC    CL39'CODE                                   '                    
         DC    CL39'AME ------------- NUMBER OF LISTS THIS '                    
         DC    CL39'                  OFFICE IS IN         '                    
*EADINGS DC    CL39'OFFICE SHORT        LONG               '                    
*        DC    CL39'CODE   NAME         NAME               '                    
*        DC    CL39'                 NUMBER OF LISTS THIS  '                    
*        DC    CL39'                 OFFICE IS IN          '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       OFFICE CODE                                  
         DC    AL2(OFFKOFF-OFFRECD)                                             
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       SHORT NAME                                   
         DC    AL2(SNMNAME-SNMELD)                                              
         DC    AL1(L'SNMNAME)                                                   
         DC    XL2'FF00'                                                        
         DC    AL1(9)                                                           
         DC    AL1(SNMELQ)         USE THIS TO PASS ELCODE TO DNTRY             
*                                                                               
         DC    AL2(ANAM-GWS)       LONG NAME                                    
         DC    AL2(NAMEREC-NAMELD)                                              
         DC    AL1(L'NAMEREC)                                                   
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(22)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       NO. TIMES USED                               
         DC    AL2(OFIINC-OFIELD)  OFFSET INTO EL                               
         DC    AL1(3)                                                           
         DC    XL2'FF00'                                                        
         DC    AL1(57)             START COL                                    
         DC    AL1(OFIELQ)         ELCODE                                       
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
KNTRYPNT DS    0H                                                               
FNTRYPNT DS    0H                                                               
         EJECT                                                                  
DNTRYPNT DS    0H                                                               
         NMOD1 0,**OFFC**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60512,RB                                                        
         L     R9,BINTAB           A(SOME LOCAL WORKING STORAGE)                
         USING LOCALD,R9                                                        
         USING DATTABD,R5                                                       
         BCTR  R4,0                R4 AT -1 WILL REJECT THE REC                 
         L     R2,AKEY-GWS(RC)     POINT R2 TO THE KEY                          
         USING OFFRECD,R2                                                       
         TM    OFFRSTAT,OFFSLIST   IS THIS REC AN OFFICE LIST                   
         BO    DNTXIT              YES, REJECT                                  
         LA    R2,DATADISP(R2)     POINT TO THE FIRST EL                        
DNT10    CLI   0(R2),0                                                          
         BE    DNTXIT              EL NOT FOUND, REJECT                         
         CLC   0(1,R2),DATTWOUP    ELCODE I WANT HAS BEEN PASSED HERE           
         BE    DNT20                                                            
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DNT10                                                            
         SPACE 1                                                                
DNT20    CLI   0(R2),SNMELQ        IS IT A SHORT NAME FIELD                     
         BNE   DNT40               NO ,ITS # OF TIMES USED                      
         USING SNMELD,R2                                                        
         MVC   WORK(L'SNMNAME),SNMNAME     MOVE SHORT NAME TO WORK              
         SR    R4,R4               TEL ROOT WE USED NO XTRA LINES               
         LA    R2,WORK                                                          
         B     DNTXIT                                                           
*                                                                               
         USING OFIELD,R2                                                        
DNT40    LA    R2,OFIINC           A(# OF LISTS THIS OFF IS IN)                 
         LA    R3,L'OFIINC         LENGTH OF SAME                               
         L     RF,EDITBIN3                                                      
         BASR  RE,RF               EDIT # INTO A THREE BYTE FIELD               
*                                  AT 0(R2)                                     
         CLI   0(R2),X'F0'         LEFT JUSTIFY                                 
         BNE   DNT50                                                            
         MVC   0(2,R2),1(R2)                                                    
         MVI   3(R2),C' '                                                       
         SPACE 1                                                                
         CLI   0(R2),X'F0'                                                      
         BNE   DNT50                                                            
         MVC   0(1,R2),1(R2)                                                    
         MVI   2(R2),C' '          LEAVE LAST DIGIT IF FIRST 2 ARE 0            
         SPACE 1                                                                
DNT50    SR    R4,R4               TELL ROOT TO ACCEPT THIS RECORD              
         B     DNTXIT                                                           
*                                                                               
DNTXIT   EQU   *                                                                
         CLI   DATTWOUP,SNMELQ     ONLY DO THE FOLLOWING ONCE PER REC           
         BNE   DNTX10                                                           
         SPACE 1                                                                
*                                  BUMP LAST CHARACTER OF KEY                   
*                                  BECAUSE 'OF' RECS USE ALL                    
*                                  42 OF KEY, SO READHI GETS NEXT               
         L     R1,AKEY-GWS(RC)     POINT R1 TO THE KEY                          
         ZIC   R7,OFFKOFF-OFFRECD+L'OFFKOFF-1(R1)                               
         LA    R7,1(R7)                                                         
         STC   R7,OFFKOFF-OFFRECD+L'OFFKOFF-1(R1)                               
         SPACE 1                                                                
DNTX10   IC    R3,DATLEN           OUTPUT LENGTH                                
         MVI   DMCB,0              NO ERRORS                                    
         XIT1  REGS=(R2,R4)                                                     
*                                                                               
DATADISP EQU   49                                                               
         LTORG                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
*                                 ACGENFILE                                     
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
LOCALD   DSECT                                                                  
MYDUB    DS    D                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACINF18   05/01/02'                                      
         END                                                                    

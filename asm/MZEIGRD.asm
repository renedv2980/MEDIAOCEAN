*          DATA SET MZEIGRD    AT LEVEL 219 AS OF 10/16/98                      
*PHASE MZEIGRD                                                                  
         TITLE  'CLASS GRADES'                                                  
GRADES   CSECT                                                                  
         PRINT  NOGEN                                                           
         NMOD1  CLASWRKQ-CLASWRKD,GATR500,RR=R2,CLEAR=YES                       
         USING  CLASWRKD,RC           WORKSPACE BASE ADDRESS                    
         L      RA,4(R1)              4(R1) = ADDRESS OF TWA                    
         USING  TB25FFD,RA            TWA BASE ADDRESS                          
         ST     R2,RELO               RELOCATION FACTOR                         
         EJECT                                                                  
*************************************************************                   
*                      MAIN PROGRAM                         *                   
*************************************************************                   
*                                     PRINT TODAY'S DATE                        
         USING  COMFACSD,R9                                                     
         L      R9,16(R1)                                                       
         GOTO1  CDATCON,DMCB,(5,00),(8,GRDDATE)                                 
         OI     GRDDATEH+6,X'80'      TRANSMIT                                  
         DROP   R9                                                              
*                                                                               
         CLI    GRDTOSS,C'Y'          TOSS ?                                    
         BNE    STARTUP                                                         
         CLI    GRDCURV,C'Y'          CURVE ?                                   
         BNE    NOTOSS2                                                         
*                                                                               
STARTUP  MVC    LOWGRD,=F'101'        INITIALIZE LOWEST GRADES                  
         MVC    LOWGRD2,=F'101'                                                 
         XC     GRDHEAD,GRDHEAD       CLEAR ERROR MESSAGE                       
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         XC     COUNTER,COUNTER       RESET COUNTER                             
         XC     GRDTOT,GRDTOT         RESET RUNNING TOTAL                       
         LA     R6,GRDGRD1H           POINT R6 TO 1ST GRADEH                    
LOOP     ZIC    RE,5(R6)              RE = LENGTH OF INPUT                      
         LTR    RE,RE                 NO MORE ENTRIES?                          
         BZ     CLEAR                 CLEAR EXTRA ENTRIES                       
         LH     R3,COUNTER                                                      
         LA     R3,1(R3)              INCREMENT COUNTER                         
         STH    R3,COUNTER                                                      
         BCTR   RE,0                  DECREMENT RE                              
         EX     RE,*+8                                                          
         B      *+10                                                            
         PACK   DUB,8(0,R6)           PACK GRADE                                
         CVB    R0,DUB                                                          
         ST     R0,GRDNUM             HEX REP OF GRADE                          
*                                                                               
*                                     INVALID CHECK                             
*                                                                               
         TM     4(R6),X'08'           CHECK IF VALID NUMERIC                    
         BZ     NONUM                                                           
         CLC    GRDNUM,=F'100'        IS GRADE > 100?                           
         BH     TOOHIGH                                                         
*                                                                               
*                                     RUNNING TOTAL                             
*                                                                               
         L      R0,GRDTOT             LOAD CURRENT TOTAL                        
         A      R0,GRDNUM             ADD NEW GRADE TO TOTAL                    
         ST     R0,GRDTOT             STORE NEW TOTAL                           
************************************************************                    
*              GET HIGH AND LOW SCORES                     *                    
************************************************************                    
         CLC    HIGRD,GRDNUM          IS GRADE HIGHER THAN HIGEST               
         BH     CHKHI2                                                          
         MVC    HIGRD2,HIGRD          STORE HIGHEST AS 2ND HIGHEST              
         MVC    HIGRD,GRDNUM          STORE GRADE AS HIGHEST                    
         B      CHKLOW                                                          
CHKHI2   CLC    HIGRD2,GRDNUM         HIGHER THAN 2ND HIGHEST?                  
         BH     CHKLOW                                                          
         MVC    HIGRD2,GRDNUM                                                   
CHKLOW   CLC    LOWGRD,GRDNUM         IS GRADE LOWER THAN LOWEST                
         BL     CHKLOW2                                                         
         MVC    LOWGRD2,LOWGRD        STORE LOWEST AS 2ND LOWEST                
         MVC    LOWGRD,GRDNUM         STORE GRADE AS LOWEST                     
         B      CURVEYN                                                         
CHKLOW2  CLC    LOWGRD2,GRDNUM        LOWER THAN 2ND LOWEST?                    
         BL     CURVEYN                                                         
         MVC    LOWGRD2,GRDNUM                                                  
************************************************************                    
*                   CURVE YES OR NO?                       *                    
************************************************************                    
CURVEYN  OC     GRDCURV,GRDCURV                                                 
         BZ     LETTER                NO CURVE, DEFAULT=N                       
         CLI    GRDCURV,C'N'                                                    
         BE     LETTER                NO CURVE                                  
         CLI    GRDCURV,C'Y'          YES CURVE                                 
         BNE    INVALID                                                         
         ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 POINT R6 TO NEXT H FIELD                  
         B      ADVANC                                                          
*************************************************************                   
*                   ASSIGN LETTER GRADE                     *                   
*************************************************************                   
LETTER   ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 POINT R6 TO NEXT H FIELD                  
         CLC    GRDNUM,=F'64'         IS GRADE = F?                             
         BH     GRDD                                                            
         MVI    8(R6),C'F'            ASSIGN F AS GRADE                         
         B      TRANSMIT                                                        
GRDD     CLC    GRDNUM,=F'69'         IS GRADE = D?                             
         BH     GRDC                                                            
         MVI    8(R6),C'D'            ASSIGN D AS GRADE                         
         B      TRANSMIT                                                        
GRDC     CLC    GRDNUM,=F'79'         IS GRADE = C?                             
         BH     GRDB                                                            
         MVI    8(R6),C'C'            ASSIGN C AS GRADE                         
         B      TRANSMIT                                                        
GRDB     CLC    GRDNUM,=F'89'         IS GRADE = B?                             
         BH     GRDA                                                            
         MVI    8(R6),C'B'            ASSIGN B AS GRADE                         
         B      TRANSMIT                                                        
GRDA     MVI    8(R6),C'A'            ASSIGN A AS GRADE                         
TRANSMIT OI     6(R6),X'80'           TRANSMIT FIELD                            
*************************************************************                   
*                  ADVANCE TO NEXT STUDENT GRADE            *                   
*************************************************************                   
ADVANC   ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 BUMP R6 TO NEXT FIELD                     
         ZIC    RE,0(R6)                                                        
         AR     R6,RE                 BUMP R6 TO NEXT GRD #                     
         LA     RE,GRDLGRDH           RE = ADDRESS OF LAST GRADE                
         CR     R6,RE                 MAX NUMBER OF STUDENTS?                   
         BNH    LOOP                  IF NOT, GET NEXT GRADES                   
*************************************************************                   
*                 CLEAR REMAINING FIELDS                    *                   
*************************************************************                   
CLEAR    CLC    COUNTER,=H'10'        IF 10 ENTRIES DON'T CLEAR                 
         BE     TOSSEM                                                          
         ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 BUMP R6 TO NEXT FIELD                     
         XC     8(1,R6),8(R6)         CLEAR LETTER GRADE                        
         OI     6(R6),X'80'           TRANSMIT BIT                              
CLEAR2   ZIC    RE,0(R6)                                                        
         AR     R6,RE                 BUMP R6 TO NEXT GRD #                     
         ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 POINT R6 TO NEXT H FIELD                  
         LA     RE,GRDLGRDH           RE = ADDRESS OF LAST GRADE                
         CR     R6,RE                 MAX NUMBER OF STUDENTS?                   
         BH     TOSSEM                                                          
         XC     8(3,R6),8(R6)         CLEAR EXTRA FIELDS                        
         OI     6(R6),X'80'           TRANSMIT                                  
         ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 BUMP R6 TO NEXT FIELD                     
         XC     8(1,R6),8(R6)         CLEAR LETTER GRADE                        
         OI     6(R6),X'80'           TRANSMIT                                  
         B      CLEAR2                                                          
*************************************************************                   
*                 TOSS HI/LOW GRADES                        *                   
*************************************************************                   
TOSSEM   OC     GRDTOSS,GRDTOSS                                                 
         BZ     AVG                   NO TOSS, DEFAULT=N                        
         CLI    GRDTOSS,C'N'                                                    
         BE     AVG                   NO TOSS                                   
         CLI    GRDTOSS,C'Y'          TOSS                                      
         BE     TOSSEM2                                                         
         B      INVALID                                                         
TOSSEM2  CLC    COUNTER,=H'4'         MUST BE AT LEAST 4 TO TOSS                
         BL     NOTOSS                                                          
         MVC    HIGRD,HIGRD2          STORE 2ND HIGHEST AS HIGHEST              
         MVC    LOWGRD,LOWGRD2        STORE 2ND LOWEST AS LOWEST                
*************************************************************                   
*                 CALCULATE AVERAGE                         *                   
*************************************************************                   
AVG      OC     COUNTER,COUNTER       NO ENTRIES?                               
         BZ     EXIT                                                            
         CLC    GRDTOT,=F'0'          IS TOTAL = 0?                             
         BE     ZEROERR                                                         
         SR     R4,R4                 PREPARE FOR DIVIDE                        
         L      R5,GRDTOT                                                       
         LH     R8,COUNTER                                                      
         DR     R4,R8                 DIVIDE GRDTOT BY COUNTER                  
         ST     R5,AVGSCRX            STORE HEX AVGSCR                          
         EDIT   (R5),AVGSCR,ZERO=NOBLANK   OUTPUT TO SCREEN                     
         MVC    GRDAVGH+8(3),AVGSCR   DISPLAY AVG SCORE                         
         OI     GRDAVGH+6,X'80'       TRANSMIT FIELD                            
*************************************************************                   
*                 PREVIOUS AVERAGE                          *                   
*************************************************************                   
         CLC    PRAVG,=C'   '         CHECK IF BLANK                            
         BE     STORE                 IF BLANK                                  
         MVC    GRDPAVGH+8(3),PRAVG                                             
         OI     GRDPAVGH+6,X'80'      TRANSMIT FIELD                            
*************************************************************                   
*                 PERCENT DIFFERENCE                        *                   
*************************************************************                   
         SR     R4,R4                                                           
         L      R5,AVGSCRX            PUT AVGSCR IN R5                          
         S      R5,PRAVGX             SUBTRACT NEW AVG - OLD AVG                
         C      R5,=F'00'             IS ANSWER NEGATIVE?                       
         BNL    MULT                  IF NO, MULTIPLY                           
         LPR    RE,R5                 CHANGE TO POSITIVE                        
         LR     R5,RE                 PUT BACK INTO R5                          
         M      R4,=F'100'            MULTIPLY NEGATIVE NUMBERS                 
         SR     R4,R4                                                           
         D      R4,PRAVGX             DIVIDE BY OLD AVG                         
         LNR    RE,R5                 NEGATE NUMBER                             
         LR     R5,RE                 PUT BACK IN R5                            
         B      PERCENT                                                         
*                                                                               
*                          MULTIPLYING POSITIVE NUMBERS                         
*                                                                               
MULT     M      R4,=F'100'                                                      
         SR     R4,R4                                                           
         D      R4,PRAVGX             DIVIDE BY OLD AVG                         
PERCENT  EDIT   (R5),PERDIFF,MINUS=YES,ZERO=NOBLANK                             
         MVC    GRDPDIFH+8(4),PERDIFF    DISPLAY %DIFF                          
         OI     GRDPDIFH+6,X'80'      TRANSMIT FIELD                            
STORE    MVC    PRAVG,AVGSCR          STORE NEW AVG AS PREV AVG                 
         MVC    PRAVGX,AVGSCRX        STORE HEX VALUES OF AVG                   
*************************************************************                   
*                   GET CURVED LETTER GRADE                 *                   
*************************************************************                   
CURVE    CLI    GRDCURV,C'Y'          CHECK FOR CURVE=Y                         
         BNE    EXIT                                                            
         CLC    COUNTER,=H'2'         ARE THERE AT LEAST 2 GRADES?              
         BL     NOCURV                                                          
*                                     CALCULATE WIDTH OF CURVE                  
         SR     R4,R4                 PREPARE FOR DIVIDE                        
         L      R5,HIGRD                                                        
         S      R5,LOWGRD             SUBTRACT HIGRD-LOWGRD                     
         D      R4,=F'5'              DIVIDE DIFFERENCE BY 5                    
         ST     R5,WIDTH              STORE ANSWER AS WIDTH                     
*                                                                               
         L      R0,LOWGRD                                                       
         A      R0,WIDTH              LOWGRD + WIDTH                            
         ST     R0,HIF                HIF = HIGHEST F                           
         A      R0,WIDTH              HIF + WIDTH = HID                         
         ST     R0,HID                HIGHEST D                                 
         A      R0,WIDTH              HID + WIDTH = HIC                         
         ST     R0,HIC                HIGHEST C                                 
         A      R0,WIDTH              HIC + WIDTH = HIB                         
         ST     R0,HIB                HIGHEST B                                 
         A      R0,WIDTH              HIB + WIDTH = HIA                         
         ST     R0,HIA                HIGHEST A                                 
*************************************************************                   
*                ASSIGN CURVED LETTER GRADE                 *                   
*************************************************************                   
         LH     R8,COUNTER                                                      
         LA     R6,GRDGRD1H           POINT R6 TO FIRST NUM GRADE               
CURVIT   ZIC    RE,5(R6)              RE = LENGTH OF INPUT                      
         LTR    RE,RE                 NO MORE ENTRIES?                          
         BZ     EXIT                  EXIT                                      
*                                                                               
*                                     GET GRDNUM                                
*                                                                               
         BCTR   RE,0                  DECREMENT RE                              
         EX     RE,*+8                                                          
         B      *+10                                                            
         PACK   DUB,8(0,R6)           PACK GRADE                                
         CVB    R0,DUB                                                          
         ST     R0,GRDNUM             HEX REP OF GRADE                          
*                                                                               
         ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 POINT R6 TO NEXT H FIELD                  
         CLI    GRDTOSS,C'Y'          TOSS?                                     
         BNE    GRDF2                 IF NO TOSS                                
*                                                                               
*                                     YES TOSS                                  
*                                                                               
         CLC    GRDNUM,HIGRD2                                                   
         BNH    LOW2CHK                                                         
         MVI    8(R6),C'A'            HIGHEST = A                               
         B      TRANSBIT                                                        
LOW2CHK  CLC    GRDNUM,LOWGRD2                                                  
         BNL    GRDF2                                                           
         MVI    8(R6),C'F'            LOWEST = F                                
         B      TRANSBIT                                                        
GRDF2    CLC    GRDNUM,HIF            IS GRADE = F?                             
         BH     GRDD2                                                           
         MVI    8(R6),C'F'            ASSIGN F AS GRADE                         
         B      TRANSBIT                                                        
GRDD2    CLC    GRDNUM,HID            IS GRADE = D?                             
         BH     GRDC2                                                           
         MVI    8(R6),C'D'            ASSIGN D AS GRADE                         
         B      TRANSBIT                                                        
GRDC2    CLC    GRDNUM,HIC            IS GRADE = C?                             
         BH     GRDB2                                                           
         MVI    8(R6),C'C'            ASSIGN C AS GRADE                         
         B      TRANSBIT                                                        
GRDB2    CLC    GRDNUM,HIB            IS GRADE = B?                             
         BH     GRDA2                                                           
         MVI    8(R6),C'B'            ASSIGN B AS GRADE                         
         B      TRANSBIT                                                        
GRDA2    MVI    8(R6),C'A'            ASSIGN A AS GRADE                         
TRANSBIT OI     6(R6),X'80'           TRANSMIT FIELD                            
*                                                                               
*                  ADVANCE TO NEXT STUDENT GRADE                                
*                                                                               
         ZIC    RE,0(R6)              RE = LENGTH OF FIELD                      
         AR     R6,RE                 BUMP R6 TO NEXT FIELD                     
         ZIC    RE,0(R6)                                                        
         AR     R6,RE                 BUMP R6 TO NEXT GRD #                     
         BCT    R8,CURVIT             REPEAT UNTIL COUNTER=0                    
         B      EXIT                                                            
*************************************************************                   
*                        INVALID                            *                   
*************************************************************                   
INVALID  OI     GRDCURVH+6,X'40'      FLASH CURSOR AT INVALID LETTER            
         MVC    GRDHEAD(29),BADLETR                                             
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
TOOHIGH  OI     6(R6),X'40'           FLASH CURSOR AT INVALID NUM               
         MVC    GRDHEAD(29),HINUM                                               
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
NONUM    OI     6(R6),X'40'           FLASH CURSOR AT NON NUMBER                
         MVC    GRDHEAD(29),NONUMB                                              
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
NOTOSS   OI     GRDTOSSH+6,X'40'      FLASH CURSOR AT TOSS                      
         MVC    GRDHEAD(34),FOURGR                                              
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
NOTOSS2  OI     GRDCURVH+6,X'40'      FLASH CURSOR AT CURVE                     
         MVC    GRDHEAD(27),NOTOSSA                                             
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
NOCURV   OI     GRDCURVH+6,X'40'      FLASH CURSOR AT CURVE                     
         MVC    GRDHEAD(34),NOCURVA                                             
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
ZEROERR  OI     GRDCURVH+6,X'40'      FLASH CURSOR AT CURVE                     
         MVC    GRDHEAD(16),BADZERO                                             
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
         B      EXIT2                                                           
*                                                                               
EXIT     OI     GRDCURVH+6,X'40'      FLASH CURSOR CURVE                        
         MVC    GRDHEAD(27),HELLO                                               
         OI     GRDHEADH+6,X'80'      TRANSMIT FIELD                            
EXIT2    XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*                        CONSTANTS                          *                   
*************************************************************                   
RELO     DS     F                                                               
BADLETR  DC     C'LETTER IS INVALID. TRY AGAIN'                                 
HINUM    DC     C'NUMBER IS TOO HIGH. TRY AGAIN'                                
NONUMB   DC     C'NUMBER NOT NUMERIC. TRY AGAIN'                                
FOURGR   DC     C'TOSSING REQUIRES AT LEAST 4 GRADES'                           
NOTOSSA  DC     C'CANNOT TOSS WITHOUT CURVING'                                  
NOCURVA  DC     C'CURVING REQUIRES AT LEAST 2 GRADES'                           
BADZERO  DC     C'TOTAL GRADES = 0'                                             
HELLO    DC     C'PLEASE ENTER STUDENT GRADES'                                  
*************************************************************                   
*                        WORKSPACE                          *                   
*************************************************************                   
CLASWRKD DSECT                                                                  
WORK     DS     CL17                  USED FOR EDIT MACRO                       
DUB      DS     D                                                               
AVGSCR   DS     CL3                                                             
AVGSCRX  DS     F                                                               
PERDIFF  DS     CL3                                                             
GRDNUM   DS     F                                                               
GRDTOT   DS     F                                                               
COUNTER  DS     H                                                               
HIGRD    DS     F                                                               
HIGRD2   DS     F                                                               
LOWGRD2  DS     F                                                               
LOWGRD   DS     F                                                               
WIDTH    DS     F                                                               
HIF      DS     F                                                               
HID      DS     F                                                               
HIC      DS     F                                                               
HIB      DS     F                                                               
HIA      DS     F                                                               
DMCB     DS     6F                                                              
CLASWRKQ EQU    *                                                               
         EJECT                                                                  
*************************************************************                   
*                           TWA                             *                   
*************************************************************                   
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE GATR5FFD                                                       
PRAVG    DS      CL3                                                            
PRAVGX   DS      F                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'219MZEIGRD   10/16/98'                                      
         END                                                                    

*          DATA SET NEMED62    AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET NEMED62    AT LEVEL 092 AS OF 04/16/99                      
*PHASE T31E62A,+0                                                               
*INCLUDE GETHUT                                                                 
T31E62   TITLE '-   HUT LIST PROGRAM'                                           
T31E62   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**HUPR**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
**    THE TWA IS NEVER REFERENCED, SO RA DOES NOT POINT TO THE TWA              
**     AREA LIKE IT USUALLY DOES. RA IS USED TO POINT TO ARGS FROM              
**     EDIT.                                                                    
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     RA,ANETWS2          USE RA FOR ARGS FROM EDIT                    
         USING HUTCOM,RA                                                        
         ST    R2,RELO                                                          
         L     R1,=V(GETHUT)       RELOCATE GETHUT                              
         AR    R1,R2                                                            
         ST    R1,GETHUT                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         CLI   TAPOPT,C'Y'                                                      
         BNE   HUT1                                                             
         OPEN  (OTAPE,(OUTPUT))                                                 
         L     R2,=A(SORTBUFF)                                                  
         A     R2,RELO                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(40,(R2))                           
         SPACE 1                                                                
HUT1     SR    R2,R2                                                            
         LA    R3,9                                                             
         CLI   SOURCE,C'-'         OPTION NOT TO GET LISTING                    
         BE    HUT3                                                             
         CLI   DAYFILT,X'FF'       SET UP FOR DAY=ALL                           
         BE    HUT2                                                             
         IC    R2,DAYFILT                                                       
         LA    R3,1                                                             
         SPACE 2                                                                
HUT2     STC   R2,DAYFILT                                                       
         BAS   RE,DAY              HANDLE FOR 1 DAY                             
         LA    R2,1(R2)                                                         
         BCT   R3,HUT2                                                          
         SPACE 1                                                                
HUT3     BAS   RE,HOLILIST         POSSIBLE HOLIDAY HUT LIST                    
         BAS   RE,SCHMLIST         AND SCHEME LISTING                           
         CLI   TAPOPT,C'Y'                                                      
         BNE   XIT                                                              
         EJECT                                                                  
*              SORT HUT RECORDS AND WRITE OUTPUT TAPE                           
         SPACE 1                                                                
HUT4     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    HUT6                                                             
         MVC   TAPAREA(144),0(R2)                                               
         PUT   OTAPE,TAPHEAD                                                    
         CLI   TAPDIT,C'N'                                                      
         BE    HUT4                                                             
         MVC   P,TAPAREA                                                        
         GOTO1 HEXOUT,DMCB,P,P+132,132,=C'SEP'                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     HUT4                                                             
         SPACE 1                                                                
HUT6     CLOSE (OTAPE)                                                          
         B     XIT                                                              
         SPACE 1                                                                
TAPOPT   DC    C'N'                Y IS USED WHEN EMITTING TAPE                 
TAPDIT   DC    C'N'                Y IS USED WHEN EMITTING TAPE                 
*                                  (SEE ALSO USE OF GHSCHEME, BELOW)            
         EJECT                                                                  
*              CONTROL THE INTERFACE TO GETHUT FOR EACH DAY                     
         SPACE 1                                                                
DAY      NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         LA    RE,BUCKETS          CLEAR BUCKETS                                
         LA    RF,48               WE NEED 48 LINES                             
         MH    RF,=H'156'          52 WEEKS PER LINE AND                        
*                                  3 BYTES EACH (1/HIT PLUS 2/COUNTER)          
         XCEF                                                                   
         SPACE 1                                                                
         LA    R6,GHAREA                                                        
         USING GETHUTD,R6                                                       
         XC    GHAREA,GHAREA       CLEAR INTERFACE AREA                         
*                                  SET UP PARAMETERS                            
         CLI   SOURCE,C'C'         FIRST SOURCE - CONTROL                       
         BE    DAY2                                                             
         MVI   GHSCHEME,X'FE'      READ FROM DEMO FILE                          
*                                  (X'FF' IS USED WHEN EMITTING TAPE)           
         MVC   GH52,OPT52          52 WEEK OPTION                               
         MVC   GHBKTYPE,BOOKTYPE   DIARY (X'00') OR I(NTEGRATED)                
*                                  OR A(SCRIBED) OR C(ONFORMED)                 
         MVC   GHAGY,NBSELAGY      FOR AGENCY LOCKOUT                           
         CLI   SOURCE,C'N'                                                      
         BE    DAY2                                                             
         MVC   GHSCHEME,SCHEME     ELSE READ AGENCY FILE                        
         MVC   GHAGYMED,NBACTAM                                                 
*                                                                               
DAY2     MVC   GHREPDAY,DAYFILT                                                 
         MVC   GHMILTIM,TIMFILT                                                 
         MVI   GHAVE,C'W'                                                       
         ZIC   R3,ENDYEAR                                                       
         LA    R3,1(R3)                                                         
         ZIC   R2,YEARFILT                                                      
         SR    R3,R2                                                            
         STC   R3,GHNYEARS         PASS # OF YEARS TO GETHUT                    
         STH   R3,NYEARS                                                        
*                                                                               
         MVC   GHYEAR,ENDYEAR      PASS END YEAR TO GETHUT                      
         MVI   GHZERO,X'FF'        RETURN ZERO HUTS                             
         MVC   GHBOOKS+2(1),ENDYEAR                                             
         MVI   GHBOOKS+3,52                                                     
         MVC   GHBOOKS(1),YEARFILT                                              
         MVC   GHBOOKS+1(1),Q1LIST+2 QUARTER 1 REQUSTED                         
         CLI   QRTFILT+0,C'Y'                                                   
         BE    DAY4                                                             
         MVC   GHBOOKS+1(1),Q2LIST+2 QUARTER 2 REQUSTED                         
         CLI   QRTFILT+1,C'Y'                                                   
         BE    DAY4                                                             
         MVC   GHBOOKS+1(1),Q3LIST+2 QUARTER 3 REQUSTED                         
         CLI   QRTFILT+2,C'Y'                                                   
         BE    DAY4                                                             
         MVC   GHBOOKS+1(1),Q4LIST+2 QUARTER 4 REQUSTED                         
         SPACE 1                                                                
DAY4     MVC   GHCOMFCS,ACOMFACS   SOME ADDRESSES                               
         LA    RE,HUTHOOK                                                       
         ST    RE,GHHOOK                                                        
         GOTO1 GETHUT,DMCB,(R6)                                                 
         BAS   RE,PRINTDAY                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A REPORT FOR THE DAY                                       
         SPACE 2                                                                
PRINTDAY NTR1                                                                   
         LA    R2,BUCKETS                                                       
         LA    R3,TIMLIST          SET UP FOR 48 HALF HOURS                     
         LA    R5,74               START 1/4 HOUR                               
         LA    R0,48                                                            
         SPACE 1                                                                
PD2      OC    0(156,R2),0(R2)                                                  
         BZ    PD4                                                              
         MVC   P+2(6),0(R3)                                                     
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   TAPOPT,C'Y'                                                      
         BNE   PD4                                                              
         BAS   RE,PUTSORT                                                       
         SPACE 1                                                                
PD4      LA    R2,156(R2)                                                       
         LA    R3,6(R3)                                                         
         LA    R5,2(R5)                                                         
         CH    R5,=H'96'                                                        
         BNE   *+6                                                              
         SR    R5,R5                                                            
         BCT   R0,PD2                                                           
         B     XIT                                                              
         SPACE 1                                                                
TIMLIST  DC    C'12.30A'                                                        
         DC    C' 1.00A 1.30A'                                                  
         DC    C' 2.00A 2.30A'                                                  
         DC    C' 3.00A 3.30A'                                                  
         DC    C' 4.00A 4.30A'                                                  
         DC    C' 5.00A 5.30A'                                                  
         DC    C' 6.00A 6.30A'                                                  
         DC    C' 7.00A 7.30A'                                                  
         DC    C' 8.00A 8.30A'                                                  
         DC    C' 9.00A 9.30A'                                                  
         DC    C'10.00A10.30A'                                                  
         DC    C'11.00A11.30A'                                                  
         DC    C'12NOON'                                                        
         DC    C'12.30P'                                                        
         DC    C' 1.00P 1.30P'                                                  
         DC    C' 2.00P 2.30P'                                                  
         DC    C' 3.00P 3.30P'                                                  
         DC    C' 4.00P 4.30P'                                                  
         DC    C' 5.00P 5.30P'                                                  
         DC    C' 6.00P 6.30P'                                                  
         DC    C' 7.00P 7.30P'                                                  
         DC    C' 8.00P 8.30P'                                                  
         DC    C' 9.00P 9.30P'                                                  
         DC    C'10.00P10.30P'                                                  
         DC    C'11.00P11.30P'                                                  
         DC    C'12 MID'                                                        
         EJECT                                                                  
*              POST INDIVIDUAL HUTS INTO BUCKETS                                
         SPACE 3                                                                
HUTHOOK  NTR1                                                                   
         LA    R6,GHAREA                                                        
         USING GETHUTD,R6                                                       
         OC    GHHOOKHT,GHHOOKHT   HAS A ZERO HUT BEEN RTURNED                  
         BZ    XIT                                                              
         LH    R1,GHHOOKTM         FIGURE OUT HALF HOUR TIME                    
         SR    R0,R0               FROM MILITARY TIME                           
         D     R0,=F'100'                                                       
         SLL   R1,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'156'                                                       
         LA    R2,BUCKETS                                                       
         AR    R2,R1               R2 POSITIONED TO RIGHT LINE                  
         SPACE 1                                                                
         ZIC   R1,GHHOOKWK                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         AR    R2,R1               NOW DISPLACED FOR SPECIFIC WEEK              
         SPACE 1                                                                
         ZIC   R1,0(R2)            NOW ADD TO REPORTED YEARS                    
         LA    R1,1(R1)                                                         
         STC   R1,0(R2)                                                         
         SPACE 1                                                                
         LH    R1,1(R2)            NOW ADD IN THIS HUT                          
         AH    R1,GHHOOKHT                                                      
         STH   R1,1(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT A LINE OF PRINT                                           
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         LA    R4,P                                                             
         CLI   FLAVOR,C'W'                                                      
         BE    FORMAT1                                                          
         SPACE 1                                                                
         LA    R3,MONLIST          MONTHLY                                      
         BAS   RE,FORMLINE                                                      
         B     XIT                                                              
         SPACE 1                                                                
FORMAT1  CLI   QRTFILT,C'Y'        QUARTER 1 FOR WEEKLY                         
         BNE   FORMAT2                                                          
         MVC   12(3,R4),=C'JAN'                                                 
         MVC   50(3,R4),=C'FEB'                                                 
         MVC   88(3,R4),=C'MAR'                                                 
         LA    R3,Q1LIST                                                        
         BAS   RE,FORMLINE                                                      
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
FORMAT2  CLI   QRTFILT+1,C'Y'      QUARTER 2 FOR WEEKLY                         
         BNE   FORMAT3                                                          
         MVC   12(3,R4),=C'APR'                                                 
         MVC   50(3,R4),=C'MAY'                                                 
         MVC   88(3,R4),=C'JUN'                                                 
         LA    R3,Q2LIST                                                        
         BAS   RE,FORMLINE                                                      
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
FORMAT3  CLI   QRTFILT+2,C'Y'      QUARTER 3 FOR WEEKLY                         
         BNE   FORMAT4                                                          
         MVC   12(3,R4),=C'JUL'                                                 
         MVC   50(3,R4),=C'AUG'                                                 
         MVC   88(3,R4),=C'SEP'                                                 
         LA    R3,Q3LIST                                                        
         BAS   RE,FORMLINE                                                      
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
FORMAT4  CLI   QRTFILT+3,C'Y'      QUARTER 4 FOR WEEKLY                         
         BNE   XIT                                                              
         MVC   12(3,R4),=C'OCT'                                                 
         MVC   50(3,R4),=C'NOV'                                                 
         MVC   88(3,R4),=C'DEC'                                                 
         LA    R3,Q4LIST                                                        
         BAS   RE,FORMLINE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE A SINGLE LINE                                             
         SPACE 3                                                                
FORMLINE NTR1                                                                   
*                                  R2=A(WEEKLY BUCKETS)                         
*                                  R3=A(FORMAT LIST)                            
*                                  R4=A(PRINT LINE)                             
         SPACE 1                                                                
FL2      BAS   RE,FL4                                                           
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         B     FL2                                                              
         SPACE 1                                                                
FL4      NTR1                                                                   
         ZIC   R1,0(R3)            ADD DISPLACEMENT TO PRINT LINE               
         BCTR  R1,0                                                             
         AR    R4,R1                                                            
         SR    R1,R1               ADD HUTS IN R1                               
         ZIC   R0,1(R3)            R0=NUMBER OF WEEKS TO AVERAGE                
         SH    R0,=H'2'                                                         
         LA    R3,2(R3)            R3=A(WEEK NUMBER)                            
         SR    R5,R5               R5=HITS                                      
*                                                                               
FL5      ZIC   R6,0(R3)            PICK UP WEEK NUMBER                          
         BCTR  R6,0                                                             
         MH    R6,=H'3'            DISPLACE INTO BUCKETS                        
         AR    R6,R2                                                            
         OC    1(2,R6),1(R6)       CHECK ANYTHING THERE                         
         BZ    FL9                                                              
         LA    R5,1(R5)            HITS                                         
*                                                                               
         LH    RF,1(R6)            RF - HUT                                     
         CLI   NYEARS+1,1          MULTI-YEAR                                   
         BE    FL6                                                              
         CLC   NYEARS+1(1),0(R6)   DID WE GET REPORT FOR EACH YEAR              
         BE    FL6                                                              
         SR    RE,RE               FORCE AVERAGE FOR TOTAL YEARS                
         MH    RF,NYEARS           MULTIPLY BY YEARS REQUESTED                  
         CLI   0(R6),1             DON'T BOTHER TO DIVIDE BY 1 YEAR             
         BE    FL6                                                              
         MVC   DIVISOR+3(1),0(R6)                                               
         SLL   RF,1                                                             
         D     RE,DIVISOR          DIVIDE BY YEARS REPORTED                     
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
FL6      AR    R1,RF               ACCUMULATE HUT                               
*                                                                               
FL9      LA    R3,1(R3)            POINT TO NEXT WEEK                           
         BCT   R0,FL5                                                           
*                                                                               
         EJECT                                                                  
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         MH    R5,NYEARS           AVERAGE FOR NUMBER OF YEARS AND              
         SLL   R1,1                AVERAGE FOR THE NUMBER OF HITS               
         DR    R0,R5                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
         EDIT  (R1),(4,0(R4)),1    AND EDIT                                     
         B     XIT                                                              
         EJECT                                                                  
*              TABLES FOR FORMAT ROUTINE                                        
         SPACE 3                                                                
Q1LIST   DS    0F                  DISPLACEMENT/LENGTH/WEEK LIST                
         SPACE 1                                                                
         DC    AL1(18,3,1)         JAN                                          
         DC    AL1(23,3,2)                                                      
         DC    AL1(28,3,3)                                                      
         DC    AL1(33,3,4)                                                      
         DC    AL1(43,6,1,2,3,4)                                                
         SPACE 1                                                                
         DC    AL1(56,3,5)         FEB                                          
         DC    AL1(61,3,6)                                                      
         DC    AL1(66,3,7)                                                      
         DC    AL1(71,3,8)                                                      
         DC    AL1(81,6,5,6,7,8)                                                
         SPACE 1                                                                
         DC    AL1(94,3,9)         MAR                                          
         DC    AL1(99,3,10)                                                     
         DC    AL1(104,3,11)                                                    
         DC    AL1(109,3,12)                                                    
         DC    AL1(119,6,9,10,11,12)                                            
         DC    AL1(126,14,1,2,3,4,5,6,7,8,9,10,11,12)                           
         DC    X'FF'                                                            
         SPACE 3                                                                
Q2LIST   DS    0F                  SECOND QUARTER                               
         SPACE 1                                                                
         DC    AL1(18,3,13)        APR                                          
         DC    AL1(23,3,14)                                                     
         DC    AL1(28,3,15)                                                     
         DC    AL1(33,3,16)                                                     
         DC    AL1(38,3,17)                                                     
         DC    AL1(43,7,13,14,15,16,17)                                         
         SPACE 1                                                                
         DC    AL1(56,3,18)        MAY                                          
         DC    AL1(61,3,19)                                                     
         DC    AL1(66,3,20)                                                     
         DC    AL1(71,3,21)                                                     
         DC    AL1(81,6,18,19,20,21)                                            
         SPACE 1                                                                
         DC    AL1(94,3,22)        JUN                                          
         DC    AL1(99,3,23)                                                     
         DC    AL1(104,3,24)                                                    
         DC    AL1(109,3,25)                                                    
         DC    AL1(114,3,26)                                                    
         DC    AL1(119,7,22,23,24,25,26)                                        
         SPACE 1                                                                
         DC    AL1(126,16,13,14,15,16,17,18,19,20,21,22,23,24,25,26)            
         DC    X'FF'                                                            
         EJECT                                                                  
Q3LIST   DS    0F                  THIRD QUARTER                                
         SPACE 1                                                                
         DC    AL1(18,3,27)        JUL                                          
         DC    AL1(23,3,28)                                                     
         DC    AL1(28,3,29)                                                     
         DC    AL1(33,3,30)                                                     
         DC    AL1(43,6,27,28,29,30)                                            
         SPACE 1                                                                
         DC    AL1(56,3,31)        AUG                                          
         DC    AL1(61,3,32)                                                     
         DC    AL1(66,3,33)                                                     
         DC    AL1(71,3,34)                                                     
         DC    AL1(76,3,35)                                                     
         DC    AL1(81,7,31,32,33,34,35)                                         
         SPACE 1                                                                
         DC    AL1(94,3,36)        SEP                                          
         DC    AL1(99,3,37)                                                     
         DC    AL1(104,3,38)                                                    
         DC    AL1(109,3,39)                                                    
         DC    AL1(119,6,36,37,38,39)                                           
         SPACE 1                                                                
         DC    AL1(126,15,27,28,29,30,31,32,33,34,35,36,37,38,39)               
         DC    X'FF'                                                            
         SPACE 1                                                                
Q4LIST   DS    0F                  FOURTH QUARTER                               
         SPACE 1                                                                
         DC    AL1(18,3,40)        OCT                                          
         DC    AL1(23,3,41)                                                     
         DC    AL1(28,3,42)                                                     
         DC    AL1(33,3,43)                                                     
         DC    AL1(43,6,40,41,42,43)                                            
         SPACE 1                                                                
         DC    AL1(56,3,44)        NOV                                          
         DC    AL1(61,3,45)                                                     
         DC    AL1(66,3,46)                                                     
         DC    AL1(71,3,47)                                                     
         DC    AL1(81,6,44,45,46,47)                                            
         SPACE 1                                                                
         DC    AL1(94,3,48)        DEC                                          
         DC    AL1(99,3,49)                                                     
         DC    AL1(104,3,50)                                                    
         DC    AL1(109,3,51)                                                    
         DC    AL1(114,3,52)                                                    
         DC    AL1(119,7,48,49,50,51,52)                                        
         SPACE 1                                                                
         DC    AL1(126,15,40,41,42,43,44,45,46,47,48,49,50,51,52)               
         DC    X'FF'                                                            
         SPACE 1                                                                
MONLIST  DS    0H         MONTHLY OPTION                                        
         DC    AL1(13,6,1,2,3,4)            JAN                                 
         DC    AL1(20,6,5,6,7,8)            FEB                                 
         DC    AL1(27,6,9,10,11,12)         MAR                                 
         DC    AL1(34,14,1,2,3,4,5,6,7,8,9,10,11,12)                            
         SPACE 1                                                                
         DC    AL1(42,7,13,14,15,16,17)     APR                                 
         DC    AL1(49,6,18,19,20,21)        MAY                                 
         DC    AL1(56,7,22,23,24,25,26)     JUN                                 
         DC    AL1(63,16,13,14,15,16,17,18,19,20,21,22,23,24,25,26)             
         SPACE 1                                                                
         DC    AL1(71,6,27,28,29,30)        JUL                                 
         DC    AL1(78,7,31,32,33,34,35)     AUG                                 
         DC    AL1(85,6,36,37,38,39)        SEP                                 
         DC    AL1(92,15,27,28,29,30,31,32,33,34,35,36,37,38,39)                
         SPACE 1                                                                
         DC    AL1(100,6,40,41,42,43)       OCT                                 
         DC    AL1(107,6,44,45,46,47)       NOV                                 
         DC    AL1(114,7,48,49,50,51,52)    DEC                                 
         DC    AL1(121,15,40,41,42,43,44,45,46,47,48,49,50,51,52)               
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              LISTING OF HOLIDAY HUTS                                          
         SPACE 3                                                                
HOLILIST NTR1                                                                   
         CLI   HOLIOPT,C'Y'                                                     
         BNE   XIT                                                              
         MVI   FLAVOR,C'H'                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    R4,KEY              SET UP KEY FOR HOLIDAY HUT                   
         XC    KEY,KEY                                                          
         USING NHHKEY,R4                                                        
         MVC   NHHKTYPE,=X'0D52'                                                
         MVC   NHHKAM,NBACTAM                                                   
         GOTO1 HIGH                                                             
         B     HOL4                                                             
         SPACE 1                                                                
HOL2     GOTO1 SEQ                                                              
         SPACE 1                                                                
HOL4     CLC   KEY(3),KEYSAVE                                                   
         BNE   XIT                                                              
*                                  REPORT ON RECORDS                            
         GOTO1 DATCON,DMCB,(2,NHHKDATE),(8,P+2)                                 
         MVC   P+15(1),NHHKSCHM                                                 
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         LA    R6,IO                                                            
         MVI   ELCODE,X'52'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         USING NHHEL,R6                                                         
         LA    R2,NHHALL           SET UP FOR EACH DAYPART                      
         LA    R3,P+22                                                          
         LA    R5,8                                                             
         SPACE 1                                                                
HOL6     OC    0(2,R2),0(R2)                                                    
         BZ    HOL8                                                             
         EDIT  (2,(R2)),(7,(R3)),2,FLOAT=-                                      
         MVI   7(R3),C'%'          SHOW PERCENT                                 
         SPACE 1                                                                
HOL8     LA    R2,2(R2)                                                         
         LA    R3,11(R3)                                                        
         BCT   R5,HOL6                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     HOL2                                                             
         EJECT                                                                  
*              LISTING OF AGENCY RECORDS                                        
         SPACE 3                                                                
SCHMLIST NTR1                                                                   
         CLI   SCHMOPT,C'Y'                                                     
         BNE   XIT                                                              
         MVI   FLAVOR,C'S'                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    R4,KEY              SET UP KEY FOR SCHEME                        
         XC    KEY,KEY                                                          
         USING NAHKEY,R4                                                        
         MVC   NAHKTYPE,=X'0D50'                                                
         MVC   NAHKAM,NBACTAM                                                   
         GOTO1 HIGH                                                             
         B     SCH4                                                             
         SPACE 1                                                                
SCH2     GOTO1 SEQ                                                              
         SPACE 1                                                                
SCH4     CLC   KEY(3),KEYSAVE                                                   
         BNE   XIT                                                              
         MVC   P+54(1),NAHKSCHM    REPORT ON RECORDS                            
         ZIC   R1,NAHKDAY                                                       
         MH    R1,=H'3'                                                         
         LA    R1,DAYALPH(R1)                                                   
         MVC   P+61(3),0(R1)                                                    
         XC    DUB,DUB                                                          
         MVC   DUB(2),NAHKTIME                                                  
         GOTO1 UNTIME,DMCB,DUB,P+68                                             
         MVC   P+76(2),=C'19'                                                   
         CLI   NAHKYEAR,50                                                      
         BH    *+10                                                             
         MVC   P+76(2),=C'20'                                                   
         EDIT  (1,NAHKYEAR),(2,P+78)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     SCH2                                                             
         EJECT                                                                  
*              OPTIONAL OUTPUT OF HUT RECORD TO SORT                            
         SPACE 3                                                                
PUTSORT  NTR1                                                                   
*                                  R2=A(104 BYTE HUT LIST)                      
*                                  R5=START 1/4 HOUR NUMBER                     
         LA    R4,TAPAREA                                                       
         USING PRKEY,R4                                                         
         MVI   PRCODE,C'P'         FILL IN KEY                                  
         MVI   PRMEDIA,C'N'                                                     
         MVI   PRSRC,C'N'                                                       
         MVC   PRSTAT,=C'HUT T'                                                 
         MVC   PRBOOK(1),ENDYEAR                                                
         MVC   PRBTYP,BOOKTYPE                                                  
         STC   R5,PRSTIM                                                        
         ZIC   R1,DAYFILT          SHIFT THE DAY                                
         SLL   R1,4                                                             
         STC   R1,PRDW                                                          
         LA    R6,PRFRSTEL                                                      
         USING PHELEM,R6                                                        
         MVI   PHCODE,PHCODEQ      PAV DAY 1/4 HOUR ELEMENT                     
         MVI   PHELN,PHELNEQ                                                    
         MVI   PHDUR,2                                                          
         MVC   PHDBOOK,PRBOOK                                                   
         MVI   PHDURTOT,2                                                       
         LA    R6,PHELNEQ(R6)                                                   
         MVI   0(R6),X'90'         90 ELEMENT                                   
         MVI   1(R6),108                                                        
*                                                                               
         LA    RF,52                                                            
         MVC   2(2,R6),1(R2)       MOVE HUTS TO ELEMENT                         
         LA    R6,2(R6)            (HIT PORTION OF LIST NOT NEEDED)             
         LA    R2,3(R2)                                                         
         BCT   RF,*-14                                                          
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',TAPAREA                                      
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         SPACE 3                                                                
         L     R4,ABOX             INITIALIZE BOXES                             
         LTR   R4,R4                                                            
         BZ    HOOK1                                                            
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         ZIC   R1,MAXLINES                                                      
         LA    R1,BOXROWS(R1)                                                   
         MVI   0(R1),C'B'                                                       
         SPACE 1                                                                
HOOK1    CLI   FLAVOR,C'H'                                                      
         BE    HOOK10                                                           
         CLI   FLAVOR,C'S'                                                      
         BE    HOOK12                                                           
         EDIT  (1,YEARFILT),(2,H4+67)                                           
         CLI   YEARFILT,100                                                     
         BL    *+10                                                             
         MVC   H4+65(2),=C'20'                                                  
         CLC   YEARFILT,ENDYEAR                                                 
         BE    HOOK2                                                            
         MVC   H4+60(4),H4+65                                                   
         MVI   H4+64,C'-'                                                       
         EDIT  (1,ENDYEAR),(2,H4+67)                                            
**       CLI   ENDYEAR,50                                                       
         CLI   ENDYEAR,100                                                      
         BL    *+10                                                             
         MVC   H4+65(2),=C'20'                                                  
         SPACE 1                                                                
HOOK2    ZIC   R1,DAYFILT          EXPAND DAY                                   
         MH    R1,=H'3'                                                         
         LA    R1,DAYALPH(R1)                                                   
         MVC   H4+54(3),0(R1)                                                   
         CLI   SOURCE,C'N'         EXPAND SOURCE(BOOKTYPE)/SCHEME               
         BNE   HOOK5                                                            
         LA    R1,TYPELIST                                                      
HOOK4    CLI   0(R1),X'FF'                                                      
         BE    HOOK6                                                            
         CLC   0(1,R1),BOOKTYPE                                                 
         BE    *+12                                                             
         LA    R1,13(R1)                                                        
         B     HOOK4                                                            
         MVC   H4+14(12),1(R1)     BOOKTYPE LITERAL                             
         B     HOOK6                                                            
HOOK5    MVC   H4+10(7),=C'CONTROL'                                             
         CLI   SOURCE,C'C'                                                      
         BE    HOOK6                                                            
         MVC   H4+10(7),=C'AGENCY='                                             
         MVC   H4+17(1),SCHEME                                                  
         SPACE 1                                                                
HOOK6    CLI   FLAVOR,C'W'                                                      
         BE    HOOK8                                                            
         LTR   R4,R4               MONTHLY                                      
         BZ    *+10                                                             
         MVC   BOXCOLS,MONCOLS                                                  
         MVC   H8,MONHEAD                                                       
         MVC   H9,MONHEAD2                                                      
         B     XIT                                                              
         SPACE 1                                                                
HOOK8    LTR   R4,R4               WEEKLY                                       
         BZ    *+10                                                             
         MVC   BOXCOLS,WEEKCOLS                                                 
         MVC   H8,WEEKHED                                                       
         MVC   H9,WEEKHED2                                                      
         MVC   H1+50(7),=C' WEEKLY'                                             
         MVI   H2+50,C' '                                                       
         MVC   WORK(10),H4+49                                                   
         MVC   H4+50(9),WORK                                                    
         B     XIT                                                              
         SPACE 1                                                                
HOOK10   LTR   R4,R4               HOLIDAY HUTS                                 
         BZ    *+10                                                             
         MVC   BOXCOLS,HOLICOLS                                                 
         MVC   H8,HOLIHED                                                       
         MVI   H9+1,0                                                           
         MVC   H1+50(7),=C'HOLIDAY'                                             
         MVC   H4+50(20),SPACES                                                 
         B     XIT                                                              
         SPACE 1                                                                
HOOK12   LTR   R4,R4               SCHEME RECORDS                               
         BZ    *+10                                                             
         MVC   BOXCOLS,SCHMCOLS                                                 
         MVC   H8,SCHMHED                                                       
         MVI   H9+1,0                                                           
         MVC   H1+50(11),=C' HUT SCHEME'                                        
         MVI   H2+50,C' '                                                       
         MVC   H4+50(20),SPACES                                                 
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 3                                                                
DAYLIST  DC    X'7C402010080402017F'                                            
DAYALPH  DC    C'M-FMONTUEWEDTHUFRISATSUNM-SALL'                                
         SPACE 1                                                                
TYPELIST DC    X'0',CL12'(DIARY)     '                                          
         DC    C'A',CL12'(ASCRIBED)  '                                          
         DC    C'I',CL12'(INTEGRATED)'                                          
         DC    C'C',CL12'(CONFORMED) '                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
WEEKHED  DC    C'   TIME   '                                                    
         DC    C' MONTH  WK1  WK2  WK3  WK4  WK5  AVE  '                        
         DC    C' MONTH  WK1  WK2  WK3  WK4  WK5  AVE  '                        
         DC    C' MONTH  WK1  WK2  WK3  WK4  WK5  AVE  '                        
         DC    C' QTR.     '                                                    
         SPACE 1                                                                
WEEKHED2 DC    C'          '                                                    
         DC    C'                                      '                        
         DC    C'                                      '                        
         DC    C'                                      '                        
         DC    C' AVE.     '                                                    
         SPACE 1                                                                
MONHEAD  DC    C'   TIME   '                                                    
         DC    C'   JAN    FEB    MAR   1ST.  '                                 
         DC    C'   APR    MAY    JUN   2ND.  '                                 
         DC    C'   JUL    AUG    SEP   3RD.  '                                 
         DC    C'   OCT    NOV    DEC   4TH.         '                          
         SPACE 1                                                                
MONHEAD2 DC    C'          '                                                    
         DC    C'                       QRT.  '                                 
         DC    C'                       QRT.  '                                 
         DC    C'                       QRT.  '                                 
         DC    C'                       QRT.         '                          
         SPACE 1                                                                
HOLIHED  DC    C'    DATE     SCHEME      ALL        DAY'                       
         DC    C'       FRINGE     PRIME       LATE       KIDS'                 
         DC    C'      SPORTS     YOUTH      OTHERS           '                 
         SPACE 1                                                                
SCHMHED  DC    CL50' '                                                          
         DC    CL82'  SCHEME   DAY    TIME    YEAR'                             
         SPACE 1                                                                
WEEKCOLS DC    C'L        C'                                                    
         DC    37C' '                                                           
         DC    C'C'                                                             
         DC    37C' '                                                           
         DC    C'C'                                                             
         DC    37C' '                                                           
         DC    C'C'                                                             
         DC    C'      R      '                                                 
         SPACE 1                                                                
MONCOLS  DC    C'L        CC'                                                   
         DC    C'      C      C      C      CC'                                 
         DC    C'      C      C      C      CC'                                 
         DC    C'      C      C      C      CC'                                 
         DC    C'      C      C      C      CR     '                            
         SPACE 1                                                                
HOLICOLS DC    C'L          C        CC'                                        
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          C'                                                   
         DC    C'          R                 '                                  
         SPACE 1                                                                
SCHMCOLS DC    CL50' '                                                          
         DC    CL82'L        C     C        C      R'                           
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              TAPE DCB AND LTORG ETC                                           
         SPACE 3                                                                
OTAPE    DS    0D                                                               
         DCB   DDNAME=OTAPE,DSORG=PS,RECFM=VB,LRECL=1100,BLKSIZE=8200, *        
               MACRF=PM                                                         
         SPACE 1                                                                
TAPHEAD  DC    H'148'                                                           
         DC    H'0'                                                             
TAPAREA  DC    20X'00'             KEYS                                         
         DC    H'144'              RECORD LENGTH                                
         DC    122X'00'            STATUS/ELEMENTS/X'00'                        
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=144'                                   
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
SORTBUFF DS    0D                                                               
******** DS    42000C                                                           
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEGETHUTD                                                      
         EJECT                                                                  
       ++INCLUDE NEGENHUT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE2D                                                       
*                                                                               
         EJECT                                                                  
HUTCOM   DSECT                                                                  
** ARGS FROM EDIT                                                               
DAYFILT  DS    CL1                                                              
QRTFILT  DS    CL4                                                              
YEARFILT DS    CL1                                                              
ENDYEAR  DS    CL1                                                              
TIMFILT  DS    CL4                                                              
FLAVOR   DS    CL1                                                              
SOURCE   DS    CL1                                                              
SCHEME   DS    CL1                                                              
HOLIOPT  DS    CL1                                                              
OPT52    DS    CL1                                                              
SCHMOPT  DS    CL1                                                              
BOOKTYPE DS    CL1                                                              
*                                                                               
DIVISOR  DS    F                                                                
NYEARS   DS    H                                                                
*                                                                               
         DS    0F                                                               
GHAREA   DS    CL100                                                            
GETHUT   DS    A                                                                
RELO     DS    A                                                                
BUCKETS  DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED62   05/01/02'                                      
         END                                                                    

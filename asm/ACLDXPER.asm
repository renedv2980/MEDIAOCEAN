*          DATA SET ACLDXPER   AT LEVEL 004 AS OF 11/11/99                      
*PHASE ACLDXPER                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
         TITLE 'FIX ICCNJ PERSON RECORDS'                                       
***********************************************************************         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
***********************************************************************         
                                                                                
ACLDXDRP CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DRP**,R7,R8                                                  
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         ST    R1,APARM                                                         
         MVC   PLIST,0(R1)                                                      
         B     DMXCTL                                                           
*                                                                               
DMXCTL   CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INITIALIZE                                             *         
***********************************************************************         
                                                                                
DMXINIT  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS RECORD                                         *         
***********************************************************************         
                                                                                
         USING ACTRECD,R4                                                       
DMXREC   DS    0H                                                               
         L     R4,AREC                                                          
         CLC   0(2,R4),=X'0FF4'    PERSON RECORD FOR ICCNJ                      
         BNE   DMXKEEP                                                          
*                                                                               
         USING PERRECD,R4                                                       
         L     R2,=A(TABLE)                                                     
DMXREC3  CLC   PERKCODE(6),8(R2)                                                
         BE    DMXREC5                                                          
         LA    R2,L'TABLE(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         BE    DMXKEEP                                                          
         B     DMXREC3                                                          
*                                                                               
DMXREC5  MVC   PERKCODE(6),22(R2)                                               
*****    BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                           *         
***********************************************************************         
                                                                                
DMXEOF   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DITTO OUTPUT RECORDS                                   *         
***********************************************************************         
                                                                                
DMPPUT   NTR1  ,                                                                
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'PUT'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
                                                                                
DUMP     LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                    
                                                                                
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*        EXIT CONDITIONS                                              *         
***********************************************************************         
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              WORK AREA                                              *         
***********************************************************************         
                                                                                
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HELLO    DC    V(HELLO)                                                         
RECTYP   DC    V(ACRECTYP)                                                      
*                                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                   VPRINTER                                     
VCPRINT  DS    A                   VCPRINT                                      
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
         DS    CL8                                                              
WORK     DS    CL20                                                             
         DS    CL8                                                              
BYTE     DS    C                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
*                                                                               
*                                                                               
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
TABLE    DS   CL28                                                              
         DC   CL14'1R0101FR0BK ',CL14'1R0101FR00BK  '                           
         DC   CL14'1R0101FR1AH ',CL14'1R0101FR01AH  '                           
         DC   CL14'1R0101FR1VH ',CL14'1R0101FR01VH  '                           
         DC   CL14'1R0101FR2RB ',CL14'1R0101FR02RB  '                           
         DC   CL14'1R0101FR7SP ',CL14'1R0101FR2641  '                           
         DC   CL14'1R0101FR8SP ',CL14'1R0101FR08SP  '                           
         DC   CL14'1R010100LM2 ',CL14'1R0101002388  '                           
         DC   CL14'1R0101000AT ',CL14'1R0101002398  '                           
         DC   CL14'1R0101000DQ ',CL14'1R0101002381  '                           
         DC   CL14'1R0101000GM ',CL14'1R0101002375  '                           
         DC   CL14'1R0101000RE ',CL14'1R0101002366  '                           
         DC   CL14'1R0101000SR ',CL14'1R0101002397  '                           
         DC   CL14'1R0101001AC ',CL14'1R0101002380  '                           
         DC   CL14'1R0101001DR ',CL14'1R0101002396  '                           
         DC   CL14'1R0101001LA ',CL14'1R01010023950 '                           
         DC   CL14'1R0101001RK ',CL14'1R01010001RK  '                           
         DC   CL14'1R0101001RY ',CL14'1R0101002385  '                           
         DC   CL14'1R0101001SN ',CL14'1R0101002377  '                           
         DC   CL14'1R0101001SO ',CL14'1R0101002371  '                           
         DC   CL14'1R0101002LH ',CL14'1R0101002374  '                           
         DC   CL14'1R0101002PS ',CL14'1R0101002372  '                           
         DC   CL14'1R0101002SW ',CL14'1R0101002389  '                           
         DC   CL14'1R0101003BM ',CL14'1R0101002376  '                           
         DC   CL14'1R0101003CL ',CL14'1R0101002390  '                           
         DC   CL14'1R0101003SP ',CL14'1R0101002368  '                           
         DC   CL14'1R0101003TB ',CL14'1R0101002383  '                           
         DC   CL14'1R0101004ST ',CL14'1R0101002370  '                           
         DC   CL14'1R0101005AB ',CL14'1R0101002382  '                           
         DC   CL14'1R0101005AM ',CL14'1R0101002391  '                           
         DC   CL14'1R0101005DD ',CL14'1R0101002367  '                           
         DC   CL14'1R0101005JW ',CL14'1R0101002394  '                           
         DC   CL14'1R0101005KB ',CL14'1R0101002384  '                           
         DC   CL14'1R0101005NS ',CL14'1R0101002379  '                           
         DC   CL14'1R0101005SC ',CL14'1R0101002392  '                           
         DC   CL14'1R0101005SL ',CL14'1R0101002373  '                           
         DC   CL14'1R0101006BG ',CL14'1R0101002489  '                           
         DC   CL14'1R0101006JL ',CL14'1R0101002369  '                           
         DC   CL14'1R0101006LB ',CL14'1R0101002393  '                           
         DC   CL14'1R0101006LD ',CL14'1R01010006LD  '                           
         DC   CL14'1R0101006LK ',CL14'1R0101002378  '                           
         DC   CL14'1R0101006LL ',CL14'1R0101002386  '                           
         DC   CL14'1R0101007RM ',CL14'1R0101002387  '                           
         DC   CL14'1R0102FRDS1 ',CL14'1R0102FR2648  '                           
         DC   CL14'1R0102FRMM8 ',CL14'1R0102FR0MM8  '                           
         DC   CL14'1R0102FR0HC ',CL14'1R0102FR2649  '                           
         DC   CL14'1R0102FR1EG ',CL14'1R0102FR01EG  '                           
         DC   CL14'1R0102FR1GM ',CL14'1R0102FR2647  '                           
         DC   CL14'1R0102FR1JJ ',CL14'1R0102FR2652  '                           
         DC   CL14'1R0102FR1KW ',CL14'1R0102FR01KW  '                           
         DC   CL14'1R0102FR1MH ',CL14'1R0102FR01MH  '                           
         DC   CL14'1R0102FR1SV ',CL14'1R0102FR4033  '                           
         DC   CL14'1R0102FR1TC ',CL14'1R0102FR01TC  '                           
         DC   CL14'1R0102FR1VS ',CL14'1R0102FR4033  '                           
         DC   CL14'1R0102FR2BL ',CL14'1R0102FR1101  '                           
         DC   CL14'1R0102FR3GM ',CL14'1R0102FR2646  '                           
         DC   CL14'1R0102FR3JR ',CL14'1R0102FR03JR  '                           
         DC   CL14'1R0102FR4CD ',CL14'1R0102FR2645  '                           
         DC   CL14'1R0102FR4KP ',CL14'1R0102FR2651  '                           
         DC   CL14'1R0102FR4MB ',CL14'1R0102FRFH11  '                           
         DC   CL14'1R0102FR5KD ',CL14'1R0102FR2650  '                           
         DC   CL14'1R0102FR5RC ',CL14'1R0102FR05RC  '                           
         DC   CL14'1R0102FR6KM ',CL14'1R0102FR06KM  '                           
         DC   CL14'1R0102FR8CS ',CL14'1R0102FR08CS  '                           
         DC   CL14'1R010200JB1 ',CL14'1R0102002425  '                           
         DC   CL14'1R010200JS2 ',CL14'1R0102002424  '                           
         DC   CL14'1R010200JS6 ',CL14'1R0102002430  '                           
         DC   CL14'1R0102000AC ',CL14'1R0102002399  '                           
         DC   CL14'1R0102000AE ',CL14'1R0102002429  '                           
         DC   CL14'1R0102000BN ',CL14'1R0102002412  '                           
         DC   CL14'1R0102000CD ',CL14'1R0102002439  '                           
         DC   CL14'1R0102000HA ',CL14'1R0102002418  '                           
         DC   CL14'1R0102000HB ',CL14'1R0102002406  '                           
         DC   CL14'1R0102000JL ',CL14'1R0102002402  '                           
         DC   CL14'1R0102000KA ',CL14'1R0102002437  '                           
         DC   CL14'1R0102000KN ',CL14'1R0102002419  '                           
         DC   CL14'1R0102000NC ',CL14'1R01020000NC  '                           
         DC   CL14'1R0102000OK ',CL14'1R0102002421  '                           
         DC   CL14'1R0102000OM ',CL14'1R0102002422  '                           
         DC   CL14'1R0102000RR ',CL14'1R0102002438  '                           
         DC   CL14'1R0102000RS ',CL14'1R0102002417  '                           
         DC   CL14'1R0102000TV ',CL14'1R0102002403  '                           
         DC   CL14'1R0102000UG ',CL14'1R0102002436  '                           
         DC   CL14'1R0102000VC ',CL14'1R0102002435  '                           
         DC   CL14'1R0102001DL ',CL14'1R0102002410  '                           
         DC   CL14'1R0102001FC ',CL14'1R0102002413  '                           
         DC   CL14'1R0102001SC ',CL14'1R0102002404  '                           
         DC   CL14'1R0102001TR ',CL14'1R0102002432  '                           
         DC   CL14'1R0102001164',CL14'1R0102002719  '                           
         DC   CL14'1R0102002BL ',CL14'1R0102001101  '                           
         DC   CL14'1R0102002EM ',CL14'1R0102002434  '                           
         DC   CL14'1R0102002LD ',CL14'1R0102002401  '                           
         DC   CL14'1R0102002ND ',CL14'1R0102002426  '                           
         DC   CL14'1R0102002TC ',CL14'1R0102002405  '                           
         DC   CL14'1R0102002TP ',CL14'1R0102002416  '                           
         DC   CL14'1R0102003BG ',CL14'1R0102002409  '                           
         DC   CL14'1R0102003GC ',CL14'1R0102002428  '                           
         DC   CL14'1R0102003JS ',CL14'1R01020003JS  '                           
         DC   CL14'1R0102003RD ',CL14'1R0102002414  '                           
         DC   CL14'1R0102003ST ',CL14'1R0102002407  '                           
         DC   CL14'1R0102004BH ',CL14'1R0102002431  '                           
         DC   CL14'1R0102004KL ',CL14'1R0102002427  '                           
         DC   CL14'1R0102005JG ',CL14'1R0102002415  '                           
         DC   CL14'1R0102005MS ',CL14'1R0102002408  '                           
         DC   CL14'1R0102006KB ',CL14'1R0102002423  '                           
         DC   CL14'1R0102008JL ',CL14'1R0102002420  '                           
         DC   CL14'1R0102008MF ',CL14'1R0102002433  '                           
         DC   CL14'1R0102009MS ',CL14'1R0102002411  '                           
         DC   CL14'1R0103FR1JM ',CL14'1R0103FR01JM  '                           
         DC   CL14'1R0103FR2KW ',CL14'1R0103FR1113  '                           
         DC   CL14'1R0103FR3KW ',CL14'1R0103FR1113  '                           
         DC   CL14'1R010300DBO ',CL14'1R0103002476  '                           
         DC   CL14'1R010300DS0 ',CL14'1R0103002468  '                           
         DC   CL14'1R010300JD1 ',CL14'1R0103002473  '                           
         DC   CL14'1R010300JH3 ',CL14'1R0103002475  '                           
         DC   CL14'1R010300JM3 ',CL14'1R0103002455  '                           
         DC   CL14'1R010300JS1 ',CL14'1R0103000JS1  '                           
         DC   CL14'1R010300JS5 ',CL14'1R0103002469  '                           
         DC   CL14'1R010300SS0 ',CL14'1R0103002465  '                           
         DC   CL14'1R0103000JW ',CL14'1R0103002443  '                           
         DC   CL14'1R0103000KO ',CL14'1R01030000KO  '                           
         DC   CL14'1R0103000MC ',CL14'1R0103002440  '                           
         DC   CL14'1R0103000SG ',CL14'1R0103002477  '                           
         DC   CL14'1R0103000SV ',CL14'1R0103002441  '                           
         DC   CL14'1R0103001AD ',CL14'1R0103002445  '                           
         DC   CL14'1R0103001AP ',CL14'1R0103002456  '                           
         DC   CL14'1R0103001CE ',CL14'1R0103002461  '                           
         DC   CL14'1R0103001CM ',CL14'1R0103001108  '                           
         DC   CL14'1R0103001CT ',CL14'1R0103002479  '                           
         DC   CL14'1R0103001EF ',CL14'1R0103001106  '                           
         DC   CL14'1R0103001IB ',CL14'1R0103002462  '                           
         DC   CL14'1R0103001KK ',CL14'1R0103001102  '                           
         DC   CL14'1R0103001ML ',CL14'1R0103002478  '                           
         DC   CL14'1R0103001PH ',CL14'1R0103002442  '                           
         DC   CL14'1R0103001PS ',CL14'1R0103001115  '                           
         DC   CL14'1R0103001SS ',CL14'1R0103002444  '                           
         DC   CL14'1R0103001TH ',CL14'1R0103002459  '                           
         DC   CL14'1R0103002BB ',CL14'1R0103002457  '                           
         DC   CL14'1R0103002DL ',CL14'1R0103002453  '                           
         DC   CL14'1R0103002EW ',CL14'1R0103002464  '                           
         DC   CL14'1R0103002JZ ',CL14'1R0103001107  '                           
         DC   CL14'1R0103002KZ ',CL14'1R0103002448  '                           
         DC   CL14'1R0103002LC ',CL14'1R0103002463  '                           
         DC   CL14'1R0103002MH ',CL14'1R0103002451  '                           
         DC   CL14'1R0103002ML ',CL14'1R0103002454  '                           
         DC   CL14'1R0103002RR ',CL14'1R0103002471  '                           
         DC   CL14'1R0103003LL ',CL14'1R0103002446  '                           
         DC   CL14'1R0103003PS ',CL14'1R01030003PS  '                           
         DC   CL14'1R0103003RS ',CL14'1R0103002467  '                           
         DC   CL14'1R0103004AL ',CL14'1R0103002470  '                           
         DC   CL14'1R0103004KB ',CL14'1R0103002449  '                           
         DC   CL14'1R0103004LH ',CL14'1R0103002472  '                           
         DC   CL14'1R0103005BC ',CL14'1R0103002460  '                           
         DC   CL14'1R0103005LF ',CL14'1R0103002452  '                           
         DC   CL14'1R0103006BM ',CL14'1R0103002466  '                           
         DC   CL14'1R0103006DD ',CL14'1R0103002447  '                           
         DC   CL14'1R0103006LF ',CL14'1R0103002450  '                           
         DC   CL14'1R0103008MM ',CL14'1R01030008MM  '                           
         DC   CL14'1R0103009SB ',CL14'1R0103002474  '                           
         DC   CL14'1R0105FR1JZ ',CL14'1R0105FR01JZ  '                           
         DC   CL14'1R010500LS0 ',CL14'1R0105002481  '                           
         DC   CL14'1R0105000JR ',CL14'1R0105002485  '                           
         DC   CL14'1R0105000NG ',CL14'1R0105002482  '                           
         DC   CL14'1R0105000TM ',CL14'1R0105002483  '                           
         DC   CL14'1R0105001HL ',CL14'1R0105002480  '                           
         DC   CL14'1R0105002FG ',CL14'1R0105001105  '                           
         DC   CL14'1R0105002LM ',CL14'1R0105002484  '                           
         DC   CL14'1R0105004JN ',CL14'1R0105004941  '                           
         DC   CL14'1R0106FR1SY ',CL14'1R0106FR01SY  '                           
         DC   CL14'1R0106FR1VE ',CL14'1R0106FR01VE  '                           
         DC   CL14'1R0106FR2JR ',CL14'1R0106FR1122  '                           
         DC   CL14'1R0106FR4EW ',CL14'1R0106FR04EW  '                           
         DC   CL14'1R0106000BH ',CL14'1R0106002492  '                           
         DC   CL14'1R0106000CM ',CL14'1R0106002493  '                           
         DC   CL14'1R0106000SG ',CL14'1R0106002477  '                           
         DC   CL14'1R0106000SN ',CL14'1R0106002494  '                           
         DC   CL14'1R0106000SS ',CL14'1R0106002491  '                           
         DC   CL14'1R0106001DZ ',CL14'1R0106001116  '                           
         DC   CL14'1R0106002EW ',CL14'1R0106002464  '                           
         DC   CL14'1R0106002JR ',CL14'1R0106001122  '                           
         DC   CL14'1R0106002JZ ',CL14'1R0106001107  '                           
         DC   CL14'1R0106002TH ',CL14'1R0106002488  '                           
         DC   CL14'1R0106003RR ',CL14'1R0106002486  '                           
         DC   CL14'1R0106005LR ',CL14'1R0106002490  '                           
         DC   CL14'1R0106006BG ',CL14'1R0106002489  '                           
         DC   CL14'1R0106006MS ',CL14'1R0106002487  '                           
         DC   CL14'1R0106008MM ',CL14'1R01060008MM  '                           
         DC   CL14'1R0107FR1SE ',CL14'1R0107FR2654  '                           
         DC   CL14'1R0107FR2MV ',CL14'1R0107FR2503  '                           
         DC   CL14'1R010700KM0 ',CL14'1R0107002502  '                           
         DC   CL14'1R0107000CZ ',CL14'1R0107002500  '                           
         DC   CL14'1R0107000NS ',CL14'1R01070000NS  '                           
         DC   CL14'1R0107000PS ',CL14'1R0107002498  '                           
         DC   CL14'1R0107000SL ',CL14'1R0107002504  '                           
         DC   CL14'1R0107001AB ',CL14'1R0107002495  '                           
         DC   CL14'1R0107001KA ',CL14'1R0107002496  '                           
         DC   CL14'1R0107001PL ',CL14'1R0107002501  '                           
         DC   CL14'1R0107001RM ',CL14'1R01070001RM  '                           
         DC   CL14'1R0107002MR ',CL14'1R0107002505  '                           
         DC   CL14'1R0107002MV ',CL14'1R0107002503  '                           
         DC   CL14'1R0107007LL ',CL14'1R0107002497  '                           
         DC   CL14'1R0108FRJL4 ',CL14'1R0108FR0JL4  '                           
         DC   CL14'1R0108FRODC ',CL14'1R0108FR0ODC  '                           
         DC   CL14'1R0108FRSP2 ',CL14'1R0108FR2664  '                           
         DC   CL14'1R0108FRSS1 ',CL14'1R0108FR2658  '                           
         DC   CL14'1R0108FR0GR ',CL14'1R0108FR00GR  '                           
         DC   CL14'1R0108FR0LS ',CL14'1R0108FR00LS  '                           
         DC   CL14'1R0108FR0RT ',CL14'1R0108FR3081  '                           
         DC   CL14'1R0108FR1AO ',CL14'1R0108FR01AO  '                           
         DC   CL14'1R0108FR1ED ',CL14'1R0108FR2655  '                           
         DC   CL14'1R0108FR1FL ',CL14'1R0108FR2662  '                           
         DC   CL14'1R0108FR1HB ',CL14'1R0108FR2665  '                           
         DC   CL14'1R0108FR1LC ',CL14'1R0108FR01LC  '                           
         DC   CL14'1R0108FR1PZ ',CL14'1R0108FR01PZ  '                           
         DC   CL14'1R0108FR1RG ',CL14'1R0108FR01RG  '                           
         DC   CL14'1R0108FR1TO ',CL14'1R0108FR1100  '                           
         DC   CL14'1R0108FR1TW ',CL14'1R0108FR2661  '                           
         DC   CL14'1R0108FR2EA ',CL14'1R0108FR2656  '                           
         DC   CL14'1R0108FR2KA ',CL14'1R0108FR02KA  '                           
         DC   CL14'1R0108FR2SS ',CL14'1R0108FR1103  '                           
         DC   CL14'1R0108FR5ES ',CL14'1R0108FR2659  '                           
         DC   CL14'1R0108FR6DL ',CL14'1R0108FR2663  '                           
         DC   CL14'1R010800KC2 ',CL14'1R0108002522  '                           
         DC   CL14'1R010800KM2 ',CL14'1R0108002523  '                           
         DC   CL14'1R010800SP2 ',CL14'1R0108002664  '                           
         DC   CL14'1R0108000EP ',CL14'1R01080000EP  '                           
         DC   CL14'1R0108000GR ',CL14'1R01080000GR  '                           
         DC   CL14'1R0108000JH ',CL14'1R0108002525  '                           
         DC   CL14'1R0108000NZ ',CL14'1R0108002526  '                           
         DC   CL14'1R0108000OA ',CL14'1R0108002517  '                           
         DC   CL14'1R0108001JE ',CL14'1R0108002509  '                           
         DC   CL14'1R0108001PT ',CL14'1R01080001PT  '                           
         DC   CL14'1R0108001TO ',CL14'1R0108001100  '                           
         DC   CL14'1R0108001TW ',CL14'1R0108002661  '                           
         DC   CL14'1R0108002AP ',CL14'1R0108002514  '                           
         DC   CL14'1R0108002EL ',CL14'1R0108002524  '                           
         DC   CL14'1R0108003AB ',CL14'1R0108002508  '                           
         DC   CL14'1R0108003AF ',CL14'1R0108002515  '                           
         DC   CL14'1R0108003AS ',CL14'1R0108002518  '                           
         DC   CL14'1R0108003BB ',CL14'1R0108002511  '                           
         DC   CL14'1R0108003BH ',CL14'1R0108002516  '                           
         DC   CL14'1R0108003RH ',CL14'1R0108002519  '                           
         DC   CL14'1R0108004JG ',CL14'1R0108002506  '                           
         DC   CL14'1R0108004MJ ',CL14'1R0108002521  '                           
         DC   CL14'1R0108004MW ',CL14'1R0108002507  '                           
         DC   CL14'1R0108005CD ',CL14'1R0108002513  '                           
         DC   CL14'1R0108005KG ',CL14'1R0108002510  '                           
         DC   CL14'1R0108006MD ',CL14'1R0108002537  '                           
         DC   CL14'1R0108008BC ',CL14'1R0108002520  '                           
         DC   CL14'1R0108009JC ',CL14'1R0108002512  '                           
         DC   CL14'1R0109FR1JZ ',CL14'1R0109FR01JZ  '                           
         DC   CL14'1R0109000MS ',CL14'1R0109002530  '                           
         DC   CL14'1R0109002KS ',CL14'1R0109002527  '                           
         DC   CL14'1R0109004CF ',CL14'1R0109002528  '                           
         DC   CL14'1R0109005DL ',CL14'1R0109002529  '                           
         DC   CL14'1R0110FR0FI ',CL14'1R0110FR00FI  '                           
         DC   CL14'1R0110FR5JA ',CL14'1R0110FR1110  '                           
         DC   CL14'1R011000SB0 ',CL14'1R0110002533  '                           
         DC   CL14'1R0110002PD ',CL14'1R0110002534  '                           
         DC   CL14'1R0110004KS ',CL14'1R0110002532  '                           
         DC   CL14'1R0110004MS ',CL14'1R0110002531  '                           
         DC   CL14'1R0110005JA ',CL14'1R0110001110  '                           
         DC   CL14'1R0111000KH ',CL14'1R0111002536  '                           
         DC   CL14'1R0111000SP ',CL14'1R0111002535  '                           
         DC   CL14'1R0112FR1MB ',CL14'1R0112FR01MB  '                           
         DC   CL14'1R0112FR1WD ',CL14'1R0112FR01WD  '                           
         DC   CL14'1R0112FR3LC ',CL14'1R0112FR03LC  '                           
         DC   CL14'1R0112FR4ML ',CL14'1R0112FR04ML  '                           
         DC   CL14'1R0112005LD ',CL14'1R0112002539  '                           
         DC   CL14'1R0112005RB ',CL14'1R0112002538  '                           
         DC   CL14'1R0112006MD ',CL14'1R0112002537  '                           
         DC   CL14'1R0113000JJ ',CL14'1R0113001114  '                           
         DC   CL14'1R0113000JR ',CL14'1R0113002485  '                           
         DC   CL14'1R011400KC2 ',CL14'1R0114002522  '                           
         DC   CL14'1R011400KM2 ',CL14'1R0114002523  '                           
         DC   CL14'1R011400SP2 ',CL14'1R0114002664  '                           
         DC   CL14'1R0114000OA ',CL14'1R0114002517  '                           
         DC   CL14'1R0114001JE ',CL14'1R0114002509  '                           
         DC   CL14'1R0114002AP ',CL14'1R0114002514  '                           
         DC   CL14'1R0114003AB ',CL14'1R0114002508  '                           
         DC   CL14'1R0114003AS ',CL14'1R0114002518  '                           
         DC   CL14'1R0114003BH ',CL14'1R0114002516  '                           
         DC   CL14'1R0114003RH ',CL14'1R0114002519  '                           
         DC   CL14'1R0114004MJ ',CL14'1R0114002521  '                           
         DC   CL14'1R0114008BC ',CL14'1R0114002520  '                           
         DC   CL14'1R0116000SG ',CL14'1R0116002477  '                           
         DC   CL14'1R0116006LF ',CL14'1R0116002450  '                           
         DC   CL14'1R0204FR0PB ',CL14'1R0204FR2642  '                           
         DC   CL14'1R0204FR0RM ',CL14'1R0204FR00RM  '                           
         DC   CL14'1R0204FR1BH ',CL14'1R0204FR1109  '                           
         DC   CL14'1R0204FR1BK ',CL14'1R0204FR01BK  '                           
         DC   CL14'1R0204FR1KC ',CL14'1R0204FR01KC  '                           
         DC   CL14'1R0204FR1KZ ',CL14'1R0204FR01KZ  '                           
         DC   CL14'1R0204FR1MK ',CL14'1R0204FR01MK  '                           
         DC   CL14'1R0204FR1MS ',CL14'1R0204FR01MS  '                           
         DC   CL14'1R0204FR1NP ',CL14'1R0204FR2653  '                           
         DC   CL14'1R0204FR2CZ ',CL14'1R0204FR1112  '                           
         DC   CL14'1R0204FR2DB ',CL14'1R0204FR02DB  '                           
         DC   CL14'1R0204FR2JJ ',CL14'1R0204FR02JJ  '                           
         DC   CL14'1R0204FR2NK ',CL14'1R0204FR02NK  '                           
         DC   CL14'1R0204FR2RB ',CL14'1R0204FR02RB  '                           
         DC   CL14'1R0204FR4CB ',CL14'1R0204FR2643  '                           
         DC   CL14'1R020400CC1 ',CL14'1R0204002549  '                           
         DC   CL14'1R020400CC3 ',CL14'1R0204002550  '                           
         DC   CL14'1R020400DC4 ',CL14'1R0204002553  '                           
         DC   CL14'1R0204000AT ',CL14'1R0204002398  '                           
         DC   CL14'1R0204000GT ',CL14'1R0204002541  '                           
         DC   CL14'1R0204000MU ',CL14'1R0204002552  '                           
         DC   CL14'1R0204000VB ',CL14'1R0204002558  '                           
         DC   CL14'1R0204000YB ',CL14'1R0204002554  '                           
         DC   CL14'1R0204001AW ',CL14'1R0204002546  '                           
         DC   CL14'1R0204001DB ',CL14'1R0204002540  '                           
         DC   CL14'1R0204001DM ',CL14'1R0204002543  '                           
         DC   CL14'1R0204001SQ ',CL14'1R0204001111  '                           
         DC   CL14'1R0204003LH ',CL14'1R0204002547  '                           
         DC   CL14'1R0204003ML ',CL14'1R0204002548  '                           
         DC   CL14'1R0204004CK ',CL14'1R0204002545  '                           
         DC   CL14'1R0204004DD ',CL14'1R0204002557  '                           
         DC   CL14'1R0204004JH ',CL14'1R02040004JH  '                           
         DC   CL14'1R0204005ER ',CL14'1R0204002555  '                           
         DC   CL14'1R0204005MM ',CL14'1R02040005MM  '                           
         DC   CL14'1R0204005MP ',CL14'1R0204002556  '                           
         DC   CL14'1R0204005SB ',CL14'1R0204002551  '                           
         DC   CL14'1R0204006KG ',CL14'1R02040006KG  '                           
         DC   CL14'1R0204007RM ',CL14'1R0204002387  '                           
         DC   CL14'1R0301FR1JL ',CL14'1R0301FR01JL  '                           
         DC   CL14'1R0301FR1LS ',CL14'1R0301FR01LS  '                           
         DC   CL14'1R0301FR1MW ',CL14'1R0301FR01MW  '                           
         DC   CL14'1R030100MM2 ',CL14'1R0301002564  '                           
         DC   CL14'1R0301000NJ ',CL14'1R0301002560  '                           
         DC   CL14'1R0301000PH ',CL14'1R0301002571  '                           
         DC   CL14'1R0301000SH ',CL14'1R0301002559  '                           
         DC   CL14'1R0301001GC ',CL14'1R0301002565  '                           
         DC   CL14'1R0301001NC ',CL14'1R0301002570  '                           
         DC   CL14'1R0301002CL ',CL14'1R0301002566  '                           
         DC   CL14'1R0301002KN ',CL14'1R0301002567  '                           
         DC   CL14'1R0301004RR ',CL14'1R0301002569  '                           
         DC   CL14'1R0301004SR ',CL14'1R0301002561  '                           
         DC   CL14'1R0301008JP ',CL14'1R0301002568  '                           
         DC   CL14'1R0301008SS ',CL14'1R0301002562  '                           
         DC   CL14'1R0301009JD ',CL14'1R0301002563  '                           
         DC   CL14'1R0302FRMS4 ',CL14'1R0302FR0MS4  '                           
         DC   CL14'1R0302FRSM3 ',CL14'1R0302FR2669  '                           
         DC   CL14'1R0302FR0DY ',CL14'1R0302FR2668  '                           
         DC   CL14'1R0302FR0KJ ',CL14'1R0302FR00KJ  '                           
         DC   CL14'1R0302FR0TB ',CL14'1R0302FR2594  '                           
         DC   CL14'1R0302FR1FG ',CL14'1R0302FR01FG  '                           
         DC   CL14'1R0302FR2MB ',CL14'1R0302FR02MB  '                           
         DC   CL14'1R0302FR3MB ',CL14'1R0302FR03MB  '                           
         DC   CL14'1R0302FR3SE ',CL14'1R0302FR2592  '                           
         DC   CL14'1R0302FR5AR ',CL14'1R0302FR2667  '                           
         DC   CL14'1R0302FR5MD ',CL14'1R0302FR05MD  '                           
         DC   CL14'1R0302FR6DG ',CL14'1R0302FR06DG  '                           
         DC   CL14'1R0302FR7BS ',CL14'1R0302FR07BS  '                           
         DC   CL14'1R0302FR7SL ',CL14'1R0302FR07SL  '                           
         DC   CL14'1R030200CS0 ',CL14'1R0302002586  '                           
         DC   CL14'1R0302000BI ',CL14'1R0302002580  '                           
         DC   CL14'1R0302000TB ',CL14'1R0302002594  '                           
         DC   CL14'1R0302001EW ',CL14'1R0302002583  '                           
         DC   CL14'1R0302001GL ',CL14'1R0302002588  '                           
         DC   CL14'1R0302001JC ',CL14'1R0302002572  '                           
         DC   CL14'1R0302001JV ',CL14'1R0302002581  '                           
         DC   CL14'1R0302001LV ',CL14'1R0302002585  '                           
         DC   CL14'1R0302001TB ',CL14'1R0302002579  '                           
         DC   CL14'1R0302002DR ',CL14'1R0302002573  '                           
         DC   CL14'1R0302002GM ',CL14'1R0302002584  '                           
         DC   CL14'1R0302003SE ',CL14'1R0302002592  '                           
         DC   CL14'1R0302004JW ',CL14'1R0302002578  '                           
         DC   CL14'1R0302005CB ',CL14'1R0302002590  '                           
         DC   CL14'1R0302005CM ',CL14'1R0302002576  '                           
         DC   CL14'1R0302005RF ',CL14'1R0302002591  '                           
         DC   CL14'1R0302006JH ',CL14'1R0302002589  '                           
         DC   CL14'1R0302006LS ',CL14'1R0302002575  '                           
         DC   CL14'1R0302006SL ',CL14'1R0302002577  '                           
         DC   CL14'1R0302007BG ',CL14'1R0302002582  '                           
         DC   CL14'1R0302007DR ',CL14'1R03020007DR  '                           
         DC   CL14'1R0302007MC ',CL14'1R0302002593  '                           
         DC   CL14'1R0302007MF ',CL14'1R0302002587  '                           
         DC   CL14'1R0302008JM ',CL14'1R0302002574  '                           
         DC   CL14'1R0302009DD ',CL14'1R0302002595  '                           
         DC   CL14'1R030300SP1 ',CL14'1R0303002598  '                           
         DC   CL14'1R0303001BL ',CL14'1R0303002597  '                           
         DC   CL14'1R0303001CO ',CL14'1R0303002603  '                           
         DC   CL14'1R0303002RV ',CL14'1R0303002600  '                           
         DC   CL14'1R0303004RM ',CL14'1R0303002596  '                           
         DC   CL14'1R0303004SG ',CL14'1R0303002599  '                           
         DC   CL14'1R0303005CA ',CL14'1R0303002604  '                           
         DC   CL14'1R0303007SK ',CL14'1R0303002601  '                           
         DC   CL14'1R0303009DM ',CL14'1R0303002602  '                           
         DC   CL14'1R0304FR2HL ',CL14'1R0304FR8001  '                           
         DC   CL14'1R0304FR6SC ',CL14'1R0304FR06SC  '                           
         DC   CL14'1R030400MM3 ',CL14'1R0304002635  '                           
         DC   CL14'1R0304000IH ',CL14'1R0304002607  '                           
         DC   CL14'1R0304002HL ',CL14'1R0304008001  '                           
         DC   CL14'1R0304003EM ',CL14'1R0304002610  '                           
         DC   CL14'1R0304004DP ',CL14'1R03040004DP  '                           
         DC   CL14'1R0304004TD ',CL14'1R0304002611  '                           
         DC   CL14'1R0304005BK ',CL14'1R0304002609  '                           
         DC   CL14'1R0304006CP ',CL14'1R0304002608  '                           
         DC   CL14'1R0304007DM ',CL14'1R0304002605  '                           
         DC   CL14'1R0304007LF ',CL14'1R0304002606  '                           
         DC   CL14'1R0306FR3CI ',CL14'1R0306FR03CI  '                           
         DC   CL14'1R030600DMO ',CL14'1R0306002618  '                           
         DC   CL14'1R030600LM3 ',CL14'1R0306002616  '                           
         DC   CL14'1R0306002AL ',CL14'1R0306002614  '                           
         DC   CL14'1R0306002JV ',CL14'1R0306002613  '                           
         DC   CL14'1R0306003DP ',CL14'1R0306002617  '                           
         DC   CL14'1R0306004KM ',CL14'1R0306002612  '                           
         DC   CL14'1R0306006JT ',CL14'1R0306002619  '                           
         DC   CL14'1R0306007MW ',CL14'1R0306002615  '                           
         DC   CL14'1R0307FR0DM ',CL14'1R0307FR1104  '                           
         DC   CL14'1R0307FR2BM ',CL14'1R0307FR2674  '                           
         DC   CL14'1R0307FR8DC ',CL14'1R0307FR2676  '                           
         DC   CL14'1R0307FR9LP ',CL14'1R0307FR2675  '                           
         DC   CL14'1R0307001CZ ',CL14'1R0307002622  '                           
         DC   CL14'1R0307002BM ',CL14'1R0307002674  '                           
         DC   CL14'1R0307002PH ',CL14'1R0307002621  '                           
         DC   CL14'1R0307003TP ',CL14'1R0307002623  '                           
         DC   CL14'1R0307005CK ',CL14'1R0307002620  '                           
         DC   CL14'1R0307007LL ',CL14'1R0307002497  '                           
         DC   CL14'1R0308FR1JS ',CL14'1R0308FR01JS  '                           
         DC   CL14'1R0308FR1LJ ',CL14'1R0308FR01LJ  '                           
         DC   CL14'1R0308FR5AC ',CL14'1R0308FR05AC  '                           
         DC   CL14'1R0308FR7DT ',CL14'1R0308FR07DT  '                           
         DC   CL14'1R0308002FG ',CL14'1R0308001105  '                           
         DC   CL14'1R0308003MJ ',CL14'1R0308002628  '                           
         DC   CL14'1R0308004CH ',CL14'1R0308002627  '                           
         DC   CL14'1R0308005SM ',CL14'1R0308002624  '                           
         DC   CL14'1R0308006CM ',CL14'1R0308002626  '                           
         DC   CL14'1R0308006MM ',CL14'1R0308002625  '                           
         DC   CL14'1R0308009DB ',CL14'1R0308002629  '                           
         DC   CL14'1R0309FR1JR ',CL14'1R0309FR01JR  '                           
         DC   CL14'1R0309FR1LP ',CL14'1R0309FR01LP  '                           
         DC   CL14'1R0309FR2JM ',CL14'1R0309FR02JM  '                           
         DC   CL14'1R0309FR2SO ',CL14'1R0309FR02SO  '                           
         DC   CL14'1R0309FR6SC ',CL14'1R0309FR06SC  '                           
         DC   CL14'1R0309FR7DG ',CL14'1R0309FR07DG  '                           
         DC   CL14'1R0309FR9JP ',CL14'1R0309FR09JP  '                           
         DC   CL14'1R030900JS7 ',CL14'1R0309002637  '                           
         DC   CL14'1R030900MM3 ',CL14'1R0309002635  '                           
         DC   CL14'1R0309002ME ',CL14'1R0309002633  '                           
         DC   CL14'1R0309003AR ',CL14'1R0309002631  '                           
         DC   CL14'1R0309005AF ',CL14'1R0309002636  '                           
         DC   CL14'1R0309005PC ',CL14'1R0309002634  '                           
         DC   CL14'1R0309006JC ',CL14'1R0309002632  '                           
         DC   CL14'1R0309006JD ',CL14'1R0309002630  '                           
         DC   CL14'1R0310FR7DK ',CL14'1R0310FR07DK  '                           
         DC   CL14'1R031000JB0 ',CL14'1R0310002638  '                           
         DC   CL14'1R0311000LI ',CL14'1R0311002639  '                           
         DC   CL14'1R0311001JK ',CL14'1R0311001118  '                           
         DC   CL14'1R0311002CD ',CL14'1R0311002640  '                           
         DC   CL14'1R0312FR3LC ',CL14'1R0312FR03LC  '                           
         DC   CL14'1R0314FR1JS ',CL14'1R0314FR01JS  '                           
         DC   CL14'1R0314FR1LJ ',CL14'1R0314FR01LJ  '                           
         DC   CL14'1R0314FR5AC ',CL14'1R0314FR05AC  '                           
         DC   CL14'1R0314001JK ',CL14'1R0314001118  '                           
         DC   CL14'1R0314002FG ',CL14'1R0314001105  '                           
         DC   CL14'1R0314003MJ ',CL14'1R0314002628  '                           
         DC   CL14'1R0314004CH ',CL14'1R0314002627  '                           
         DC   CL14'1R0314005SM ',CL14'1R0314002624  '                           
         DC   CL14'1R0314006CM ',CL14'1R0314002626  '                           
         DC   CL14'1R0314006MM ',CL14'1R0314002625  '                           
         DC   CL14'1R0314009DB ',CL14'1R0314002629  '                           
         DC   CL14'1R0315FR7DK ',CL14'1R0315FR07DK  '                           
         DC   CL14'1R031500JB0 ',CL14'1R0315002638  '                           
         DC   CL14'1R0317002HL ',CL14'1R0317008001  '                           
         DC   CL14'1R0401002KN ',CL14'1R0101002567  '                           
         DC   CL14'1R0401004TD ',CL14'1R0401002611  '                           
         DC   CL14'1R040200CS0 ',CL14'1R0402002586  '                           
         DC   CL14'1R0402001LV ',CL14'1R0402002585  '                           
         DC   CL14'1R040300JS7 ',CL14'1R0403002637  '                           
         DC   CL14'1R0403004SG ',CL14'1R0403002599  '                           
         DC   CL14'1R040600DMO ',CL14'1R0406002618  '                           
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        OTHER INCLUDES                                               *         
***********************************************************************         
                                                                                
* DDDPRINT                                                                      
* ACGENFILE                                                                     
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACLDXPER  11/11/99'                                      
         END                                                                    

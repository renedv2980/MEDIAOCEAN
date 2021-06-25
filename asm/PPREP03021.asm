*          DATA SET PPREP03021 AT LEVEL 041 AS OF 05/01/02                      
*PHASE PP03021,+0,NOAUTO                                                        
*INCLUDE PPFRGTAB                                                               
*INCLUDE PPSPCVAL                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE XSORT                                                                  
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL READ ALL BUYS AND PRINT OUT SPACE DESCRIPTION             
*   FROM X'20' ELEMENT. THEN USING PPSPCVAL MODULE TO GET BINARY                
*   REPSENTATION OF THE CORRESPONDING SPACE DESCRIPTION.                        
*   FINALLY, PRINT THE REPORT OF ALL FINDINGS.                                  
*                                                                               
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                    
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
*                                                                               
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'         BUYS READ                                    
         ZAP   SPACNT,=P'0'        SPACE DESCRIPTION ELEMS COUNTER              
         ZAP   ERRSCNT,=P'0'       ERROR FOUND IN SPACE DESCRIPTION             
         ZAP   UERRCNT,=P'0'       UNIQUE ERROR SPACE DESP COUNTER              
         ZAP   MEDMCNT,=P'0'       MEDIA M COUNTER                              
         ZAP   MEDSCNT,=P'0'       MEDIA S COUNTER                              
         ZAP   MEDTCNT,=P'0'       MEDIA T COUNTER                              
         ZAP   SKIPCNT,=P'0'       BUY RECS SKIPPED                             
         ZAP   DUMPCNT,=P'0'                                                    
         MVC   VFRGTAB,=V(PPFRGTAB)                                             
         XC    WKAREA,WKAREA                                                    
*                                                                               
         L     RE,=A(ERRSTAB)                                                   
         ST    RE,AERRSTAB         ADDRESS OF ERROR SPACE DESCRIPTOIN           
         L     RF,=A(84021)                                                     
         XCEF                                                                   
*                                                                               
* CLEAR BINSRCH TABLE AND SETUP IT'S PARAMETERS                                 
*                                                                               
         L     RE,=A(BINTAB)                                                    
         ST    RE,BINATAB          ADDRESS OF BINSRCH TABLE                     
         L     RF,=A(54000)        LENGTH OF TABLE                              
         XCEF                                                                   
         XC    BINPARMS(4),BINPARMS                                             
         XC    BINCOUNT(4),BINCOUNT                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'20'          BUYS                                        
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    DS    0H                                                               
         CLC   KEY(2),=C'BD'       ONLY DOING AGENCY BD                         
         BNE   EXIT                                                             
         CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              END OF MEDIA GO DO NEXT                      
*                                                                               
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   KEY+2,C'M'          MEDIA M?                                     
         BNE   *+14                                                             
         AP    MEDMCNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'S'          MEDIA S?                                     
         BNE   *+14                                                             
         AP    MEDSCNT,=P'1'                                                    
         B     PROC8                                                            
         CLI   KEY+2,C'T'          MEDIA T?                                     
         BNE   *+14                                                             
         AP    MEDTCNT,=P'1'                                                    
         B     PROC8                                                            
*                                                                               
         AP    SKIPCNT,=P'1'       THIS REC NEED TO BE SKIPPED                  
         B     PROC3               NEXT BUY REC                                 
*                                                                               
PROC8    DS    0H                                                               
         GOTO1 GETPRT              GET BUY REC                                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROC10   DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'         BUY DESCRIPTION ELEM                         
         BE    PROC20                                                           
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC20   DS    0H                                                               
         AP    SPACNT,=P'1'        SPACE DESCRIPTION ELEM COUNTER               
         USING PBDELEM,R2                                                       
         MVC   WORK(L'PBDSPACE),PBDSPACE                                        
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(PPSPCVAL),DMCB,(17,WORK),WKAREA,VFRGTAB                       
         CLI   12(R1),0                                                         
         BE    PROC40              NO ERROR FROM PPSPCVAL                       
         AP    ERRSCNT,=P'1'                                                    
         CP    UERRCNT,=P'4000'                                                 
         BH    PROC3               ONLY STORES 4000 UNIQUE ENTRIES              
         L     RE,AERRSTAB                                                      
PROC30   CLC   0(17,RE),WORK       SEE IF ALREADY IN TABLE                      
         BE    PROC35              NEXT SEQ                                     
         OC    0(17,RE),0(RE)      SEE IF BLANK ENTRY                           
         BZ    PROC32                                                           
         LA    RE,21(RE)           NEXT ENTRY                                   
         B     PROC30                                                           
*                                                                               
PROC32   MVC   0(17,RE),WORK       PUT INCORRECT SPACE DESP IN TABLE            
         AP    UERRCNT,=P'1'                                                    
*                                                                               
PROC35   ZICM  RF,17(RE),(15)                                                   
         AHI   RF,1                                                             
         STCM  RF,15,17(RE)                                                     
*                                                                               
         B     PROC3               NEXT SEQ                                     
*                                                                               
PROC40   DS    0H                                                               
         MVC   WKAREA+15(17),WORK                                               
         XC    WKAREA+32(4),WKAREA+32                                           
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WKAREA)                              
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         B     PROC3               NEXT SEQ                                     
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'         SKIP TO NEXT AGY/MED                         
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'20'         BUYS                                         
         B     PROC2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNL     DS    0H                                                               
         MVI   WORK,X'FF'                                                       
         MVC   WORK+1(BINKEYLQ-1),WORK                                          
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         XC    WORK(BINKEYLQ),WORK                                              
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)                                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         ZICM  R3,1(R1),(7)        POINT R3 TO TABLE - A(FOUND RECORD)          
*                                                                               
RUNL30   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    RUNL40              YES - FINISH UP                              
*                                                                               
         MVC   P+05(17),15(R3)                                                  
         EDIT  (B4,00(R3)),(08,P+030),5,ZERO=NOBLANK,ALIGN=LEFT                 
         EDIT  (B4,04(R3)),(08,P+040),5,ZERO=NOBLANK,ALIGN=LEFT                 
         EDIT  (B4,08(R3)),(08,P+050),5,ZERO=NOBLANK,ALIGN=LEFT                 
*                                                                               
         EDIT  (B1,12(R3)),(03,P+065),0,ZERO=NOBLANK,ALIGN=LEFT                 
         CLI   12(R3),0                                                         
         BE    RUNL33H                                                          
         LA    R5,12(R3)                                                        
         BAS   RE,RUNL35                                                        
         MVC   P+070(12),0(R6)     GET SPACE NAME                               
*                                                                               
RUNL33H  EDIT  (B1,13(R3)),(03,P+085),0,ZERO=NOBLANK,ALIGN=LEFT                 
         CLI   13(R3),0                                                         
         BE    RUNL33M                                                          
         LA    R5,13(R3)                                                        
         BAS   RE,RUNL35                                                        
         MVC   P+090(12),0(R6)     GET SPACE NAME                               
*                                                                               
RUNL33M  EDIT  (B1,14(R3)),(03,P+105),0,ZERO=NOBLANK,ALIGN=LEFT                 
         CLI   14(R3),0                                                         
         BE    RUNL33X                                                          
         LA    R5,14(R3)                                                        
         BAS   RE,RUNL35                                                        
         MVC   P+110(12),0(R6)     GET SPACE NAME                               
*                                                                               
RUNL33X  DS    0H                                                               
         GOTO1 REPORT              FINALLY PRINT EVERYTHING                     
         LA    R3,BINRECLQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     RUNL30                                                           
*                                                                               
*                                                                               
*                                                                               
RUNL35   DS    0H                  SEARCH TABLE FOR SPACE NAME                  
         L      R6,VFRGTAB                                                      
RUNL35H  CLI    0(R6),X'FF'        END OF TABLE?                                
         BNE    *+6                                                             
         DC     H'0'               ENTRY MUST BE FOUND IN TABLE!                
         CLC    14(1,R6),0(R5)     R5 POINTS TO MATCHING CODE                   
         BE     RUNL35X                                                         
         LA     R6,20(R6)          NEXT ENTRY IN FRGTAB                         
         B      RUNL35H                                                         
RUNL35X  BR     RE                                                              
*                                                                               
*                                                                               
*                                                                               
RUNL40   DS    0H                                                               
         GOTO1 REPORT              SHIP A LINE                                  
*                                                                               
         MVC   P+05(42),=C'TOTAL SPACE DESCRIPTION IN BINSRCH TABLE: '          
         EDIT  (B4,BINCOUNT),(13,P+47),ZERO=NOBLANK,ALIGN=LEFT,        +        
               COMMAS=YES                                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SHIP A LINE                                  
*                                                                               
* SORTE TABLE OF OF INCORRECT SPACE DESCRIPTION ITEMS                           
*                                                                               
         GOTO1 =V(XSORT),DMCB,(0,AERRSTAB),4001,21,17,0                         
*                                                                               
* PRINT OUT SORTED LIST OF INCORRECT SPACE DESCRIPTION ITEMS                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+05(36),=C'LIST OF INCORRECT SPACE DESCRIPTIONS'                
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
         SR    R3,R3               TOTAL ERROR SPACE DESP COUNTER               
         LA    R4,P+5                                                           
         L     R5,AERRSTAB                                                      
         SR    R6,R6               PER PRINTLINE COUNTER                        
         XC    TOTCNT,TOTCNT       USED FOR TOTALING                            
         XC    FULL,FULL           USED FOR LOOP CONTROL                        
*                                                                               
RUNL41   DS    0H                                                               
         L     RE,FULL                                                          
         AHI   RE,1                                                             
         ST    RE,FULL                                                          
         CHI   RE,4001                                                          
         BH    RUNL45              DONE                                         
*                                                                               
         OC    0(17,R5),0(R5)                                                   
         BZ    RUNL43              NOTHING TO PRINT, NEXT ENTRY                 
         AHI   R3,1                                                             
*                                                                               
         CHI   R6,3                THREE PER PRINTLINE                          
         BL    RUNL42                                                           
         GOTO1 REPORT                                                           
         LA    R4,P+5              REPOINT TO PRINTLINE                         
         SR    R6,R6               RESET PER PRINTLINE COUNTER                  
*                                                                               
RUNL42   MVC   0(17,R4),0(R5)                                                   
         ZICM  RE,17(R5),(15)      KEEP TRACK OF TOTAL                          
         A     RE,TOTCNT                                                        
         ST    RE,TOTCNT                                                        
         EDIT  (B4,17(R5)),(10,20(R4)),ZERO=NOBLANK,ALIGN=LEFT,        +        
               COMMAS=YES                                                       
         LA    R4,35(R4)                                                        
         AHI   R6,1                                                             
RUNL43   LA    R5,21(R5)           NEXT ENTRY                                   
         B     RUNL41              CHK NEXT ENTRY IN TABLE                      
*                                                                               
RUNL45   DS    0H                                                               
         GOTO1 REPORT              PRINT OUT WHATEVER LEFT IN PLINE             
         GOTO1 REPORT                                                           
         MVC   P+05(26),=C'TOTAL OF UNIQUE INCORRECT '                          
         MVC   P+31(20),=C'SPACE DESCRIPTIONS: '                                
         EDIT  (R3),(13,P+51),ZERO=NOBLANK,ALIGN=LEFT,COMMAS=YES                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+05(19),=C'TOTAL OF INCORRECT '                                 
         MVC   P+24(20),=C'SPACE DESCRIPTIONS: '                                
         EDIT  (B4,TOTCNT),(13,P+44),ZERO=NOBLANK,ALIGN=LEFT,COMMAS=YES         
         GOTO1 REPORT                                                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL80                                                           
         MVC   P(CNTLNQ-08),8(R4)                                               
         OI    7(R4),X'0F'                                                      
         UNPK  P+CNTLNQ+05(10),0(8,R4)                                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
         LA    R4,CNTLNQ(R4)                                                    
         B     RUNL50                                                           
*                                                                               
RUNL80   DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPRT     NTR1                                                                   
*                                                                               
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CHI   R4,32                                                            
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
CNTLNQ   EQU   8+30                                                             
*                                                                               
INCNT    DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS READ'                                        
MEDMCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA M'                                 
MEDSCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA S'                                 
MEDTCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS FOR MEDIA T'                                 
SKIPCNT  DS    PL8                                                              
         DC    CL30'TOTAL BUY RECS SKIPPED'                                     
SPACNT   DS    PL8                                                              
         DC    CL30'TOTAL SPACE DESRIPTION ELEM'                                
ERRSCNT  DS    PL8                                                              
         DC    CL30'TOTAL ERROR IN SPACE ELEM'                                  
UERRCNT  DS    PL8                                                              
         DC    CL30'TOTAL UNIQUE ERROR IN SPACE'                                
         DC    X'FF'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BINKEYLQ EQU   15                                                               
BINRECLQ EQU   36                                                               
*                                                                               
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(0)                                                             
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(36)               LENGTH                                       
         DC    AL1(0),AL3(BINKEYLQ)                                             
BINMAX   DC    A(1500)             MAX NUMBER OF RECORDS                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DUMPCNT  DS    PL8                                                              
*                                                                               
TOTCNT   DS    F                                                                
SAVEKEY  DS    CL32                                                             
WKAREA   DS    CL255                                                            
VFRGTAB  DS    V                                                                
AERRSTAB DS    A                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BINTAB   CSECT                     TABLE FOR BINSRCH                            
*                                                                               
         DS    CL54000                                                          
BTABXQ   EQU   *                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ERRSTAB  CSECT                     TABLE FOR SPACE DESCRIPTION ERRORS           
*                                                                               
         DS    4001CL21            17 FOR SPACE DESP AND 4 FOR COUNT            
ERRSTXQ  EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041PPREP0302105/01/02'                                      
         END                                                                    

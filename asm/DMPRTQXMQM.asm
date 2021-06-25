*          DATA SET DMPRTQXMQM AT LEVEL 038 AS OF 11/10/11                      
*PROCESS USING(WARN(15))                                                        
*PHASE PQXMQMA                                                                  
         TITLE 'PRTQEXTE - PRTQXFR EXTERNAL ROUTINE FOR EASYLINK'               
* PARAMETER LIST LAYOUT                                                         
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IS HEADER RECORD              
*                                   X'02'= RECORD IS DATA RECORD                
*                                   X'FF'= RECORD IS EOF REC                    
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOF'=PURGE & CAUSE EOF              
* P2=N/D                                                                        
*                                                                               
*                                                                               
* P3=1ST BYTE                       C'Y' = YOU ASKED ME TO RETURN               
*                                   C'R' = RETURN BACK TO EXTERNAL              
* P4=N/D                                                                        
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
DMPQEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMPQEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITIALISE                     
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         CALL TO PROCESS HEADER                       
         BE    DMXHDR                                                           
         CLI   PLIST,X'02'         CALL TO PROCESS RECORD                       
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         CALL ON EOF REC                              
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED              *         
***********************************************************************         
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
*                                                                               
         OPEN  (EXTSYSIN)          OPEN EXTERN PARAMETER FILE                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,RCPTAB                                                        
         LA    R5,CCRTAB                                                        
         LA    R6,BCCTAB                                                        
*                                                                               
DMXIN10  GET   EXTSYSIN,CARD                                                    
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    DMXIN10                                                          
*                                                                               
         CLC   =C'WIDE=Y',CARD                                                  
         BNE   *+12                                                             
         MVI   HDRREC+34,C'W'      WIDE FORMAT 132 BYTES                        
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'TO=',CARD                                                     
         BNE   DMXIN20                                                          
         MVC   0(MAILADRQ,R4),CARD+3                                            
         AHI   R4,MAILADRQ                                                      
         C     R4,=A(RCPTEND)                                                   
         BNH   DMXIN10                                                          
         DC    H'0'                                                             
*                                                                               
DMXIN20  CLC   =C'CC=',CARD                                                     
         BNE   DMXIN30                                                          
         MVC   0(MAILADRQ,R5),CARD+3                                            
         AHI   R5,MAILADRQ                                                      
         C     R5,=A(CCRTEND)                                                   
         BNH   DMXIN10                                                          
         DC    H'0'                                                             
*                                                                               
DMXIN30  CLC   =C'BCC=',CARD                                                    
         BNE   DMXIN40                                                          
         MVC   0(MAILADRQ,R6),CARD+4                                            
         AHI   R6,MAILADRQ                                                      
         C     R6,=A(BCCTEND)                                                   
         BNH   DMXIN10                                                          
         DC    H'0'                                                             
*                                                                               
DMXIN40  CLC   =C'SUBJECT=',CARD                                                
         BNE   *+14                                                             
         MVC   SUBJECT,CARD+8                                                   
         B     DMXIN10                                                          
*                                                                               
         DC    H'0'                UNKNOWN CONTROL CARD                         
*                                                                               
DMXINX   CLOSE (EXTSYSIN)          CLOSE EXTERN PARAMETER FILE                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    RCPTAB(MAILADRQ),RCPTAB   E-MAIL ADDRESS IS REQUIRED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SUBREC+15(L'SUBJECT),SUBJECT                                     
*                                                                               
         LA    R1,HDRRECLN                                                      
         STCM  R1,3,HDRRECOV                                                    
         LA    R1,TRNRECLN                                                      
         STCM  R1,3,TRNRECOV                                                    
         LA    R1,RCPRECLN                                                      
         STCM  R1,3,RCPRECOV                                                    
         LA    R1,SUBRECLN                                                      
         STCM  R1,3,SUBRECOV                                                    
*                                                                               
         MVC   AMYREC,=A(HDRREC)                                                
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS HEADER LOGIC - RECORD IN AREC - NO RETURN ALLOWED           *         
***********************************************************************         
         SPACE 1                                                                
DMXHDR   L     R9,AREC             MODIFY HEADER                                
         USING QLINDEX,R9                                                       
*                                                                               
         MVI   QLCLASS,C'G'        SET CLASS TO G                               
         MVI   QLLINEW,132         SET REPORT WIDTH TO 132                      
*                                                                               
         B     DMXKEEP                                                          
         DROP  R9                                                               
***********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED              *         
***********************************************************************         
         SPACE 1                                                                
DMXREC   L     R9,AREC                                                          
         CLI   FIRSTREC,C'Y'                                                    
         BE    DMXR10                                                           
         CLI   8(R1),C'Y'          IS THIS A RETURN TO ME                       
         BNE   DMXKEEP                                                          
         B     DMXR20                                                           
*                                                                               
DMXR10   DS    0H                                                               
         CLC   =C'SUBJECT=',5(R9)  SUBJECT= IS FIRST INPUT FILE RECORD?         
         BNE   DMXR15              NO                                           
         CLC   SUBJECT,BLANKS      SUBJECT OVERRIDDEN VIA CONTROL CARD?         
         BNE   *+10                YES                                          
         MVC   SUBREC+15(L'SUBJECT),13(R9)  NO: TAKE IT FROM INPUT FILE         
         B     DMXPURGE                                                         
*                                                                               
DMXR15   DS    0H                                                               
         CLC   SUBREC+15(L'SUBJECT),BLANKS    SUBJECT IS REQUIRED               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   FIRSTREC,C'N'                                                    
         BAS   RE,SAVELINE         SAVE THIS LINE                               
*                                                                               
DMXR20   MVC   0(132,R9),BLANKS                                                 
*                                                                               
         CLC   AMYREC,=A(HDRREC)                                                
         BNE   *+20                                                             
         MVC   0(HDRRECLN,R9),HDRRECOV                                          
         MVC   AMYREC,=A(TRNREC)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         CLC   AMYREC,=A(TRNREC)                                                
         BNE   *+20                                                             
         MVC   0(TRNRECLN,R9),TRNRECOV                                          
         MVC   AMYREC,=A(RCPTAB)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         CLC   AMYREC,=A(RCPTEND)                                               
         BNL   DMXR30                                                           
         CLC   AMYREC,=A(RCPTAB)                                                
         BL    DMXR30                                                           
         L     R3,AMYREC                                                        
         CLC   AMYREC,=A(RCPTEND)                                               
         BNL   *+14                                                             
         OC    0(MAILADRQ,R3),0(R3)                                             
         BNZ   *+14                                                             
         MVC   AMYREC,=A(CCRTAB)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         MVC   RCPREC+15(MAILADRQ),0(R3)                                        
         MVC   0(RCPRECLN,R9),RCPRECOV                                          
         AHI   R3,MAILADRQ                                                      
         ST    R3,AMYREC                                                        
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR30   CLC   AMYREC,=A(CCRTEND)                                               
         BNL   DMXR40                                                           
         CLC   AMYREC,=A(CCRTAB)                                                
         BL    DMXR40                                                           
         L     R3,AMYREC                                                        
         CLC   AMYREC,=A(CCRTEND)                                               
         BNL   *+14                                                             
         OC    0(MAILADRQ,R3),0(R3)                                             
         BNZ   *+14                                                             
         MVC   AMYREC,=A(BCCTAB)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         MVC   CCRREC+15(MAILADRQ),0(R3)                                        
         MVC   0(CCRRECLN,R9),CCRRECOV                                          
         AHI   R3,MAILADRQ                                                      
         ST    R3,AMYREC                                                        
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR40   CLC   AMYREC,=A(BCCTEND)                                               
         BNL   DMXR50                                                           
         CLC   AMYREC,=A(BCCTAB)                                                
         BL    DMXR50                                                           
         L     R3,AMYREC                                                        
         CLC   AMYREC,=A(BCCTEND)                                               
         BNL   *+14                                                             
         OC    0(MAILADRQ,R3),0(R3)                                             
         BNZ   *+14                                                             
         MVC   AMYREC,=A(SUBREC)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         MVC   BCCREC+15(MAILADRQ),0(R3)                                        
         MVC   0(BCCRECLN,R9),BCCRECOV                                          
         AHI   R3,MAILADRQ                                                      
         ST    R3,AMYREC                                                        
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR50   CLC   AMYREC,=A(SUBREC)                                                
         BNE   *+20                                                             
         MVC   0(SUBRECLN,R9),SUBRECOV                                          
         XC    AMYREC,AMYREC                                                    
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         OC    AMYREC,AMYREC                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,RESTLINE         RESTORE THIS LINE                            
         B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
***********************************************************************         
         SPACE                                                                  
DMXEOF   L     R9,AREC                                                          
*                                                                               
         B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
*        SAVE LINE FOR LATER                                          *         
***********************************************************************         
         SPACE                                                                  
SAVELINE SR    R1,R1               SAVE THIS LINE                               
         ICM   R1,3,0(R9)                                                       
         CH    R1,=Y(L'SAVELIN)                                                 
         BNH   *+6                                                              
         DC    H'0'                TOO BIG FOR SAVELIN                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVELIN(0),0(R9)                                                 
         BR    RE                                                               
*                                                                               
RESTLINE SR    R1,R1               RESTORE THIS LINE                            
         ICM   R1,3,SAVELIN                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R9),SAVELIN                                                  
         BR    RE                                                               
*                                                                               
SAVELIN  DS    CL200               SAVE THE LINE HERE                           
         EJECT                                                                  
EXTSYSIN DCB   DDNAME=EXTSYSIN,MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,    +        
               EODAD=DMXINX                                                     
         SPACE 3                                                                
BLANKS   DC    CL200' '                                                         
         SPACE 3                                                                
CARD     DS    CL80                                                             
FIRSTREC DC    C'Y'                                                             
SUBJECT  DC    CL60' '                                                          
AMYREC   DS    A                                                                
HDRRECOV DC    X'00000000',X'09'                                                
HDRREC   DC    C'    *HDR*                          P                  +        
                              M'                                                
HDRRECLN EQU   *-HDRRECOV                                                       
TRNRECOV DC    X'00000000',X'09'                                                
TRNREC   DC    C'++DDS X XXXTRN'                                                
TRNRECLN EQU   *-TRNRECOV                                                       
RCPRECOV DC    X'00000000',X'09'                                                
RCPREC   DC    CL80'++DDS      RCP '                                            
RCPRECLN EQU   *-RCPRECOV                                                       
CCRRECOV DC    X'00000000',X'09'                                                
CCRREC   DC    CL80'++DDS      CCR '                                            
CCRRECLN EQU   *-CCRRECOV                                                       
BCCRECOV DC    X'00000000',X'09'                                                
BCCREC   DC    CL80'++DDS      BCC '                                            
BCCRECLN EQU   *-BCCRECOV                                                       
MAX      EQU   10                                                               
MAILADRQ EQU   60                                                               
RCPTAB   DS    (MAX)XL(MAILADRQ)                                                
RCPTEND  EQU   *                                                                
CCRTAB   DS    (MAX)XL(MAILADRQ)                                                
CCRTEND  EQU   *                                                                
BCCTAB   DS    (MAX)XL(MAILADRQ)                                                
BCCTEND  EQU   *                                                                
SUBRECOV DC    X'00000000',X'09'                                                
SUBREC   DC    CL80'++DDS      SUB '                                            
SUBRECLN EQU   *-SUBRECOV                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*DMPRTQL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
         SPACE                                                                  
* DSECT TO COVER LOCAL W/STORAGE                                                
         SPACE                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL80                                                             
APARM    DS    F                                                                
SAVERE   DS    F                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
         DS    A                                                                
         DS    A                                                                
         DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038DMPRTQXMQM11/10/11'                                      
         END                                                                    

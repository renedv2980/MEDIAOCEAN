*          DATA SET DMPRTQXEZ  AT LEVEL 014 AS OF 08/28/08                      
*PHASE PQXEZA                                                                   
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
DMXIN10  GET   EXTSYSIN,CARD                                                    
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    DMXIN10                                                          
*                                                                               
         CLC   =C'FAXNUM=',CARD                                                 
         BNE   *+14                                                             
         MVC   FAXNUM,CARD+7                                                    
         B     DMXIN10                                                          
*                                                                               
         DC    H'0'                UNKNOWN CONTROL CARD                         
*                                                                               
DMXINX   CLOSE (EXTSYSIN)          OPEN EXTERN PARAMETER FILE                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   FAXNUM,BLANKS       FAX NUMBER IS REQUIRED                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   HDRREC+13(L'FAXNUM),FAXNUM                                       
         LA    R1,HDRRECLN                                                      
         STCM  R1,3,HDRRECOV                                                    
         LA    R1,TRNRECLN                                                      
         STCM  R1,3,TRNRECOV                                                    
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
DMXR10   MVI   FIRSTREC,C'N'                                                    
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
FAXNUM   DC    CL20' '                                                          
AMYREC   DS    A                                                                
HDRRECOV DC    X'00000000',X'09'                                                
HDRREC   DC    C'    *HDR*FAX                      WP'                          
HDRRECLN EQU   *-HDRRECOV                                                       
TRNRECOV DC    X'00000000',X'09'                                                
TRNREC   DC    C'++DDS X XXXTRN'                                                
TRNRECLN EQU   *-TRNRECOV                                                       
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
**PAN#1  DC    CL21'014DMPRTQXEZ 08/28/08'                                      
         END                                                                    

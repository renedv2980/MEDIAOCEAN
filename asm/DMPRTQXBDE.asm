*          DATA SET DMPRTQXBDE AT LEVEL 052 AS OF 09/03/08                      
*PHASE PQXBDEA                                                                  
         TITLE 'PRTQEXT2 - PRTQXFR EXTERNAL ROUTINE FOR BDE'                    
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
         CLC   =C'EDICTKEY=',CARD                                               
         BNE   *+14                                                             
         MVC   EDICTKEY,CARD+9                                                  
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'TO=',CARD                                                     
         BNE   *+14                                                             
         MVC   EMAIL,CARD+3                                                     
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'DIR=',CARD                                                    
         BNE   *+18                                                             
         MVC   DIR,CARD+4                                                       
         MVI   HDRREC+37,C'R'            DIR IS PROVIDED                        
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'DSN=',CARD                                                    
         BNE   *+18                                                             
         MVC   DSN,CARD+4                                                       
         MVI   HDRREC+37,C'D'            DSN IS PROVIDED                        
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'HFS=',CARD                                                    
         BNE   *+18                                                             
         MVC   DSN,CARD+4                                                       
         MVI   HDRREC+37,C'H'            HFS IS PROVIDED                        
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'FILE=',CARD                                                   
         BNE   *+14                                                             
         MVC   FIL,CARD+5                                                       
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'EXT=',CARD                                                    
         BNE   *+14                                                             
         MVC   EXT,CARD+4                                                       
         B     DMXIN10                                                          
*                                                                               
         CLC   =C'SUBJECT=',CARD                                                
         BNE   *+14                                                             
         MVC   SUBJECT,CARD+8                                                   
         B     DMXIN10                                                          
*                                                                               
         B     DMXIN10             IGNORE UNKNOWN CONTROL CARD                  
*                                                                               
DMXINX   CLOSE (EXTSYSIN)          CLOSE EXTERN PARAMETER FILE                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SUBJECT,BLANKS      SUBJECT IS REQUIRED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   EDICTKEY,BLANKS                                                  
         BE    *+10                                                             
         MVC   HDRREC+15(8),EDICTKEY                                            
*                                                                               
         MVC   SUBREC+15(L'SUBJECT),SUBJECT                                     
         MVC   RCPREC+15(L'EMAIL),EMAIL                                         
         MVC   FILREC+15(L'FIL),FIL                                             
         MVC   EXTREC+15(L'EXT),EXT                                             
         MVC   DSNREC+15(L'DSN),DSN                                             
         MVC   DIRREC+15(L'DIR),DIR                                             
*                                                                               
         LA    R1,HDRRECLN                                                      
         STCM  R1,3,HDRRECOV                                                    
         LA    R1,TRNRECLN                                                      
         STCM  R1,3,TRNRECOV                                                    
         LA    R1,RCPRECLN                                                      
         STCM  R1,3,RCPRECOV                                                    
         LA    R1,SUBRECLN                                                      
         STCM  R1,3,SUBRECOV                                                    
         LA    R1,FILRECLN                                                      
         STCM  R1,3,FILRECOV                                                    
         LA    R1,EXTRECLN                                                      
         STCM  R1,3,EXTRECOV                                                    
         LA    R1,DSNRECLN                                                      
         STCM  R1,3,DSNRECOV                                                    
         LA    R1,DIRRECLN                                                      
         STCM  R1,3,DIRRECOV                                                    
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
         MVC   AMYREC,=A(RCPREC)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         CLC   AMYREC,=A(RCPREC)                                                
         BNE   DMXR60                                                           
         MVC   AMYREC,=A(SUBREC)                                                
         CLC   EMAIL,BLANKS                                                     
         BE    DMXR60                                                           
         MVC   0(RCPRECLN,R9),RCPRECOV                                          
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR60   CLC   AMYREC,=A(SUBREC)                                                
         BNE   DMXR70                                                           
         MVC   0(SUBRECLN,R9),SUBRECOV                                          
         MVC   AMYREC,=A(DSNREC)                                                
         CLI   HDRREC+37,C'D'            DSN IS PROVIDED                        
         BE    DMXKERET                                                         
         CLI   HDRREC+37,C'H'            HFS FILE IS PROVIDED                   
         BE    DMXKERET                                                         
         MVC   AMYREC,=A(DIRREC)                                                
         CLI   HDRREC+37,C'R'            DIR IS PROVIDED                        
         BE    DMXKERET                                                         
         MVC   AMYREC,=A(FILREC)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR70   CLC   AMYREC,=A(DSNREC)                                                
         BNE   *+20                                                             
         MVC   0(DSNRECLN,R9),DSNRECOV                                          
         MVC   AMYREC,=A(FILREC)                                                
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         CLC   AMYREC,=A(DIRREC)                                                
         BNE   *+20                                                             
         MVC   0(DIRRECLN,R9),DIRRECOV                                          
         XC    AMYREC,AMYREC       NO FILE/EXT AFTER THIS                       
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
         CLC   AMYREC,=A(FILREC)                                                
         BNE   DMXR80                                                           
         MVC   AMYREC,=A(EXTREC)                                                
         CLC   FIL,BLANKS                                                       
         BE    DMXR80                                                           
         MVC   0(FILRECLN,R9),FILRECOV                                          
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR80   CLC   AMYREC,=A(EXTREC)                                                
         BNE   DMXR90                                                           
         XC    AMYREC,AMYREC                                                    
         CLC   EXT,BLANKS                                                       
         BE    DMXR90                                                           
         MVC   0(EXTRECLN,R9),EXTRECOV                                          
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
DMXR90   OC    AMYREC,AMYREC                                                    
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
EDICTKEY DC    CL8' '                                                           
SUBJECT  DC    CL60' '                                                          
EMAIL    DC    CL60' '                                                          
DSN      DC    CL60' '                                                          
DIR      DC    CL60' '                                                          
FIL      DC    CL60' '                                                          
EXT      DC    CL60' '                                                          
AMYREC   DS    A                                                                
HDRRECOV DC    X'00000000',X'09'                                                
HDRREC   DC    C'    *HDR*EDICT=*BDE               WP                  +        
                               '                                                
HDRRECLN EQU   *-HDRRECOV                                                       
TRNRECOV DC    X'00000000',X'09'                                                
TRNREC   DC    C'++DDS SPTDATRN'                                                
TRNRECLN EQU   *-TRNRECOV                                                       
RCPRECOV DC    X'00000000',X'09'                                                
RCPREC   DC    CL80'++DDS      RCP '                                            
RCPRECLN EQU   *-RCPRECOV                                                       
MAILADRQ EQU   60                                                               
SUBRECOV DC    X'00000000',X'09'                                                
SUBREC   DC    CL80'++DDS      SUB '                                            
SUBRECLN EQU   *-SUBRECOV                                                       
DSNRECOV DC    X'00000000',X'09'                                                
DSNREC   DC    CL80'++DDS      DSN '                                            
DSNRECLN EQU   *-DSNRECOV                                                       
DIRRECOV DC    X'00000000',X'09'                                                
DIRREC   DC    CL80'++DDS      DIR '                                            
DIRRECLN EQU   *-DIRRECOV                                                       
FILRECOV DC    X'00000000',X'09'                                                
FILREC   DC    CL80'++DDS      FIL '                                            
FILRECLN EQU   *-FILRECOV                                                       
EXTRECOV DC    X'00000000',X'09'                                                
EXTREC   DC    CL80'++DDS      EXT '                                            
EXTRECLN EQU   *-EXTRECOV                                                       
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
**PAN#1  DC    CL21'052DMPRTQXBDE09/03/08'                                      
         END                                                                    

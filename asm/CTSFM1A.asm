*          DATA SET CTSFM1A    AT LEVEL 016 AS OF 08/22/00                      
*PHASE TA0A1AA                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE TWABLD                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFMD6 -- SCROLLER PANEL BUILD                      *         
*                                                                     *         
*  COMMENTS:     BUILDS/DISPLAYS PANEL FOR USER                       *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  CALLS TO:     SCROLLER                                             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- SCROLLD                                        *         
*                R7 -- WORK                                           *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A1A - SCROLLER PANEL BUILD/DISPLAY'                          
TA0A1A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A1A**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALREC         DO EVERYTHING AT VALREC                      
         BE    DISPLAY                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY AND CALL SCROLLER                                                
*                                                                               
DISPLAY  LA    R6,SCROLBLK         CLEAR SCROLLER PARAMETER BLOCK               
         USING SCROLLD,R6                                                       
         LR    RE,R6                                                            
         LA    RF,SCROLDLQ                                                      
         XCEFL                                                                  
*                                                                               
         L     RF,=V(RECUP)                                                     
         A     RF,RELO                                                          
         ST    RF,SDRECUP          A(RECUP)                                     
         L     RF,=V(TWABLD)                                                    
         A     RF,RELO                                                          
         ST    RF,SDTWABLD         A(TWABLD)                                    
*                                                                               
         MVC   SDCOMFCS,ACOMFACS   A(COMFACS)                                   
         ST    RA,SDTWA            A(TWA)                                       
         LA    RE,HOOK                                                          
         ST    RE,SDHOOK                                                        
         MVI   SDACTION,SDACTPAN   BUILD/DISPLAY PANEL ONLY                     
         LA    RE,SFMENDH                                                       
         MVC   0(3,RE),=X'000101'  ALWAYS MARK END OF TWA AND XMIT              
*                                                                               
         LA    R2,SFMSYPGH         VALIDATE SYS.PRG                             
         CLI   5(R2),0             REQUIRED                                     
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         CLI   5(R2),5             MUST BE 5 CHARS                              
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   SDSYSPRG,SFMSYPG                                                 
*                                                                               
         LA    R2,SFMPANLH         VALIDATE PANEL NAME                          
         CLI   5(R2),0             REQUIRED                                     
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         MVC   SDPANEL,SFMPANL                                                  
         OC    SDPANEL,SPACES      BLANK PAD                                    
*                                                                               
         LA    R2,SFMAGCYH                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   SDAGENCY,SFMAGCY    AGENCY                                       
*                                                                               
         LA    R2,SFMMEDIH                                                      
         CLI   5(R2),0             MEDIA HAS SOMETHING                          
         BE    CKCLNT              NO                                           
         CLI   SFMAGCYH+5,0        YES, AGENCY DOES NOT?                        
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDAGENCY) YES, NEED AGENCY BEFORE MEDIA              
         B     VSFMERR                                                          
         MVC   SDMEDIA,SFMMEDI     MEDIA                                        
*                                                                               
CKCLNT   LA    R2,SFMCLNTH                                                      
         CLI   5(R2),0             CLIENT HAS SOMETHING                         
         BE    ENDKEY                                                           
         CLI   SFMMEDIH+5,0        YES, MEDIA DOES NOT?                         
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDMEDIA) YES, NEED MEDIA BEFORE CLIENT               
         B     VSFMERR                                                          
         MVC   SDCLIENT,SFMCLNT    CLIENT                                       
*                                                                               
ENDKEY   MVC   SDPANTYP,CONREC     SINGLE OR MULTIPLE IN RECORD TYPE            
*                                                                               
         MVI   SDSTROW,7           DEFAULT TO ROW 7                             
         LA    R2,SFMROWH                                                       
         CLI   5(R2),0                                                          
         BE    CALLSCRL                                                         
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+14                                                             
         MVC   GERROR,=AL2(NOTNUM)                                              
         B     VSFMERR                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'6'                                                         
         BNL   *+14                                                             
         MVC   GERROR,=AL2(BADRANGE)                                            
         B     VSFMERR                                                          
         CH    R1,=H'24'                                                        
         BNH   *+14                                                             
         MVC   GERROR,=AL2(BADRANGE)                                            
         B     VSFMERR                                                          
         STC   R1,SDSTROW          START ROW                                    
*                                                                               
CALLSCRL XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A8C'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             A(SCROLLER)                                  
         GOTO1 (RF),DMCB,SCROLBLK                                               
*                                                                               
         LA    R2,SFMSYPGH                                                      
         ST    R2,ACURFORC         ALWAYS PUT CURSOR ON FIRST KEY FIELD         
         CLI   SDRETURN,SDRETOK                                                 
         BE    DISPLAYX                                                         
         CLI   SDRETURN,SDRETNOP   PANEL RECORD NOT FOUND?                      
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NOTFOUND)                                            
         B     VSFMERR                                                          
         CLI   SDRETURN,SDRETTWA   BAD TWA DESCRIPTION?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GERROR,=AL2(BADTWA)                                              
         B     VSFMERR                                                          
*                                                                               
DISPLAYX B     XIT                                                              
         EJECT                                                                  
HOOK     CLI   SDWHY,SDYFRSTQ                                                   
         BNE   *+8                                                              
         MVI   SDRETURN,X'FE'      SPECIAL CODE -- FILL DATA FIELDS             
         B     XIT                                                              
         SPACE 3                                                                
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
NDAGENCY EQU   230                                                              
NDMEDIA  EQU   231                                                              
BADRANGE EQU   247                                                              
BADTWA   EQU   248                                                              
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE DDSCROLLD                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD6D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
SCROLBLK DS    (SCROLDLQ)X         SCROLLER PARAMETER BLOCK                     
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016CTSFM1A   08/22/00'                                      
         END                                                                    

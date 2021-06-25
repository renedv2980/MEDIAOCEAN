*          DATA SET CTMAD16    AT LEVEL 229 AS OF 05/01/02                      
*PHASE TA0C16A,*                                                                
         TITLE 'TA0C16 - $MAD GENERAL DOWNLOAD TEMP FILE'                       
TA0C16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C16,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.  CURRENTLY, NOTHING NEEDS INITIALIZATION.                     
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE DEMO REQUEST         
* OBJECT PASSED BY THE PC AND RETURNS THE FIRST FRAME FULL OF DEMO              
* CELL OBJECTS.                                                                 
*                                                                               
PROCSTRT NTR1                                                                   
         XC    AFID,AFID           INITIALIZE UPLOAD FILE ID OVERRIDE           
*                                                                               
*                                  IF ACTION DOWNLOAD TEMP FILE                 
         CLC   PCACTION,=Y(ACDNLTMP)                                            
         BNE   PS50                                                             
*                                                                               
         GOTO1 GETITEM             FIRST FRAME CAN HAVE OVERRIDE                
         BNE   EXIT                    FILE NUMBER                              
*                                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME THEN DO NOTHING              
         BE    PS10                                                             
*                                  IF ITEM IS WORKER FILE NUMBER                
         CLC   TYPENUM,=A(ITWRKNUM)                                             
         BNE   PS10                                                             
*                                                                               
         L     R2,DATALEN          THEN SET WORKER FILE NUMBER                  
         GOTO1 HEXIN,DMCB,ADATA,HALF,4                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         MVC   WRKFILNO,HALF                                                    
*                                                                               
PS10     OC    WRKFILNO,WRKFILNO   TEST FOR ZERO FILE NUMBER                    
         BZ    ERRZEFN                                                          
         GOTO1 WRKLOC              LOCATE WORKER FILE                           
         BNE   EXIT                                                             
*                                                                               
         XC    TMPTYPE,TMPTYPE     CLEAR CONTINUE DATA                          
*                                                                               
         BAS   RE,FILLFRM          FILL FIRST FRAME WITH MAD OBJECTS            
         B     PSX                                                              
*                                                                               
PS50     GOTO1 GETITEM             GET FIRST ITEM                               
         BNE   EXIT                                                             
*                                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME THEN DO NOTHING              
         BE    PS60                                                             
*                                  IF ITEM IS WORKER FILE ID OVERRIDE           
         CLC   TYPENUM,=A(ITWRKFID)                                             
         BNE   PS60                                                             
         CLC   DATALEN,=F'7'       DATA LENGTH MUST BE 7 (ID(6) + TYPE)         
         BNE   ERRINV                                                           
*                                                                               
         L     RE,ADATA            MOVE DATA TO DUB AND SET AFID                
         MVC   DUB(7),0(RE)                                                     
         LA    RF,DUB                                                           
         ST    RF,AFID                                                          
         B     PS70                                                             
*                                                                               
PS60     MVC   AINPUT,AINFRM       DIDN'T FIND WORKER FILE NUMBER               
         MVC   INPLEFT,MDFRMSIZ        OBJECT SO REWIND GETITEM HERE            
         MVI   EIFFLAG,C'N'                                                     
*                                                                               
PS70     GOTO1 WRKCRE,DMCB,AFID    CREATE WORKER FILE                           
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,FILLTMP          FIRST FRAME OF OBJECTS TO TEMP FILE          
*                                                                               
         GOTO1 WRKCLOS             CLOSE WORKER FILE                            
*                                                                               
*                                  RETURN FILE NUMBER OBJECT TO PC              
         GOTO1 HEXOUT,DMCB,WRKFILNO,FULL,2                                      
         GOTO1 PUTITEM,DMCB,A(ITWRKNUM),4,FULL                                  
         BNE   EXIT                                                             
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT RETURNS THE NEXT FRAME FULL           
* OF MAD OBJECTS AND SETS THE LAST FRAME FLAG IF IT REACHES THE                 
* END OF THE TEMP FILE.                                                         
*                                                                               
PROCMID  NTR1                                                                   
*                                  IF ACTION DOWNLOAD TEMP FILE                 
         CLC   PCACTION,=Y(ACDNLTMP)                                            
         BNE   PM50                                                             
*                                                                               
         GOTO1 WRKREGT             REOPEN WORKER FILE FOR GETS                  
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME WITH MAD OBJECTS                  
         B     PMX                                                              
*                                                                               
PM50     GOTO1 WRKREPT             ELSE REOPEN WORKER FILE FOR PUTS             
*                                                                               
         BAS   RE,FILLTMP          FILL TEMP FILE WITH MAD OBJECTS              
*                                                                               
         GOTO1 WRKCLOS             CLOSE WORKER FILE                            
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE FILLS THE FRAME WITH MAD OBJECTS FROM THE TEMP FILE.             
*                                                                               
FILLFRM  NTR1                                                                   
         OC    TMPTYPE,TMPTYPE     IF CONTINUE TMPAREA EXISTS THEN CONT         
         BNZ   FF20                                                             
*                                  GET NEXT ITEM FROM TEMP FILE                 
FF10     GOTO1 WRKGET,DMCB,TMPHDR                                               
         BNE   EXIT                                                             
*                                  IF END OF TEMP FILE                          
         CLC   TMPHDR(8),=C'*EOFEOF*'                                           
         BNE   FF20                                                             
*                                                                               
         GOTO1 WRKSENT             MARK REPORT SENT                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR AND DONE            
         B     FF100                                                            
*                                                                               
FF20     ICM   R2,15,TMPTYPE       PUT MAD OBJECT TO OUTPUT                     
         LH    R3,TMPHDR                                                        
         SH    R3,=H'8'                                                         
         GOTO1 PUTITEM,DMCB,(R2),(R3),TMPDATA                                   
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF NOT END OF FRAME                          
         BNE   FF10                THEN GO BACK                                 
*                                                                               
FF100    DS    0H                                                               
*                                                                               
FFX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE FILLS THE TEMP FILE WITH OBJECTS FROM THE INPUT FRAME.           
*                                                                               
FILLTMP  NTR1                                                                   
*                                                                               
FT10     GOTO1 GETITEM             GET MAD OBJECT FROM SCREEN                   
         BNE   EXIT                                                             
*                                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME THEN DONE FOR                
         BE    FT90                    THIS TRANSACTION                         
*                                                                               
         MVC   TMPTYPE,TYPENUM     BUILD TEMP FILE RECORD WITH                  
         L     RE,ADATA                TYPE AND DATA FROM GETITEM               
         L     RF,DATALEN                                                       
         LA    R2,TMPDATA                                                       
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         L     R3,DATALEN          ADD RECORD TO TEMP FILE                      
         LA    R3,8(R3)                                                         
         XC    TMPHDR,TMPHDR                                                    
         STH   R3,TMPHDR                                                        
         GOTO1 WRKPUT,DMCB,TMPHDR                                               
         BNE   EXIT                                                             
         B     FT10                LOOP BACK                                    
*                                                                               
FT90     CLI   PCLAST,C'Y'         IF LAST PC FRAME                             
         BNE   *+8                                                              
         MVI   MDLAST,C'Y'         THEN THIS IS LAST TRANSACTION                
*                                                                               
FTX      B     XIT                                                              
         EJECT                                                                  
ERRINV   MVC   MDACTION,=Y(INVITEM)                                             
         B     EXIT                                                             
ERRZEFN  MVC   MDACTION,=Y(ER00ZEFN)                                            
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
AFID     DS    A                   A(FILE ID OVERRIDE) OF ZERO                  
TMPHDR   DS    F                   MVS RECORD HEADER (FROM WRKGET)              
TMPAREA  DS    0XL2000             AREA TO READ TEMP FILE RECORD                
TMPTYPE  DS    XL4                 MAD OBJECT TYPE                              
TMPDATA  DS    XL1996              MAD OBJECT DATA                              
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'229CTMAD16   05/01/02'                                      
         END                                                                    

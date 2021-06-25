*          DATA SET SPREPFXBYR AT LEVEL 043 AS OF 04/28/99                      
*PHASE SPFX02C                                                                  
*INCLUDE BINSRCH2                                                               
*        TITLE 'SPREPFXBYR - BUILD BUYER DATASET'                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET'                               
***********************************************************************         
*                                                                     *         
*        SPREPFXBYR - BUILD BUYER DATASET                             *         
*                                                                     *         
*              READ SPTDILE FOR ALL BUYER RECORDS                     *         
*              BUILD DATASET OF BUYERS AND THEIR CAMPAIGN             *         
*                AND SEQUENCE NUMBERS                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8,CLEAR=YES                                            
*                                                                               
         L     RA,0(R1)            ESTABLISH SPWORKD                            
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    FRST                                                             
*                                                                               
         CLI   MODE,REQFRST        FIRST FOR REQUEST                            
         BE    FX                                                               
*                                                                               
         CLI   MODE,RUNLAST        LAST FOR RUN                                 
         BE    LAST                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - FRST'                        
***********************************************************************         
*                                                                     *         
*        OPEN OUTPUT FILE                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FRST     DS    0H                                                               
*                                                                               
*        OPEN OUTPUT FILE                                                       
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))  OPEN OUTFILE                                 
*                                                                               
FRSTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - FX'                          
***********************************************************************         
*                                                                     *         
*        OUTPUT A FILE CONTAINING ALL BUYERS ON FILE                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FX       DS    0H                                                               
*                                                                               
*        BUILD STARTING BUYER KEY                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING BYRRECD,R5                                                       
*                                                                               
         MVI   BYRKTYP,X'0D'       RECORD TYPE                                  
         MVI   BYRKSUB,X'65'       RECORD SUBTYPE                               
         MVC   BYRKAGMD,BAGYMD     SET AGENCY/MEDIA                             
*                                                                               
         LA    R6,BYORECC          ESTABLISH RECORD FOR CONVERSION FILE         
         USING BYOREC,R6                                                        
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD.                           
*                                                                               
FXBYRLP  DS    0H                                                               
*                                                                               
         LA    R5,KEY              POINT TO NEXT KEY                            
*                                                                               
         CLC   BYRKTYP(BYRKBYR-BYRKEY),KEYSAVE DONE ON BREAK IN                 
         BNE   FXBYRDN             AGENCY/MEDIA                                 
*                                                                               
         GOTO1 GETBUY              READ IN RECORD                               
*                                                                               
         L     R5,ADBUY            POINT TO FOUND RECORD                        
*                                                                               
*        BUILD CONVERSION FILE RECORD                                           
*                                                                               
         MVC   BYOREC(BYORECLQ),SPACES INIT CONVERSION FILE RECORD              
*                                                                               
         MVC   BYOKEY,BYRKEY       SAVE KEY                                     
         MVC   BYOBYRCD,BYRCODE    SAVE BUYER CODE                              
         MVC   BYOBYRNM,BYRNAME    SAVE BUYER CODE                              
*                                                                               
         MVC   BYOAGYMD,BYRKAGMD     AGENCY/MEDIA                               
*                                                                               
*        PRINT RECORD                                                           
*                                                                               
         LA    R1,P1               ESTABLISH PRINT LINE                         
         USING PLINED,R1                                                        
*                                                                               
         UNPK  WORK(15),BYRKEY(8)       PRINT KEY                               
         UNPK  WORK+14(13),BYRKEY+7(6)  PRINT KEY                               
         TR    WORK(26),HEXTAB-C'0'                                             
         MVC   PKEY,WORK                                                        
*                                                                               
         UNPK  WORK(3),BYRKAGMD(2)  PRINT AGY/MED                               
         TR    WORK(2),HEXTAB-C'0'                                              
         MVC   PAGYMED,WORK                                                     
*                                                                               
         MVC   PBYR,BYRKBYR        BUYER                                        
*                                                                               
         UNPK  WORK(3),BYRCODE(2)  PRINT BUYER CODE                             
         TR    WORK(2),HEXTAB-C'0'                                              
         MVC   PBYRCD,WORK                                                      
*                                                                               
         MVC   PBYRNM,BYRNAME      BUYER NAME                                   
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
*        WRITE RECORD                                                           
*                                                                               
         PUT   FILEOUT,BYOREC      ADD TO CONVERSION FILE                       
*                                                                               
FXBYRCN DS     0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT STATION ON FILE                    
*                                                                               
         B     FXBYRLP                                                          
*                                                                               
FXBYRDN  DS    0H                                                               
*                                                                               
         GOTO1 AENDREQ             STOP REQUEST                                 
*                                                                               
FXX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - LAST'                        
***********************************************************************         
*                                                                     *         
*        CLOSE OUTPUT FILE                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LAST     DS    0H                                                               
*                                                                               
         CLOSE FILEOUT             CLOSE CONVERSION FILE                        
*                                                                               
LASTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - FILEOUT'                     
***********************************************************************         
*                                                                     *         
*        DCB FOR OUTPUT DATASET CONTAINING BUYER INFORMATION          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILEOUT  DCB   DDNAME=BYROUT,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF' HEX CONVERSION TABLE                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - WORKAREAS'                   
***********************************************************************         
*                                                                     *         
*        WORKAREAS                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SAVEKEY  DS    XL32                                                             
BYORECC  DS    XL(BYORECLQ)        CONVERSION RECORD BUILDAREA                  
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - BYORECD'                     
**********************************************************************          
*                                                                               
*        DSECT FOR BUYER OUTPUT DATASET                                         
*                                                                               
**********************************************************************          
         SPACE 2                                                                
BYORECD  DSECT                                                                  
BYOREC   DS    0XL1                CONVERSION RECORD                            
BYOKEY   DS    XL(L'BYRKEY)        KEY FOR STATION RECORD                       
BYOBYRCD DS    XL(L'BYRCODE)       BUYER CODE                                   
BYOBYRNM DS    XL(L'BYRNAME)       BUYER NAME                                   
BYOAGYMD DS    XL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYORECD))  SPARE                                        
BYORECLQ EQU   *-BYOREC            RECORD LENGTH                                
*                                                                               
         TITLE 'SPREPFXBYR - RENUMBER CANADIAN STATIONS - PLINED'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR PRINT LINE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PLINED   DSECT                                                                  
PKEY     DS    XL26                KEY                                          
         DS    XL2                                                              
PAGYMED  DS    XL2                 AGENCY MEDIA BYTE                            
         DS    XL2                                                              
PBYR     DS    XL3                 BUYER                                        
         DS    XL2                                                              
PBYRCD   DS    XL2                 BUYER CODE                                   
         DS    XL2                                                              
PBYRNM   DS    CL20                BUYER NAME                                   
         DS    CL2                                                              
*                                                                               
         TITLE 'SPREPFXBYR - BUILD BUYER DATASET - BYRRECD'                     
**********************************************************************          
*                                                                               
*        FIXED-RECORDS DSECT                                                    
*                                                                               
**********************************************************************          
         SPACE 2                                                                
       ++INCLUDE SPNWSBYR                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPREPFXBYR04/28/99'                                      
         END                                                                    

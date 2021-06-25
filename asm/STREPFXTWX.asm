*          DATA SET STREPFXTWX AT LEVEL 026 AS OF 10/17/96                      
*PHASE SPFX02U                                                                  
*                                                                               
         TITLE 'STREPFXTWX  GET TWIX NUMBER FOR WILO'                           
***********************************************************************         
*                                                                               
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8                                                      
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
RQF      DS    0H                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         MVI   KEY,C'S'            STATION RECORDS ONLY                         
*                                                                               
RQF5     GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     RQF20                                                            
*                                                                               
RQF10    GOTO1 SEQSTA                                                           
*                                                                               
RQF20    DS    0H                                                               
         L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
         USING STARECD,R5                                                       
         L     R1,READCNT                                                       
         LA    R1,1(R1)            INCREMENT RECORDS READ.                      
         ST    R1,READCNT                                                       
*                                                                               
         CLI   STAKTYPE,C'S'       IS IT A STATION RECORD?                      
         BNE   RQF100                                                           
*                                                                               
         CLI   STAKMED,C'R'        RADIO?                                       
         BE    *+12                YES                                          
         CLI   STAKMED,C'T'        TELEVISION?                                  
         BNE   RQF10               NO                                           
*                                                                               
         CLC   STAKAGY,=C'WG'      AGENCY WG?                                   
         BE    RQF30               YES                                          
         CLC   STAKAGY,=C'WI'      AGENCY WI?                                   
         BE    RQF30               YES                                          
         CLC   STAKAGY,=C'WJ'      AGENCY WJ?                                   
         BE    RQF30               YES                                          
         CLC   STAKAGY,=C'WR'      AGENCY WR?                                   
         BNE   RQF10               YES                                          
*                                                                               
RQF30    CLC   STAKCLT,=C'000'     ANY CLIENT CODE?                             
         BNE   RQF10                                                            
         CLI   STWIX,C' '          ANY TWIX NUMBER?                             
         BNH   RQF10                                                            
*                                                                               
RQF40    DS    0H                                                               
         XC    REC,REC                                                          
         LA    R3,REC                                                           
         USING FILEOUTD,R3                                                      
         MVC   MEDFO,STAKMED       MEDIA                                        
         MVC   CALL,STAKCALL       CALL LETTERS                                 
         MVC   AGYFO,STAKAGY       AGENCY CODE                                  
         MVC   TWIX,STWIX          TWIX NUMBER                                  
         CLC   TEMPREC,REC         CURRENT REC = LAST REC?                      
         BE    RQF10               YES                                          
         PUT   FILEOUT,REC                                                      
         MVC   TEMPREC,REC                                                      
         B     RQF10                                                            
********************  PROCESSING END, SHOW QUOTAS  ********************         
RQF100   DS    0H                                                               
         CLOSE FILEOUT                                                          
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
**************************** MISCELLANEOUS ****************************         
         DS    0D                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,LRECL=28,MACRF=PM               
INVLCNT  DS    F                   # OF INVALID S RECORDS                       
READCNT  DS    F                   # OF RECORDS READ.                           
*                                                                               
MYWORK   DS    CL17                                                             
SAVEKEY  DS    XL15                                                             
         DS    0F                                                               
STAWORK  DS    XL31                STAPACK DSECT                                
REC      DS    CL28                NEW RECORD WITH TWIX (FILEOUT)               
TEMPREC  DS    CL8                 TEMPREC WITH TWIX (FILEOUT)                  
         EJECT                                                                  
************************* FIXED-RECORDS DSECT *************************         
       ++INCLUDE SPGENMSTA                                                      
         EJECT                                                                  
       ++INCLUDE SPGENANMK                                                      
         EJECT                                                                  
************************** REP-RECORDS DSECT **************************         
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
FILEOUTD DSECT                                                                  
MEDFO    DS    CL1                 MEDIA                                        
CALL     DS    CL5                 CALL LETTERS                                 
AGYFO    DS    CL2                 AGENCY CODE                                  
TWIX     DS    CL20                TWIX NUMBER                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREPT                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026STREPFXTWX10/17/96'                                      
         END                                                                    

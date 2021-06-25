*          DATA SET SPREPFXHST AT LEVEL 001 AS OF 04/17/97                      
*PHASE SPFX02C                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPFX02 - FIX HST TAXES ON CANADIAN STATIONS'                    
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   *-4000                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* REQF - CLEAR RECORD COUNTER                                                   
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         EJECT                                                                  
*                                                                               
* -  GO THROUGH STATION MASTER RECORDS                                          
*                                                                               
         XC    KEY,KEY            CLEAR KEY                                     
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'      STATION RECORD                                
*                                                                               
         GOTO1 HIGHSTA                                                          
         L     R3,ADSTAT          ADSTAT POINTS TO RECORD FOUND                 
         B     REQL20                                                           
*                                                                               
REQL10   DS    0H                                                               
         GOTO1 SEQSTA             GET NEXT RECORD                               
         L     R3,ADSTAT          ADSTAT POINTS TO RECORD FOUND                 
*                                                                               
REQL20   DS    0H                                                               
*                                                                               
         XC    KEY,KEY             KEEP LATEST KEY                              
         MVC   KEY(15),0(R3)                                                    
*                                                                               
         CLI   STAKTYPE,C'S'                                                    
         BNE   REQL90              MAKE SURE STILL C'S'                         
*                                                                               
         CLI   STAKMED,C'T'        SPOT TV?                                     
         BE    REQL40                                                           
         CLI   STAKMED,C'R'        SPOT RADIO?                                  
         BE    REQL40                                                           
         CLI   STAKMED,C'N'        NETWORK TV?                                  
         BE    REQL40                                                           
         B     REQL10                                                           
*                                                                               
REQL40   DS    0H                  RECORD IS ALSO CANADIAN                      
         CLI   SPST+6,0            NEW BRUNSWICK?                               
         BNE   REQL50                                                           
         CLI   SPST+7,0            NOVA SOCTIA?                                 
         BNE   REQL50                                                           
         CLI   SPST+9,0            NEWFOUNDLAND?                                
         BNE   REQL50                                                           
         B     REQL10                                                           
*                                                                               
REQL50   DS    0H                  FOUND A RECORD TO FIX!!                      
         XC    REC,REC             KEY FIELDS OF RECORD TO FILEOUT              
         LA    R4,REC                                                           
         USING FILEOUTD,R4                                                      
*                                                                               
         MVC   RECAGY,STAKAGY      AGENCY                                       
         MVC   RECMED,STAKMED      MEDIA                                        
         MVC   RECCALL,STAKCALL    CALL LETTERS                                 
         MVC   RECPST,SPST         PST CODES                                    
*                                                                               
         MVI   BYTE,0                                                           
         LA    R1,RECPST                                                        
         LA    R0,10                                                            
REQL52   CLI   0(R1),C' '                                                       
         BNH   REQL54                                                           
         CLI   BYTE,0                                                           
         BNE   MULTPST                                                          
         MVI   BYTE,1                                                           
REQL54   LA    R1,1(R1)                                                         
         BCT   R0,REQL52                                                        
         B     REQL58                                                           
*                                                                               
MULTPST  MVC   P(120),0(R3)        PRINT OUT BAD PST DATA                       
         GOTO1 REPORT                                                           
* AND THEN STAGGER ON                                                           
REQL58   CLC   =C'000',STAKCLT                                                  
         BE    REQL60                                                           
         GOTO1 CLPACK,DMCB,STAKCLT,RECCLT      CLIENT                           
*                                                                               
REQL60   XC    STAWORK,STAWORK     BLOCK FOR STAPACK                            
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'        PACK                                         
         MVC   STAPAGY,STAKAGY     AGENCY                                       
         MVI   STAPCTRY,C'C'       CANADA                                       
         MVC   STAPMED,STAKMED     MEDIA                                        
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'   MARKET                                       
         MVC   STAPQSTA,STAKCALL   CALL LETTERS                                 
         MVC   STAPQNET,=C'   '    NETWORK                                      
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   RECSEQN,STAPSTA     SEQUENCE #                                   
*                                                                               
         CLI   STAKMED,C'R'        SPOT RADIO?                                  
         BE    *+8                                                              
         MVI   RECSEQN+2,0                                                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
*                                                                               
         GOTO1 HIGHSTA                                                          
         B     REQL10                                                           
*                                                                               
REQL90   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4    ANY MORE RECS' TO GET?                       
         BZ    REQLX               NO                                           
         XC    REC,REC                                                          
         L     R5,DMCB+4           A(SORTED REC)                                
         MVC   REC,0(R5)                                                        
         PUT   FILEOUT,REC                                                      
         B     REQL90                                                           
*                                                                               
REQLX    DS    0H                                                               
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AENDREQ                                                          
         DC    H'0'                NO RETURN EXPECTED HERER                     
         DROP  R1,R3                                                            
*                                                                               
*SORTCARD DC    CL80'SORT FIELDS=(1,3,A,9,5,A,4,2,A),FORMAT=BI,WORK=1'          
SORTCARD DC    CL80'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=23'                                    
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=23,BLKSIZE=23000                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
         LTORG                                                                  
REC      DS    CL23                KEY FIELDS OF RECORD TO FIX                  
SAVEKEY  DS    XL15                                                             
*                                                                               
         DS    0A                                                               
STAWORK  DS    XL32                PARAMETER BLOCK FOR STAPACK                  
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
FILEOUTD DSECT                                                                  
RECAGY   DS    XL2                 AGENCY                                       
RECMED   DS    XL1                 MEDIA                                        
RECCLT   DS    XL2                 CLIENT                                       
RECSEQN  DS    XL3                 SEQUENCE #, 2 BYTES FOR TV/N -               
*                                  3 BYTES FOR R                                
RECCALL  DS    CL5                 STATION CALL LETTERS                         
RECPST   DS    CL10                PST CODES                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFXHST04/17/97'                                      
         END                                                                    

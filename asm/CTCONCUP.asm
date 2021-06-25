*          DATA SET CTCONCUP   AT LEVEL 003 AS OF 02/24/98                      
*PHASE CONCUP,*                                                                 
*INCLUDE LOADER                                                                 
***********************************************************************         
* MODULE CALLED AS EXTERN TO CONCRETE TO DELETE UNKNOWN USER PROFILES *         
***********************************************************************         
         TITLE 'CTCONCUP - LIST USER PROFILE ID'S                               
CONCUP   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONCUP*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVI   DATADISP+1,28                                                    
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         OC    LOCBUFF,LOCBUFF     DONE GETMAIN FOR BUFFER                      
         BNZ   UP02                                                             
         ICM   R0,LOCLEN           LENGTH OF BUFFER SET HERE                    
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R1,LOCBUFF          R1 HOLDS BUFFER ADDRESS                      
         ST    R1,LOCLEN           R0 HOLDS BUFFER LENGTH                       
*                                                                               
         GOTO1 =V(LOADER),PLIST,=CL8'T00A7D'                                    
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR            LOAD TSAROFF                                 
*                                                                               
         USING TSARD,TSBUFF                                                     
         XC    TSBUFF,TSBUFF                                                    
         MVI   TSKEYL,3            SET LENGTH OF KEY                            
         MVC   TSRECL,3            SET MAX REC LENGTH                           
         MVC   TSABUF,LOCBUFF      SET A(BUFFER)                                
         MVC   TSAREC,LOCLEN       SET LENGTH OF BUFFER                         
         MVI   TSOFFACT,TSAINI     SET INITIALISE                               
         OI    TSIND2,TSI2MANY     SET FOR MANY RECORDS                         
         GOTO1 ATSAR                                                            
*                                                                               
         USING CTUREC,R2                                                        
UP02     TM    CTUSTAT,X'80'       DELETED?                                     
         BO    EXIT                YES                                          
         CLI   CTUKTYP,CTUKTYPQ    USER PROFILE RECORD                          
         BE    ?????                                                            
         CLI   CTUKTYP,CT5KTYPQ    ACCESS RECORD                                
         BE    ADDACC                                                           
         CLI   CTUKTYP,CTIKTYPQ    ID RECORD                                    
         BE    ADDID                                                            
         B     EXIT                                                             
*                                                                               
ADDACC   XC    TSREC,TSREC                                                      
         MVI   TSREC,C'A'                                                       
         MVC   TSREC+1(2),CT5KALPH                                              
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSADD                                                   
         GOTO1 ATSAR                                                            
         CLI   TSERRS,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
ADDID    OC    CTIKID(8),CTIKID    ONLY ID NUMBERS                              
         BNZ   EXIT                                                             
         XC    TSREC,TSREC                                                      
         MVI   TSREC,C'I'                                                       
         MVC   TSREC+1(2),CTIKNUM                                               
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSADD                                                   
         GOTO1 ATSAR                                                            
         CLI   TSERRS,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
ADDID                                                                           
* SKIP HIGH ID/TERM REC                                                         
         OC    1(24,R2),1(R2)                                                   
         BZ    EXIT                                                             
*                                                                               
         CLI   0(R2),C'T'          TERM REC?                                    
         BE    M1A                                                              
         CLI   0(R2),C'I'          ID REC?                                      
         BNE   EXIT                                                             
*                                                                               
* GET 2-CHAR AGY CODE FROM ID REC                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'06',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
**NOP    BE    *+6                                                              
**NOP    DC    H'0'                                                             
         BNE   EXIT                                                             
         L     R3,DMCB+12                                                       
         USING CTAGYD,R3                                                        
         LA    RF,AGYTABLE                                                      
*                                                                               
* SEE IF THIS IS ONE WE WANT                                                    
         CLC   CTAGYID,0(RF)                                                    
         BE    M24                                                              
         LA    RF,L'AGYTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-18                                                             
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
         USING CTTREC,R2                                                        
M1A      TM    27(R2),X'04'        PRINTER OR SHUTTLE?                          
         BNZ   EXIT                LEAVE IT ALONE                               
*                                                                               
         OC    CTTKTID,CTTKTID     THIS A PASSIVE?                              
         BZ    M14                  YES                                         
         OC    CTTKPASS,CTTKPASS   PASSWORD REC?                                
         BNZ   EXIT                 YES - SKIP                                  
         LA    R3,CTTKTID                                                       
         B     M16                                                              
*                                                                               
M14      DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'03',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,DMCB+12                                                       
         CLI   1(R3),X'0A'         PASSWORD REC?                                
         BH    EXIT                 YES - SKIP                                  
         LA    R3,2(R3)                                                         
         B     M16                                                              
*                                                                               
M16      LA    RF,LUIDLIST                                                      
         CLC   0(L'LUIDLIST,RF),0(R3)    LISTED TERMINAL?                       
         BE    M24                                                              
         LA    RF,L'LUIDLIST(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-18                                                             
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
M24      BAS   RE,CHAID                                                         
* PRINT OUT CHANGES                                                             
         TM    FLAGS,IDADDED       ANY CHANGES?                                 
         BZ    EXIT                 NO                                          
         CLI   0(R2),C'T'          TERM REC?                                    
         BNE   M52                                                              
         USING CTTREC,R2                                                        
         LA    R3,P                                                             
         MVC   0(08,R3),=C'TERMINAL'                                            
         LA    R3,9(R3)                                                         
         OC    CTTKTID,CTTKTID                                                  
         BNZ   M30                                                              
         GOTO1 VHEXOUT,DMCB,23(R2),0(R3),2,=C'TOG'                              
         LA    R3,5(R3)                                                         
         B     M40                                                              
*                                                                               
M30      MVC   0(08,R3),CTTKTID                                                 
         LA    R3,9(R3)                                                         
         OC    CTTKPASS,CTTKPASS   ANY PASSWORD?                                
         BZ    *+14                                                             
         MVC   0(10,R3),CTTKPASS                                                
         LA    R3,11(R3)                                                        
         DROP  R2                                                               
*                                                                               
M40      MVC   0(14,R3),=C'HAD ID''S ADDED'                                     
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
         USING CTIREC,R2                                                        
M52      DS    0H                  PRINT OUT CHANGED ID'S                       
         LA    R3,P                                                             
         MVC   0(02,R3),=C'ID'                                                  
         LA    R3,3(R3)                                                         
         OC    CTIKID(8),CTIKID                                                 
         BNZ   M60                                                              
         GOTO1 VHEXOUT,DMCB,23(R2),0(R3),2,=C'TOG'                              
         LA    R3,5(R3)                                                         
         B     M40                                                              
M60      MVC   0(10,R3),CTIKID                                                  
         LA    R3,11(R3)                                                        
         B     M40                                                              
         DROP  R2                                                               
*                                                                               
DONE     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHAID    NTR1                                                                   
         NI    FLAGS,X'FF'-IDADDED                                              
         SPACE                                                                  
* REMOVE ALL X'20' ELEMS                                                        
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'20',(R2))                       
         SPACE                                                                  
* ADD NEW X'20' ELEMS                                                           
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'200C'                                                 
         LA    R5,IDLIST                                                        
*                                                                               
CI20     TM    0(R5),X'80'         THIS AN ID LIST?                             
         BZ    CI30                 NO                                          
         XC    ELEM+2(2),ELEM+2                                                 
         MVC   ELEM+4(8),1(R5)                                                  
         B     *+10                                                             
*                                                                               
CI30     MVC   ELEM+2(10),1(R5)                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'20',(R2)),ELEM,        X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,5           REC TOO LARGE?                               
         BE    PRNERR                                                           
         CLI   DMCB+12,0           ANY OTHER ERRORS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,L'IDLIST(R5)                                                  
         CLI   0(R5),X'FF'         EOT?                                         
         BNZ   CI20                 NO                                          
         SPACE                                                                  
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         SPACE                                                                  
         CLC   =H'999',25(R2)      REC >= 999 BYTES                             
         BNH   PRNERR               YES                                         
         ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
         OI    FLAGS,IDADDED                                                    
         B     EXIT                                                             
         EJECT                                                                  
* PRINT ERROR RECS *                                                            
         SPACE                                                                  
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VARIOUS TEST CODE                                                             
*                                                                               
*         MVC   P(25),0(R2)                                                     
*         GOTO1 VPRINTER                                                        
*         GOTO1 VHEXOUT,DMCB,0(R6),P,60,=C'TOG'                                 
*         GOTO1 VPRINTER                                                        
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
         ANSR  X=N                                                              
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PLIST    DS    6F                                                               
*                                                                               
ATSAR    DS    A                   A(TSAROFF)                                   
LOCBUFF  DC    F'0'                A(GETMAIN BUFFER)                            
LOCLEN   DC    A(1024*1024)        L'GETMAIN BUFFER                             
*                                                                               
         DS    0D                                                               
         DC    CL8'TSARBUFF'                                                    
TSBUFF   DS    AL(TSARDL)          TSAR BUFFER                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'TSARRECD'                                                    
TSREC    DS    CL16                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTCONCUP  02/24/98'                                      
         END                                                                    
